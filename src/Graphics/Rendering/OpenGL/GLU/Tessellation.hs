--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.Tessellation
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to chapter 5 (Polygon Tessellation) of the GLU specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GLU.Tessellation (
   -- * Polygon description
   AnnotatedVertex(..), ComplexContour(..), ComplexPolygon(..),

   -- * Combining vertices
   WeightedProperties(..), Combiner,

   -- * Tessellation parameters
   TessWinding(..), Tolerance,

   -- * Tessellator type
   Tessellator,

   -- * Contour extraction
   SimpleContour(..), PolygonContours(..), extractContours,

   -- * Triangulation
   TriangleVertex, Triangle(..), Triangulation(..), triangulate,

   -- * Tessellation into primitives
   Primitive(..), SimplePolygon(..), tessellate
) where

import Control.Monad ( foldM_, unless )
import Data.IORef ( newIORef, readIORef, writeIORef, modifyIORef )
import Data.Maybe ( fromJust, fromMaybe )
import Foreign.Marshal.Alloc ( allocaBytes )
import Foreign.Marshal.Array ( peekArray, pokeArray )
import Foreign.Marshal.Pool ( Pool, withPool, pooledNew )
import Foreign.Ptr ( Ptr, nullPtr, plusPtr, castPtr, freeHaskellFunPtr )
import Foreign.Storable ( Storable(..) )
import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL.GL.Tensor
import Graphics.Rendering.OpenGL.GL.EdgeFlag ( unmarshalEdgeFlag )
import Graphics.Rendering.OpenGL.GL.Exception ( bracket )
import Graphics.Rendering.OpenGL.GL.GLboolean ( marshalGLboolean )
import Graphics.Rendering.OpenGL.GL.PrimitiveMode ( PrimitiveMode )
import Graphics.Rendering.OpenGL.GL.PrimitiveModeInternal ( unmarshalPrimitiveMode )
import Graphics.Rendering.OpenGL.GL.BeginEnd ( EdgeFlag(BeginsInteriorEdge) )
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

data TessWinding =
     TessWindingOdd
   | TessWindingNonzero
   | TessWindingPositive
   | TessWindingNegative
   | TessWindingAbsGeqTwo
   deriving ( Eq, Ord, Show )

marshalTessWinding :: TessWinding -> GLenum
marshalTessWinding x = case x of
   TessWindingOdd -> glu_TESS_WINDING_ODD
   TessWindingNonzero -> glu_TESS_WINDING_NONZERO
   TessWindingPositive -> glu_TESS_WINDING_POSITIVE
   TessWindingNegative -> glu_TESS_WINDING_NEGATIVE
   TessWindingAbsGeqTwo -> glu_TESS_WINDING_ABS_GEQ_TWO

--------------------------------------------------------------------------------

-- | The basic building block in tessellation is a 3D vertex with an associated
-- property, e.g. color, texture coordinates, etc.

data AnnotatedVertex v = AnnotatedVertex (Vertex3 GLdouble) v
   deriving ( Eq, Ord )

offsetOfProperty :: Storable v => v -> Int
offsetOfProperty v = alignOffset v (3 * sizeOf x)
   where AnnotatedVertex (Vertex3 x _ _) _ = undefined

alignOffset :: Storable a => a -> Int -> Int
alignOffset x offset = n - (n `mod` a)
   where a = alignment x
         n = a + offset - 1

instance Storable v => Storable (AnnotatedVertex v) where

   sizeOf ~(AnnotatedVertex (Vertex3 x _ _) v) =
      alignOffset x (sizeOf v + offsetOfProperty v)

   alignment ~(AnnotatedVertex (Vertex3 x _ _) _) =
      alignment x

   peek ptr = do
      x <- peekElemOff (castPtr ptr) 0
      y <- peekElemOff (castPtr ptr) 1
      z <- peekElemOff (castPtr ptr) 2
      let dummyElement :: Ptr (AnnotatedVertex v) -> v
          dummyElement = undefined
      v <- peekByteOff (castPtr ptr) (offsetOfProperty (dummyElement ptr))
      return $ AnnotatedVertex (Vertex3 x y z) v

   poke ptr (AnnotatedVertex (Vertex3 x y z) v) = do
      pokeElemOff (castPtr ptr) 0 x
      pokeElemOff (castPtr ptr) 1 y
      pokeElemOff (castPtr ptr) 2 z
      pokeByteOff (castPtr ptr) (offsetOfProperty v) v

--------------------------------------------------------------------------------

-- | A complex contour, which can be self-intersecting and\/or concave.

newtype ComplexContour v = ComplexContour [AnnotatedVertex v]
   deriving ( Eq, Ord )

sizeOfComplexContour :: Storable v => ComplexContour v -> Int
sizeOfComplexContour (ComplexContour vs) =
   length vs * sizeOf (head vs)

pokeComplexContour ::
   Storable v => Ptr (ComplexContour v) -> ComplexContour v -> IO ()
pokeComplexContour ptr (ComplexContour vs) =
   pokeArray (castPtr ptr) vs

--------------------------------------------------------------------------------

-- | A complex (possibly concave) polygon, represented by one or more complex
-- and possibly intersecting contours.

newtype ComplexPolygon v = ComplexPolygon [ComplexContour v]
   deriving ( Eq, Ord )

sizeOfComplexPolygon :: Storable v => ComplexPolygon v -> Int
sizeOfComplexPolygon (ComplexPolygon complexContours) =
   sum (map sizeOfComplexContour complexContours)

pokeComplexPolygon ::
   Storable v => Ptr (ComplexPolygon v) -> ComplexPolygon v -> IO ()
pokeComplexPolygon ptr (ComplexPolygon complexContours) =
   foldM_ pokeAndAdvance (castPtr ptr) complexContours >> return ()
   where pokeAndAdvance p complexContour = do
            pokeComplexContour p complexContour
            return $ p `plusPtr` sizeOfComplexContour complexContour

withComplexPolygon ::
   Storable v => ComplexPolygon v -> (Ptr (ComplexPolygon v) -> IO a) -> IO a
withComplexPolygon complexPolygon f =
   allocaBytes (sizeOfComplexPolygon complexPolygon) $ \ptr -> do
      pokeComplexPolygon ptr complexPolygon
      f ptr

--------------------------------------------------------------------------------

-- | Four vertex properties (cf. 'AnnotatedVertex') with associated weigths
-- summing up to 1.0.

data WeightedProperties v
   = WeightedProperties (GLfloat, v)
                        (GLfloat, v)
                        (GLfloat, v)
                        (GLfloat, v)
   deriving ( Eq, Ord )

-- | A function combining given vertex properties into a property for a newly
-- generated vertex

type Combiner v
    = Vertex3 GLdouble
   -> WeightedProperties v
   -> v

--------------------------------------------------------------------------------

-- | The relative tolerance under which two vertices can be combined (see
-- 'Combiner'). Multiplication with the largest coordinate magnitude of all
-- polygon vertices yields the maximum distance between two mergeable vertices.
--
-- Note that merging is optional and the tolerance is only a hint.

type Tolerance = GLdouble

--------------------------------------------------------------------------------

-- | A general tessellator type.
--
-- Before tessellation of a complex polygon, all its vertices are projected into
-- a plane perpendicular to the given normal. If the given normal is
-- @Normal3 0 0 0@, a fitting plane of all vertices is used.

type Tessellator p v
   = TessWinding
  -> Tolerance
  -> Normal3 GLdouble
  -> Combiner v
  -> ComplexPolygon v
  -> IO (p v)

--------------------------------------------------------------------------------

-- | A simple, non-self-intersecting contour

newtype SimpleContour v = SimpleContour [AnnotatedVertex v]
   deriving ( Eq, Ord )

-- | The contours of a complex polygon, represented by one or more
-- non-intersecting simple contours

newtype PolygonContours v = PolygonContours [SimpleContour v]
   deriving ( Eq, Ord )

extractContours :: Storable v => Tessellator PolygonContours v
extractContours windingRule tolerance theNormal combiner complexPoly = do

   vertices <- newIORef []
   let addVertex v = modifyIORef vertices (v:)

   contours <- newIORef []
   let finishContour = do
          vs <- readIORef vertices
          writeIORef vertices []
          modifyIORef contours (SimpleContour (reverse vs) :)

       getContours = fmap (PolygonContours . reverse) (readIORef contours)

   withTessellatorObj (PolygonContours [])$ \tessObj -> do
      setTessellatorProperties tessObj windingRule tolerance theNormal True
      withVertexCallback tessObj addVertex $
         withEndCallback tessObj finishContour $
            checkForError tessObj $
               withCombineCallback tessObj combiner $ do
                  defineComplexPolygon tessObj complexPoly
                  getContours

--------------------------------------------------------------------------------

-- | A triangle vertex with additional information about the edge it begins

type TriangleVertex v = AnnotatedVertex (v,EdgeFlag)

-- | A triangle, represented by three triangle vertices

data Triangle v
   = Triangle (TriangleVertex v) (TriangleVertex v) (TriangleVertex v)
   deriving ( Eq, Ord )

-- | A triangulation of a complex polygon

newtype Triangulation v = Triangulation [Triangle v]
   deriving ( Eq, Ord )

triangulate :: Storable v => Tessellator Triangulation v
triangulate windingRule tolerance theNormal combiner complexPoly = do

   edgeFlagState <- newIORef BeginsInteriorEdge
   let registerEdgeFlag = writeIORef edgeFlagState

   vertices <- newIORef []
   let addVertex (AnnotatedVertex xyz v) = do
          ef <- readIORef edgeFlagState
          modifyIORef vertices (AnnotatedVertex xyz (v,ef) :)

       getTriangulation = do
          vs <- readIORef vertices
          return $ Triangulation (collectTriangles (reverse vs))

   withTessellatorObj (Triangulation []) $ \tessObj -> do
      setTessellatorProperties tessObj windingRule tolerance theNormal False
      withEdgeFlagCallback tessObj registerEdgeFlag $
         withVertexCallback tessObj addVertex $
            checkForError tessObj $
               withCombineCallback tessObj combiner $ do
                  defineComplexPolygon tessObj complexPoly
                  getTriangulation

collectTriangles :: [TriangleVertex v] -> [Triangle v]
collectTriangles []           = []
collectTriangles (a:b:c:rest) = Triangle a b c : collectTriangles rest
collectTriangles _            = error "triangles left"

--------------------------------------------------------------------------------

data Primitive v = Primitive PrimitiveMode [AnnotatedVertex v]
   deriving ( Eq, Ord )

newtype SimplePolygon v = SimplePolygon [Primitive v]
   deriving ( Eq, Ord )

tessellate :: Storable v => Tessellator SimplePolygon v
tessellate windingRule tolerance theNormal combiner complexPoly = do

   beginModeState <- newIORef undefined
   let setPrimitiveMode = writeIORef beginModeState

   vertices <- newIORef []
   let addVertex v = modifyIORef vertices (v:)

   primitives <- newIORef []
   let finishPrimitive = do
          beginMode <- readIORef beginModeState
          vs <- readIORef vertices
          writeIORef vertices []
          modifyIORef primitives (Primitive beginMode (reverse vs) :)

       getSimplePolygon = fmap (SimplePolygon . reverse) (readIORef primitives)

   withTessellatorObj (SimplePolygon []) $ \tessObj -> do
      setTessellatorProperties tessObj windingRule tolerance theNormal False
      withBeginCallback tessObj setPrimitiveMode $
         withVertexCallback tessObj addVertex $
            withEndCallback tessObj finishPrimitive $
               checkForError tessObj $
                  withCombineCallback tessObj combiner $ do
                     defineComplexPolygon tessObj complexPoly
                     getSimplePolygon

--------------------------------------------------------------------------------
-- chapter 5.1: The Tessellation Object

-- an opaque pointer to a tessellator object
type TessellatorObj = Ptr GLUtesselator

isNullTesselatorObj :: TessellatorObj -> Bool
isNullTesselatorObj = (nullPtr ==)

withTessellatorObj :: a -> (TessellatorObj -> IO a) -> IO a
withTessellatorObj failureValue action =
   bracket gluNewTess safeDeleteTess
           (\tessObj -> if isNullTesselatorObj tessObj
                           then do recordOutOfMemory
                                   return failureValue
                           else action tessObj)

safeDeleteTess :: TessellatorObj -> IO ()
safeDeleteTess tessObj =
   unless (isNullTesselatorObj tessObj) $ gluDeleteTess tessObj

--------------------------------------------------------------------------------
-- chapter 5.2: Polygon Definition (polygons)

defineComplexPolygon ::
   Storable v => TessellatorObj -> ComplexPolygon v -> IO ()
defineComplexPolygon tessObj cp@(ComplexPolygon complexContours) =
   withComplexPolygon cp $ \ptr ->
      tessBeginEndPolygon tessObj nullPtr $
         let loop _ []     = return ()
             loop p (c:cs) = do defineComplexContour tessObj (castPtr p) c
                                loop (p `plusPtr` sizeOfComplexContour c) cs
         in loop ptr complexContours

tessBeginEndPolygon :: TessellatorObj -> Ptr p -> IO a -> IO a
tessBeginEndPolygon tessObj ptr f = do
   gluTessBeginPolygon tessObj ptr
   res <- f
   gluTessEndPolygon tessObj
   return res

--------------------------------------------------------------------------------
-- chapter 5.2: Polygon Definition (contours)

defineComplexContour ::
   Storable v =>
   TessellatorObj -> Ptr (ComplexContour v) -> ComplexContour v -> IO ()
defineComplexContour tessObj ptr (ComplexContour annotatedVertices) =
   tessBeginEndContour tessObj $
         let loop _ []     = return ()
             loop p (v:vs) = do defineVertex tessObj (castPtr p)
                                loop (p `plusPtr` sizeOf v) vs
         in loop ptr annotatedVertices

tessBeginEndContour :: TessellatorObj -> IO a -> IO a
tessBeginEndContour tessObj f = do
   gluTessBeginContour tessObj
   res <- f
   gluTessEndContour tessObj
   return res

--------------------------------------------------------------------------------
-- chapter 5.2: Polygon Definition (vertices)

defineVertex :: TessellatorObj -> Ptr (AnnotatedVertex v) -> IO ()
defineVertex tessObj ptr = gluTessVertex tessObj (castPtr ptr) ptr

--------------------------------------------------------------------------------
-- chapter 5.3: Callbacks (begin)

type BeginCallback  = PrimitiveMode -> IO ()

withBeginCallback :: TessellatorObj -> BeginCallback -> IO a -> IO a
withBeginCallback tessObj beginCallback action =
   bracket (makeTessBeginCallback (beginCallback . unmarshalPrimitiveMode))
           freeHaskellFunPtr $ \callbackPtr -> do
      gluTessCallback tessObj glu_TESS_BEGIN callbackPtr
      action

--------------------------------------------------------------------------------
-- chapter 5.3: Callbacks (edgeFlag)

type EdgeFlagCallback  = EdgeFlag -> IO ()

withEdgeFlagCallback :: TessellatorObj -> EdgeFlagCallback -> IO a -> IO a
withEdgeFlagCallback tessObj edgeFlagCallback action =
   bracket (makeTessEdgeFlagCallback (edgeFlagCallback . unmarshalEdgeFlag))
           freeHaskellFunPtr $ \callbackPtr -> do
      gluTessCallback tessObj glu_TESS_EDGE_FLAG callbackPtr
      action

--------------------------------------------------------------------------------
-- chapter 5.3: Callbacks (vertex)

type VertexCallback v = AnnotatedVertex v -> IO ()

withVertexCallback ::
   Storable v => TessellatorObj -> VertexCallback v -> IO a -> IO a
withVertexCallback tessObj vertexCallback action =
   bracket (makeTessVertexCallback (\p -> peek p >>= vertexCallback))
           freeHaskellFunPtr $ \callbackPtr -> do
      gluTessCallback tessObj glu_TESS_VERTEX callbackPtr
      action

--------------------------------------------------------------------------------
-- chapter 5.3: Callbacks (end)

type EndCallback  = IO ()

withEndCallback :: TessellatorObj -> EndCallback -> IO a -> IO a
withEndCallback tessObj endCallback action =
   bracket (makeTessEndCallback endCallback) freeHaskellFunPtr $ \callbackPtr -> do
      gluTessCallback tessObj glu_TESS_END callbackPtr
      action

--------------------------------------------------------------------------------
-- chapter 5.3: Callbacks (error)

type ErrorCallback = GLenum -> IO ()

withErrorCallback :: TessellatorObj -> ErrorCallback -> IO a -> IO a
withErrorCallback tessObj errorCallback action =
   bracket (makeTessErrorCallback errorCallback)
           freeHaskellFunPtr $ \callbackPtr -> do
      gluTessCallback tessObj glu_TESS_ERROR callbackPtr
      action

checkForError :: TessellatorObj -> IO a -> IO a
checkForError tessObj = withErrorCallback tessObj recordErrorCode

--------------------------------------------------------------------------------
-- chapter 5.3: Callbacks (combine)

type CombineCallback v =
      Ptr GLdouble
   -> Ptr (Ptr (AnnotatedVertex v))
   -> Ptr GLfloat
   -> Ptr (Ptr (AnnotatedVertex v))
   -> IO ()

withCombineCallback ::
   Storable v => TessellatorObj -> Combiner v -> IO a -> IO a
withCombineCallback tessObj combiner action =
   withPool $ \vertexPool ->
      bracket (makeTessCombineCallback (combineProperties vertexPool combiner))
              freeHaskellFunPtr $ \callbackPtr -> do
         gluTessCallback tessObj glu_TESS_COMBINE callbackPtr
         action

-- NOTE: SGI's tesselator has a bug, sometimes passing NULL for the last two
-- vertices instead of valid vertex data, so we have to work around this. We
-- just pass the first vertex in these cases, which is OK, because the
-- corresponding weight is 0.
combineProperties :: Storable v => Pool -> Combiner v -> CombineCallback v
combineProperties pool combiner newVertexPtr propertyPtrs weights result = do
   newVertex <- peek (castPtr newVertexPtr :: Ptr (Vertex3 GLdouble))
   [v0, v1, v2, v3] <- mapM (getProperty propertyPtrs) [0..3]
   [w0, w1, w2, w3] <- peekArray 4 weights
   let defaultProperty = fromJust v0
       f = fromMaybe defaultProperty
       wp = WeightedProperties (w0, f v0) (w1, f v1) (w2, f v2) (w3, f v3)
       av = AnnotatedVertex newVertex (combiner newVertex wp)
   poke result =<< pooledNew pool av

getProperty :: Storable v => Ptr (Ptr (AnnotatedVertex v)) -> Int -> IO (Maybe v)
getProperty propertyPtrs n = peekElemOff propertyPtrs n >>=
                             maybeNullPtr (return Nothing) peekProperty

peekProperty :: Storable v => Ptr (AnnotatedVertex v) -> IO (Maybe v)
peekProperty ptr = do
   AnnotatedVertex _ v <- peek ptr
   return (Just v)

--------------------------------------------------------------------------------
-- chapter 5.4: Control over Tessellation

setTessellatorProperties ::
    TessellatorObj -> TessWinding -> Tolerance -> Normal3 GLdouble -> Bool
 -> IO ()
setTessellatorProperties tessObj windingRule tolerance theNormal boundaryOnly = do
   setWindingRule tessObj windingRule
   setTolerance tessObj tolerance
   setNormal tessObj theNormal
   setBoundaryOnly tessObj boundaryOnly

setWindingRule :: TessellatorObj -> TessWinding -> IO ()
setWindingRule tessObj =
   gluTessProperty tessObj glu_TESS_WINDING_RULE . fromIntegral . marshalTessWinding

setBoundaryOnly :: TessellatorObj -> Bool -> IO ()
setBoundaryOnly tessObj =
   gluTessProperty tessObj glu_TESS_BOUNDARY_ONLY . marshalGLboolean

setTolerance :: TessellatorObj -> Tolerance -> IO ()
setTolerance tessObj = gluTessProperty tessObj glu_TESS_TOLERANCE

setNormal :: TessellatorObj -> Normal3 GLdouble -> IO ()
setNormal tessObj (Normal3 x y z) = gluTessNormal tessObj x y z
