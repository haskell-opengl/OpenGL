--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.Tessellation
-- Copyright   :  (c) Sven Panne 2002
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
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

   -- * Tesselation parameters
   TessWinding(..), Tolerance,

   -- * Tesselator type
   Tesselator,

   -- * Contour extraction
   SimpleContour(..), PolygonContours(..), extractContours,

   -- * Triangulation
   EdgeFlag(..), TriangleVertex, Triangle(..), Triangulation(..), triangulate,

   -- * Tesselation into primitives
   Primitive(..), SimplePolygon(..), tesselate
) where

import Control.Exception ( finally )
import Control.Monad ( foldM, liftM )
import Data.Either ( Either )
import Data.IORef ( newIORef, readIORef, writeIORef, modifyIORef )
import Foreign.Marshal.Alloc ( allocaBytes )
import Foreign.Marshal.Array ( peekArray, pokeArray )
import Foreign.Marshal.Pool ( Pool, withPool, pooledNew )
import Foreign.Ptr ( Ptr, nullPtr, plusPtr, castPtr, FunPtr, freeHaskellFunPtr )
import Foreign.Storable ( Storable(..) )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLboolean, marshalGLboolean, unmarshalGLboolean, GLclampf, GLdouble, GLenum )
import Graphics.Rendering.OpenGL.GL.BeginEnd (
   BeginMode, unmarshalBeginMode )
import Graphics.Rendering.OpenGL.GL.VertexSpec (
   Vertex3(..), Normal3(..) )
import Graphics.Rendering.OpenGL.GLU.Errors (
   Error(..), makeError, outOfMemoryError )
import Graphics.Rendering.OpenGL.GLU.Constants (
   TessCallback(..), marshalTessCallback,
   TessProperty(..), marshalTessProperty,
   TessWinding(..), marshalTessWinding )

--------------------------------------------------------------------------------

-- | The basic building block in tesselation is a 3D vertex with an associated
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
   foldM pokeAndAdvance (castPtr ptr) complexContours >> return ()
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
   = WeightedProperties (GLclampf, v)
                        (GLclampf, v)
                        (GLclampf, v)
                        (GLclampf, v)
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

-- | A general tesselator type.
--
-- Before tesselation of a complex polygon, all its vertices are projected into
-- a plane perpendicular to the given normal. If the given normal is
-- @Normal3 0 0 0@, a fitting plane of all vertices is used.

type Tesselator p v
   = TessWinding
  -> Tolerance
  -> Normal3 GLdouble
  -> Combiner v
  -> ComplexPolygon v
  -> IO (Either Error (p v))

--------------------------------------------------------------------------------

-- | A simple, non-self-intersecting contour

newtype SimpleContour v = SimpleContour [AnnotatedVertex v]
   deriving ( Eq, Ord )

-- | The contours of a complex polygon, represented by one or more
-- non-intersecting simple contours

newtype PolygonContours v = PolygonContours [SimpleContour v]
   deriving ( Eq, Ord )

extractContours :: Storable v => Tesselator PolygonContours v
extractContours windingRule tolerance normal combiner complexPoly = do

   vertices <- newIORef []
   let addVertex v = modifyIORef vertices (v:)

   contours <- newIORef []
   let finishContour = do
          vs <- readIORef vertices
          writeIORef vertices []
          modifyIORef contours (SimpleContour (reverse vs) :)

       getContours = liftM (PolygonContours . reverse) (readIORef contours)

   withTesselatorObj $ \tessObj -> do
      setTesselatorProperties tessObj windingRule tolerance normal True
      withVertexCallback tessObj addVertex $
         withEndCallback tessObj finishContour $
            checkForError tessObj $
               withCombineCallback tessObj combiner $ do
                  defineComplexPolygon tessObj complexPoly
                  getContours

--------------------------------------------------------------------------------

-- | A vertex can begin an edge which lies in the interior of its polygon or on
-- the polygon\'s boundary.

data EdgeFlag = BeginsInteriorEdge | BeginsBoundaryEdge
   deriving ( Eq, Ord )

unmarshalEdgeFlag :: GLboolean -> EdgeFlag
unmarshalEdgeFlag f
   | unmarshalGLboolean f = BeginsBoundaryEdge
   | otherwise            = BeginsInteriorEdge

-- | A triangle vertex with additional information about the edge it begins

type TriangleVertex v = AnnotatedVertex (v,EdgeFlag)

-- | A triangle, represented by three triangle vertices

data Triangle v
   = Triangle (TriangleVertex v) (TriangleVertex v) (TriangleVertex v)
   deriving ( Eq, Ord )

-- | A triangulation of a complex polygon

newtype Triangulation v = Triangulation [Triangle v]
   deriving ( Eq, Ord )

triangulate :: Storable v => Tesselator Triangulation v
triangulate windingRule tolerance normal combiner complexPoly = do

   edgeFlagState <- newIORef BeginsInteriorEdge
   let registerEdgeFlag = writeIORef edgeFlagState

   vertices <- newIORef []
   let addVertex (AnnotatedVertex xyz v) = do
          ef <- readIORef edgeFlagState
          modifyIORef vertices (AnnotatedVertex xyz (v,ef) :)

       getTriangulation = do
          vs <- readIORef vertices
          return $ Triangulation (collectTriangles (reverse vs))

   withTesselatorObj $ \tessObj -> do
      setTesselatorProperties tessObj windingRule tolerance normal False
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

data Primitive v = Primitive BeginMode [AnnotatedVertex v]
   deriving ( Eq, Ord )

newtype SimplePolygon v = SimplePolygon [Primitive v]
   deriving ( Eq, Ord )

tesselate :: Storable v => Tesselator SimplePolygon v
tesselate windingRule tolerance normal combiner complexPoly = do

   beginModeState <- newIORef undefined
   let setBeginMode = writeIORef beginModeState

   vertices <- newIORef []
   let addVertex v = modifyIORef vertices (v:)

   primitives <- newIORef []
   let finishPrimitive = do
          beginMode <- readIORef beginModeState
          vs <- readIORef vertices
          writeIORef vertices []
          modifyIORef primitives (Primitive beginMode (reverse vs) :)

       getSimplePolygon = liftM (SimplePolygon . reverse) (readIORef primitives)

   withTesselatorObj $ \tessObj -> do
      setTesselatorProperties tessObj windingRule tolerance normal False
      withBeginCallback tessObj setBeginMode $
         withVertexCallback tessObj addVertex $
            withEndCallback tessObj finishPrimitive $
               checkForError tessObj $
                  withCombineCallback tessObj combiner $ do
                     defineComplexPolygon tessObj complexPoly
                     getSimplePolygon

--------------------------------------------------------------------------------
-- chapter 5.1: The Tesselation Object

-- 'Char' is a fake here, any marshalable type would do
newtype TesselatorObj v = TesselatorObj (Ptr Char)
   deriving ( Eq )

withTesselatorObj ::
   (TesselatorObj v -> IO (Either Error a)) -> IO (Either Error a)
withTesselatorObj action = do
   tessObj <- gluNewTess
   if tessObj == TesselatorObj nullPtr
      then return $ Left outOfMemoryError
      else action tessObj `finally` gluDeleteTess tessObj

foreign import CALLCONV unsafe "gluNewTess" gluNewTess :: IO (TesselatorObj v)

foreign import CALLCONV unsafe "gluDeleteTess" gluDeleteTess ::
   TesselatorObj v -> IO ()

--------------------------------------------------------------------------------
-- chapter 5.2: Polygon Definition (polygons)

defineComplexPolygon ::
   Storable v => TesselatorObj v -> ComplexPolygon v -> IO ()
defineComplexPolygon tessObj cp@(ComplexPolygon complexContours) =
   withComplexPolygon cp $ \ptr ->
      tessBeginEndPolygon tessObj nullPtr $
         let loop _ []     = return ()
             loop p (c:cs) = do defineComplexContour tessObj (castPtr p) c
                                loop (p `plusPtr` sizeOfComplexContour c) cs
         in loop ptr complexContours

tessBeginEndPolygon :: TesselatorObj v -> Ptr p -> IO a -> IO a
tessBeginEndPolygon tessObj ptr f = do
   gluTessBeginPolygon tessObj ptr
   res <- f
   gluTessEndPolygon tessObj
   return res

foreign import CALLCONV safe "gluTessBeginPolygon" gluTessBeginPolygon ::
   TesselatorObj v -> Ptr p -> IO ()

foreign import CALLCONV safe "gluTessEndPolygon" gluTessEndPolygon ::
   TesselatorObj v -> IO ()

--------------------------------------------------------------------------------
-- chapter 5.2: Polygon Definition (contours)

defineComplexContour ::
   Storable v =>
   TesselatorObj v -> Ptr (ComplexContour v) -> ComplexContour v -> IO ()
defineComplexContour tessObj ptr (ComplexContour annotatedVertices) =
   tessBeginEndContour tessObj $
         let loop _ []     = return ()
             loop p (v:vs) = do defineVertex tessObj (castPtr p)
                                loop (p `plusPtr` sizeOf v) vs
         in loop ptr annotatedVertices

tessBeginEndContour :: TesselatorObj v -> IO a -> IO a
tessBeginEndContour tessObj f = do
   gluTessBeginContour tessObj
   res <- f
   gluTessEndContour tessObj
   return res

foreign import CALLCONV safe "gluTessBeginContour" gluTessBeginContour ::
   TesselatorObj v -> IO ()

foreign import CALLCONV safe "gluTessEndContour" gluTessEndContour ::
   TesselatorObj v -> IO ()

--------------------------------------------------------------------------------
-- chapter 5.2: Polygon Definition (vertices)

defineVertex :: TesselatorObj v -> Ptr (AnnotatedVertex v) -> IO ()
defineVertex tessObj ptr = gluTessVertex tessObj (castPtr ptr) ptr

foreign import CALLCONV safe "gluTessVertex" gluTessVertex ::
   TesselatorObj v -> Ptr (Vertex3 GLdouble) -> Ptr (AnnotatedVertex v) -> IO ()

--------------------------------------------------------------------------------
-- chapter 5.3: Callbacks (begin)

type BeginCallback  = BeginMode -> IO ()

type BeginCallback' = GLenum -> IO ()

withBeginCallback :: TesselatorObj v -> BeginCallback -> IO a -> IO a
withBeginCallback tessObj beginCallback action = do
   callbackPtr <- makeBeginCallback (beginCallback . unmarshalBeginMode)
   setBeginCallback tessObj (marshalTessCallback TessBegin) callbackPtr
   action `finally` freeHaskellFunPtr callbackPtr

foreign import ccall "wrapper" makeBeginCallback ::
   BeginCallback' -> IO (FunPtr BeginCallback')

foreign import CALLCONV unsafe "gluTessCallback" setBeginCallback ::
   TesselatorObj v -> GLenum -> FunPtr BeginCallback' -> IO ()

--------------------------------------------------------------------------------
-- chapter 5.3: Callbacks (edgeFlag)

type EdgeFlagCallback  = EdgeFlag -> IO ()

type EdgeFlagCallback' = GLboolean -> IO ()

withEdgeFlagCallback :: TesselatorObj v -> EdgeFlagCallback -> IO a -> IO a
withEdgeFlagCallback tessObj edgeFlagCallback action = do
   callbackPtr <- makeEdgeFlagCallback (edgeFlagCallback . unmarshalEdgeFlag)
   setEdgeFlagCallback tessObj (marshalTessCallback TessEdgeFlag) callbackPtr
   action `finally` freeHaskellFunPtr callbackPtr

foreign import ccall "wrapper" makeEdgeFlagCallback ::
   EdgeFlagCallback' -> IO (FunPtr EdgeFlagCallback')

foreign import CALLCONV unsafe "gluTessCallback" setEdgeFlagCallback ::
   TesselatorObj v -> GLenum -> FunPtr EdgeFlagCallback' -> IO ()

--------------------------------------------------------------------------------
-- chapter 5.3: Callbacks (vertex)

type VertexCallback v = AnnotatedVertex v -> IO ()

type VertexCallback' v = Ptr (AnnotatedVertex v) -> IO ()

withVertexCallback ::
   Storable v => TesselatorObj v -> VertexCallback v -> IO a -> IO a
withVertexCallback tessObj vertexCallback action = do
   callbackPtr <- makeVertexCallback (\p -> peek p >>= vertexCallback)
   setVertexCallback tessObj (marshalTessCallback TessVertex) callbackPtr
   action `finally` freeHaskellFunPtr callbackPtr

foreign import ccall "wrapper" makeVertexCallback ::
   VertexCallback' v -> IO (FunPtr (VertexCallback' v))

foreign import CALLCONV unsafe "gluTessCallback" setVertexCallback ::
   TesselatorObj v -> GLenum -> FunPtr (VertexCallback' v) -> IO ()

--------------------------------------------------------------------------------
-- chapter 5.3: Callbacks (end)

type EndCallback  = IO ()

withEndCallback :: TesselatorObj v -> EndCallback -> IO a -> IO a
withEndCallback tessObj endCallback action = do
   callbackPtr <- makeEndCallback endCallback
   setEndCallback tessObj (marshalTessCallback TessEnd) callbackPtr
   action `finally` freeHaskellFunPtr callbackPtr

foreign import ccall "wrapper" makeEndCallback ::
   EndCallback -> IO (FunPtr EndCallback)

foreign import CALLCONV unsafe "gluTessCallback" setEndCallback ::
   TesselatorObj v -> GLenum -> FunPtr EndCallback -> IO ()

--------------------------------------------------------------------------------
-- chapter 5.3: Callbacks (error)

type ErrorCallback  = Error -> IO ()

type ErrorCallback' = GLenum -> IO ()

withErrorCallback :: TesselatorObj v -> ErrorCallback -> IO a -> IO a
withErrorCallback tessObj errorCallback action = do
   callbackPtr <- makeErrorCallback (\e -> makeError e >>= errorCallback)
   setErrorCallback tessObj (marshalTessCallback TessError) callbackPtr
   action `finally` freeHaskellFunPtr callbackPtr

foreign import ccall "wrapper" makeErrorCallback ::
   ErrorCallback' -> IO (FunPtr ErrorCallback')

foreign import CALLCONV unsafe "gluTessCallback" setErrorCallback ::
   TesselatorObj v -> GLenum -> FunPtr ErrorCallback' -> IO ()

checkForError :: TesselatorObj v -> IO a -> IO (Either Error a)
checkForError tessObj action = do
   maybeErrorRef <- newIORef Nothing
   withErrorCallback tessObj (writeIORef maybeErrorRef . Just) $ do
      res <- action
      maybeError <- readIORef maybeErrorRef
      return $ maybe (Right res) Left maybeError

--------------------------------------------------------------------------------
-- chapter 5.3: Callbacks (combine)

type CombineCallback v =
      Ptr (Vertex3 GLdouble)
   -> Ptr (Ptr (AnnotatedVertex v))
   -> Ptr GLclampf
   -> Ptr (Ptr (AnnotatedVertex v))
   -> IO ()

withCombineCallback ::
   Storable v => TesselatorObj v -> Combiner v -> IO a -> IO a
withCombineCallback tessObj combiner action =
   withPool $ \vertexPool -> do
      let callback = combineProperties vertexPool combiner
      callbackPtr <- makeCombineCallback callback
      setCombineCallback tessObj (marshalTessCallback TessCombine) callbackPtr
      action `finally` freeHaskellFunPtr callbackPtr

combineProperties :: Storable v => Pool -> Combiner v -> CombineCallback v
combineProperties pool combiner newVertexPtr propertyPtrs weights result = do
   newVertex <- peek newVertexPtr
   [v0, v1, v2, v3] <- mapM (getProperty propertyPtrs) [0..3]
   [w0, w1, w2, w3] <- peekArray 4 weights
   let wp = WeightedProperties (w0,v0) (w1,v1) (w2,v2) (w3,v3)
       av = AnnotatedVertex newVertex (combiner newVertex wp)
   poke result =<< pooledNew pool av

getProperty :: Storable v => Ptr (Ptr (AnnotatedVertex v)) -> Int -> IO v
getProperty propertyPtrs n = do
   AnnotatedVertex _ v <- peek =<< peekElemOff propertyPtrs n
   return v

foreign import ccall "wrapper" makeCombineCallback ::
   CombineCallback v -> IO (FunPtr (CombineCallback v))

foreign import CALLCONV unsafe "gluTessCallback" setCombineCallback ::
   TesselatorObj v -> GLenum -> FunPtr (CombineCallback v) -> IO ()

--------------------------------------------------------------------------------
-- chapter 5.4: Control over Tesselation

setTesselatorProperties ::
    TesselatorObj v -> TessWinding -> Tolerance -> Normal3 GLdouble -> Bool
 -> IO ()
setTesselatorProperties tessObj windingRule tolerance normal boundaryOnly = do
   setWindingRule tessObj windingRule
   setTolerance tessObj tolerance
   setNormal tessObj normal
   setBoundaryOnly tessObj boundaryOnly

setWindingRule :: TesselatorObj v -> TessWinding -> IO ()
setWindingRule tessObj =
   tessProperty tessObj TessWindingRule . fromIntegral . marshalTessWinding

setBoundaryOnly :: TesselatorObj v -> Bool -> IO ()
setBoundaryOnly tessObj =
   tessProperty tessObj TessBoundaryOnly . fromIntegral . marshalGLboolean

setTolerance :: TesselatorObj v -> Tolerance -> IO ()
setTolerance tessObj =
   tessProperty tessObj TessTolerance

tessProperty :: TesselatorObj v -> TessProperty -> GLdouble -> IO ()
tessProperty tessObj =
   gluTessProperty tessObj . marshalTessProperty

foreign import CALLCONV unsafe "gluTessProperty" gluTessProperty ::
   TesselatorObj v -> GLenum -> GLdouble -> IO ()

setNormal :: TesselatorObj v -> Normal3 GLdouble -> IO ()
setNormal tessObj (Normal3 x y z) = gluTessNormal tessObj x y z

foreign import CALLCONV unsafe "gluTessNormal" gluTessNormal ::
   TesselatorObj v -> GLdouble -> GLdouble -> GLdouble -> IO ()
