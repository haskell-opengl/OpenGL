--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.Tessellation
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
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

import Control.Monad ( foldM, liftM )
import Data.Either ( Either )
import Data.IORef ( newIORef, readIORef, writeIORef, modifyIORef )
import Foreign.Marshal.Alloc ( allocaBytes )
import Foreign.Marshal.Array ( peekArray, pokeArray )
import Foreign.Marshal.Pool ( Pool, withPool, pooledNew )
import Foreign.Ptr ( Ptr, nullPtr, plusPtr, castPtr, FunPtr, freeHaskellFunPtr )
import Foreign.Storable ( Storable(..) )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLclampf, GLdouble, GLenum )
import Graphics.Rendering.OpenGL.GL.EdgeFlag ( unmarshalEdgeFlag )
import Graphics.Rendering.OpenGL.GL.Exception ( finally )
import Graphics.Rendering.OpenGL.GL.GLboolean ( GLboolean, marshalGLboolean )
import Graphics.Rendering.OpenGL.GL.PrimitiveMode ( unmarshalPrimitiveMode )
import Graphics.Rendering.OpenGL.GL.BeginEnd (
   PrimitiveMode, EdgeFlag(BeginsInteriorEdge) )
import Graphics.Rendering.OpenGL.GL.VertexSpec (
   Vertex3(..), Normal3(..) )
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal ( makeError )
import Graphics.Rendering.OpenGL.GLU.Errors (
   Error(Error), ErrorCategory(OutOfMemory) )

--------------------------------------------------------------------------------

data TessCallback =
     TessBegin
   | Begin
   | TessVertex
   | Vertex
   | TessEnd
   | End
   | TessError
   | Error'
   | TessEdgeFlag
   | EdgeFlag
   | TessCombine
   | TessBeginData
   | TessVertexData
   | TessEndData
   | TessErrorData
   | TessEdgeFlagData
   | TessCombineData

marshalTessCallback :: TessCallback -> GLenum
marshalTessCallback x = case x of
   TessBegin -> 0x18704
   Begin -> 0x18704
   TessVertex -> 0x18705
   Vertex -> 0x18705
   TessEnd -> 0x18706
   End -> 0x18706
   TessError -> 0x18707
   Error' -> 0x18707
   TessEdgeFlag -> 0x18708
   EdgeFlag -> 0x18708
   TessCombine -> 0x18709
   TessBeginData -> 0x1870a
   TessVertexData -> 0x1870b
   TessEndData -> 0x1870c
   TessErrorData -> 0x1870d
   TessEdgeFlagData -> 0x1870e
   TessCombineData -> 0x1870f

--------------------------------------------------------------------------------

data TessProperty =
     TessWindingRule
   | TessBoundaryOnly
   | TessTolerance

marshalTessProperty :: TessProperty -> GLenum
marshalTessProperty x = case x of
   TessWindingRule -> 0x1872c
   TessBoundaryOnly -> 0x1872d
   TessTolerance -> 0x1872e

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
   TessWindingOdd -> 0x18722
   TessWindingNonzero -> 0x18723
   TessWindingPositive -> 0x18724
   TessWindingNegative -> 0x18725
   TessWindingAbsGeqTwo -> 0x18726

--------------------------------------------------------------------------------

-- | The basic building block in tessellation is a 3D vertex with an associated
-- property, e.g. color, texture coordinates, etc.

data AnnotatedVertex v = AnnotatedVertex (Vertex3 GLdouble) v
#ifdef __HADDOCK__
-- Help Haddock a bit, because it doesn't do any instance inference.
instance Eq v => Eq (AnnotatedVertex v)
instance Ord v => Ord (AnnotatedVertex v)
#else
   deriving ( Eq, Ord )
#endif

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
#ifdef __HADDOCK__
-- Help Haddock a bit, because it doesn't do any instance inference.
instance Eq v => Eq (ComplexContour v)
instance Ord v => Ord (ComplexContour v)
#else
   deriving ( Eq, Ord )
#endif

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
#ifdef __HADDOCK__
-- Help Haddock a bit, because it doesn't do any instance inference.
instance Eq v => Eq (ComplexPolygon v)
instance Ord v => Ord (ComplexPolygon v)
#else
   deriving ( Eq, Ord )
#endif

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
#ifdef __HADDOCK__
-- Help Haddock a bit, because it doesn't do any instance inference.
instance Eq v => Eq (WeightedProperties v)
instance Ord v => Ord (WeightedProperties v)
#else
   deriving ( Eq, Ord )
#endif

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
  -> IO (Either Error (p v))

--------------------------------------------------------------------------------

-- | A simple, non-self-intersecting contour

newtype SimpleContour v = SimpleContour [AnnotatedVertex v]
#ifdef __HADDOCK__
-- Help Haddock a bit, because it doesn't do any instance inference.
instance Eq v => Eq (SimpleContour v)
instance Ord v => Ord (SimpleContour v)
#else
   deriving ( Eq, Ord )
#endif

-- | The contours of a complex polygon, represented by one or more
-- non-intersecting simple contours

newtype PolygonContours v = PolygonContours [SimpleContour v]
#ifdef __HADDOCK__
-- Help Haddock a bit, because it doesn't do any instance inference.
instance Eq v => Eq (PolygonContours v)
instance Ord v => Ord (PolygonContours v)
#else
   deriving ( Eq, Ord )
#endif

extractContours :: Storable v => Tessellator PolygonContours v
extractContours windingRule tolerance normal combiner complexPoly = do

   vertices <- newIORef []
   let addVertex v = modifyIORef vertices (v:)

   contours <- newIORef []
   let finishContour = do
          vs <- readIORef vertices
          writeIORef vertices []
          modifyIORef contours (SimpleContour (reverse vs) :)

       getContours = liftM (PolygonContours . reverse) (readIORef contours)

   withTessellatorObj $ \tessObj -> do
      setTessellatorProperties tessObj windingRule tolerance normal True
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
#ifdef __HADDOCK__
-- Help Haddock a bit, because it doesn't do any instance inference.
instance Eq v => Eq (Triangle v)
instance Ord v => Ord (Triangle v)
#else
   deriving ( Eq, Ord )
#endif

-- | A triangulation of a complex polygon

newtype Triangulation v = Triangulation [Triangle v]
#ifdef __HADDOCK__
-- Help Haddock a bit, because it doesn't do any instance inference.
instance Eq v => Eq (Triangulation v)
instance Ord v => Ord (Triangulation v)
#else
   deriving ( Eq, Ord )
#endif

triangulate :: Storable v => Tessellator Triangulation v
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

   withTessellatorObj $ \tessObj -> do
      setTessellatorProperties tessObj windingRule tolerance normal False
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
#ifdef __HADDOCK__
-- Help Haddock a bit, because it doesn't do any instance inference.
instance Eq v => Eq (Primitive v)
instance Ord v => Ord (Primitive v)
#else
   deriving ( Eq, Ord )
#endif

newtype SimplePolygon v = SimplePolygon [Primitive v]
#ifdef __HADDOCK__
-- Help Haddock a bit, because it doesn't do any instance inference.
instance Eq v => Eq (SimplePolygon v)
instance Ord v => Ord (SimplePolygon v)
#else
   deriving ( Eq, Ord )
#endif

tessellate :: Storable v => Tessellator SimplePolygon v
tessellate windingRule tolerance normal combiner complexPoly = do

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

       getSimplePolygon = liftM (SimplePolygon . reverse) (readIORef primitives)

   withTessellatorObj $ \tessObj -> do
      setTessellatorProperties tessObj windingRule tolerance normal False
      withBeginCallback tessObj setPrimitiveMode $
         withVertexCallback tessObj addVertex $
            withEndCallback tessObj finishPrimitive $
               checkForError tessObj $
                  withCombineCallback tessObj combiner $ do
                     defineComplexPolygon tessObj complexPoly
                     getSimplePolygon

--------------------------------------------------------------------------------
-- chapter 5.1: The Tessellation Object

-- 'Char' is a fake here, any marshalable type would do
newtype TessellatorObj = TessellatorObj (Ptr Char)
   deriving ( Eq )

withTessellatorObj ::
   (TessellatorObj -> IO (Either Error a)) -> IO (Either Error a)
withTessellatorObj action = do
   tessObj <- gluNewTess
   if tessObj == TessellatorObj nullPtr
      then return $ Left (Error OutOfMemory "out of memory")
      else action tessObj `finally` gluDeleteTess tessObj

foreign import CALLCONV unsafe "gluNewTess" gluNewTess :: IO (TessellatorObj)

foreign import CALLCONV unsafe "gluDeleteTess" gluDeleteTess ::
   TessellatorObj -> IO ()

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

foreign import CALLCONV safe "gluTessBeginPolygon" gluTessBeginPolygon ::
   TessellatorObj -> Ptr p -> IO ()

foreign import CALLCONV safe "gluTessEndPolygon" gluTessEndPolygon ::
   TessellatorObj -> IO ()

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

foreign import CALLCONV safe "gluTessBeginContour" gluTessBeginContour ::
   TessellatorObj -> IO ()

foreign import CALLCONV safe "gluTessEndContour" gluTessEndContour ::
   TessellatorObj -> IO ()

--------------------------------------------------------------------------------
-- chapter 5.2: Polygon Definition (vertices)

defineVertex :: TessellatorObj -> Ptr (AnnotatedVertex v) -> IO ()
defineVertex tessObj ptr = gluTessVertex tessObj (castPtr ptr) ptr

foreign import CALLCONV safe "gluTessVertex" gluTessVertex ::
   TessellatorObj -> Ptr (Vertex3 GLdouble) -> Ptr (AnnotatedVertex v) -> IO ()

--------------------------------------------------------------------------------
-- chapter 5.3: Callbacks (begin)

type BeginCallback  = PrimitiveMode -> IO ()

type BeginCallback' = GLenum -> IO ()

withBeginCallback :: TessellatorObj -> BeginCallback -> IO a -> IO a
withBeginCallback tessObj beginCallback action = do
   callbackPtr <- makeBeginCallback (beginCallback . unmarshalPrimitiveMode)
   setBeginCallback tessObj (marshalTessCallback TessBegin) callbackPtr
   action `finally` freeHaskellFunPtr callbackPtr

foreign import ccall "wrapper" makeBeginCallback ::
   BeginCallback' -> IO (FunPtr BeginCallback')

foreign import CALLCONV unsafe "gluTessCallback" setBeginCallback ::
   TessellatorObj -> GLenum -> FunPtr BeginCallback' -> IO ()

--------------------------------------------------------------------------------
-- chapter 5.3: Callbacks (edgeFlag)

type EdgeFlagCallback  = EdgeFlag -> IO ()

type EdgeFlagCallback' = GLboolean -> IO ()

withEdgeFlagCallback :: TessellatorObj -> EdgeFlagCallback -> IO a -> IO a
withEdgeFlagCallback tessObj edgeFlagCallback action = do
   callbackPtr <- makeEdgeFlagCallback (edgeFlagCallback . unmarshalEdgeFlag)
   setEdgeFlagCallback tessObj (marshalTessCallback TessEdgeFlag) callbackPtr
   action `finally` freeHaskellFunPtr callbackPtr

foreign import ccall "wrapper" makeEdgeFlagCallback ::
   EdgeFlagCallback' -> IO (FunPtr EdgeFlagCallback')

foreign import CALLCONV unsafe "gluTessCallback" setEdgeFlagCallback ::
   TessellatorObj -> GLenum -> FunPtr EdgeFlagCallback' -> IO ()

--------------------------------------------------------------------------------
-- chapter 5.3: Callbacks (vertex)

type VertexCallback v = AnnotatedVertex v -> IO ()

type VertexCallback' v = Ptr (AnnotatedVertex v) -> IO ()

withVertexCallback ::
   Storable v => TessellatorObj -> VertexCallback v -> IO a -> IO a
withVertexCallback tessObj vertexCallback action = do
   callbackPtr <- makeVertexCallback (\p -> peek p >>= vertexCallback)
   setVertexCallback tessObj (marshalTessCallback TessVertex) callbackPtr
   action `finally` freeHaskellFunPtr callbackPtr

foreign import ccall "wrapper" makeVertexCallback ::
   VertexCallback' v -> IO (FunPtr (VertexCallback' v))

foreign import CALLCONV unsafe "gluTessCallback" setVertexCallback ::
   TessellatorObj -> GLenum -> FunPtr (VertexCallback' v) -> IO ()

--------------------------------------------------------------------------------
-- chapter 5.3: Callbacks (end)

type EndCallback  = IO ()

withEndCallback :: TessellatorObj -> EndCallback -> IO a -> IO a
withEndCallback tessObj endCallback action = do
   callbackPtr <- makeEndCallback endCallback
   setEndCallback tessObj (marshalTessCallback TessEnd) callbackPtr
   action `finally` freeHaskellFunPtr callbackPtr

foreign import ccall "wrapper" makeEndCallback ::
   EndCallback -> IO (FunPtr EndCallback)

foreign import CALLCONV unsafe "gluTessCallback" setEndCallback ::
   TessellatorObj -> GLenum -> FunPtr EndCallback -> IO ()

--------------------------------------------------------------------------------
-- chapter 5.3: Callbacks (error)

type ErrorCallback  = Error -> IO ()

type ErrorCallback' = GLenum -> IO ()

withErrorCallback :: TessellatorObj -> ErrorCallback -> IO a -> IO a
withErrorCallback tessObj errorCallback action = do
   callbackPtr <- makeErrorCallback (\e -> makeError e >>= errorCallback)
   setErrorCallback tessObj (marshalTessCallback TessError) callbackPtr
   action `finally` freeHaskellFunPtr callbackPtr

foreign import ccall "wrapper" makeErrorCallback ::
   ErrorCallback' -> IO (FunPtr ErrorCallback')

foreign import CALLCONV unsafe "gluTessCallback" setErrorCallback ::
   TessellatorObj -> GLenum -> FunPtr ErrorCallback' -> IO ()

checkForError :: TessellatorObj -> IO a -> IO (Either Error a)
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
   Storable v => TessellatorObj -> Combiner v -> IO a -> IO a
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
   TessellatorObj -> GLenum -> FunPtr (CombineCallback v) -> IO ()

--------------------------------------------------------------------------------
-- chapter 5.4: Control over Tessellation

setTessellatorProperties ::
    TessellatorObj -> TessWinding -> Tolerance -> Normal3 GLdouble -> Bool
 -> IO ()
setTessellatorProperties tessObj windingRule tolerance normal boundaryOnly = do
   setWindingRule tessObj windingRule
   setTolerance tessObj tolerance
   setNormal tessObj normal
   setBoundaryOnly tessObj boundaryOnly

setWindingRule :: TessellatorObj -> TessWinding -> IO ()
setWindingRule tessObj =
   tessProperty tessObj TessWindingRule . fromIntegral . marshalTessWinding

setBoundaryOnly :: TessellatorObj -> Bool -> IO ()
setBoundaryOnly tessObj =
   tessProperty tessObj TessBoundaryOnly . fromIntegral . marshalGLboolean

setTolerance :: TessellatorObj -> Tolerance -> IO ()
setTolerance tessObj =
   tessProperty tessObj TessTolerance

tessProperty :: TessellatorObj -> TessProperty -> GLdouble -> IO ()
tessProperty tessObj =
   gluTessProperty tessObj . marshalTessProperty

foreign import CALLCONV unsafe "gluTessProperty" gluTessProperty ::
   TessellatorObj -> GLenum -> GLdouble -> IO ()

setNormal :: TessellatorObj -> Normal3 GLdouble -> IO ()
setNormal tessObj (Normal3 x y z) = gluTessNormal tessObj x y z

foreign import CALLCONV unsafe "gluTessNormal" gluTessNormal ::
   TessellatorObj -> GLdouble -> GLdouble -> GLdouble -> IO ()
