--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Evaluators
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 5.1 (Evaluators) of the OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Evaluators (
   -- * Evaluator-related Types
   Order, maxOrder, Domain, MapDescriptor(..), ControlPoint,

   -- * Defining Evaluator Maps
   -- ** One-dimensional Evaluator Maps
   Map1(..), GLmap1, map1,

   -- ** Two-dimensional Evaluator Maps
   Map2(..), GLmap2, map2,

   -- * Using Evaluator Maps

   -- ** Evaluating an Arbitrary Coordinate Value
   evalCoord1, evalCoord1v, evalCoord2, evalCoord2v,

   -- ** Using Evenly Spaced Coordinate Values

   -- *** Defining a Grid
   mapGrid1, mapGrid2,

   -- *** Evaluating a Whole Mesh
   evalMesh1, evalMesh2,

   -- *** Evaluating a Single Point on a Mesh
   evalPoint1, evalPoint2,

   -- * Normal Generation
   autoNormal
) where

import Control.Monad ( liftM, zipWithM_ )
import Data.List ( genericLength )
import Foreign.ForeignPtr ( ForeignPtr, mallocForeignPtrArray, withForeignPtr )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( allocaArray )
import Foreign.Ptr ( Ptr, plusPtr )
import Foreign.Storable ( Storable(peek,poke,sizeOf) )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapAutoNormal,CapMap1Color4,CapMap1Index,CapMap1Normal,
             CapMap1TextureCoord1,CapMap1TextureCoord2,CapMap1TextureCoord3,
             CapMap1TextureCoord4,CapMap1Vertex3,CapMap1Vertex4,CapMap2Color4,
             CapMap2Index,CapMap2Normal,CapMap2TextureCoord1,
             CapMap2TextureCoord2,CapMap2TextureCoord3,CapMap2TextureCoord4,
             CapMap2Vertex3,CapMap2Vertex4),
   makeCapability, makeStateVarMaybe )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLint, GLfloat, GLdouble, Capability )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek2, peek4 )
import Graphics.Rendering.OpenGL.GL.PolygonMode ( marshalPolygonMode )
import Graphics.Rendering.OpenGL.GL.Polygons ( PolygonMode )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetMaxEvalOrder,
            GetMap1GridSegments,GetMap1GridDomain,
            GetMap2GridSegments,GetMap2GridDomain),
   getSizei1, getInteger1, getInteger2, getFloat2, getFloat4, getDouble2,
   getDouble4 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.VertexArrays ( NumComponents, Stride )
import Graphics.Rendering.OpenGL.GL.VertexSpec (
   Vertex3, Vertex4(..), TexCoord1, TexCoord2, TexCoord3, TexCoord4(..),
   Normal3(..), Color4(..), Index1(..) )

--------------------------------------------------------------------------------

#include "HsOpenGLTypes.h"

--------------------------------------------------------------------------------

type Order = GLint

maxOrder :: GettableStateVar Order
maxOrder = makeGettableStateVar (getSizei1 id GetMaxEvalOrder)

--------------------------------------------------------------------------------

class Storable d => Domain d where
   glMap1      :: GLenum -> d -> d -> GLint -> GLint -> Ptr d -> IO ()
   glMap2      :: GLenum -> d -> d -> GLint -> GLint -> d -> d -> GLint -> GLint -> Ptr d -> IO ()
   glGetMapv   :: GLenum -> GLenum -> Ptr d -> IO ()
   evalCoord1  :: d -> IO ()
   evalCoord1v :: Ptr d -> IO ()
   evalCoord2  :: (d, d) -> IO ()
   evalCoord2v :: Ptr d -> IO ()
   glMapGrid1  :: GLint -> d -> d -> IO ()
   glMapGrid2  :: GLint -> d -> d -> GLint -> d -> d -> IO ()
   get2        :: (d -> d -> a) -> GetPName -> IO a
   get4        :: (d -> d -> d -> d -> a) -> GetPName -> IO a

instance Domain GLfloat_ where
   glMap1      = glMap1f
   glMap2      = glMap2f
   glGetMapv   = glGetMapfv
   evalCoord1  = glEvalCoord1f
   evalCoord1v = glEvalCoord1fv
   evalCoord2  = uncurry glEvalCoord2f
   evalCoord2v = glEvalCoord2fv
   glMapGrid1  = glMapGrid1f
   glMapGrid2  = glMapGrid2f
   get2        = getFloat2
   get4        = getFloat4

instance Domain GLdouble_ where
   glMap1      = glMap1d
   glMap2      = glMap2d
   glGetMapv   = glGetMapdv
   evalCoord1  = glEvalCoord1d
   evalCoord1v = glEvalCoord1dv
   evalCoord2  = uncurry glEvalCoord2d
   evalCoord2v = glEvalCoord2dv
   glMapGrid1  = glMapGrid1d
   glMapGrid2  = glMapGrid2d
   get2        = getDouble2
   get4        = getDouble4

--------------------------------------------------------------------------------

data Domain d => MapDescriptor d =
   MapDescriptor (d, d) Stride Order NumComponents
   deriving ( Eq, Ord, Show )

totalComponents1 :: Domain d => MapDescriptor d -> Int
totalComponents1 (MapDescriptor _ stride order numComp) =
   fromIntegral stride * (fromIntegral order - 1) + fromIntegral numComp

totalComponents2 :: Domain d => MapDescriptor d -> MapDescriptor d -> Int
totalComponents2 uDescriptor vDescriptor@(MapDescriptor _ _ _ numComp) =
   totalComponents1 uDescriptor + totalComponents1 vDescriptor - fromIntegral numComp

--------------------------------------------------------------------------------

class ControlPoint c where
   map1Target       :: Domain d => c d -> MapTarget
   map2Target       :: Domain d => c d -> MapTarget
   enableCap1       :: Domain d => c d -> EnableCap
   enableCap2       :: Domain d => c d -> EnableCap
   numComponents    :: Domain d => c d -> Stride
   peekControlPoint :: Domain d => Ptr (c d) -> IO (c d)
   pokeControlPoint :: Domain d => Ptr (c d) -> (c d) -> IO ()

instance ControlPoint Vertex3 where
   map1Target       = const Map1Vertex3
   map2Target       = const Map2Vertex3
   enableCap1       = const CapMap1Vertex3
   enableCap2       = const CapMap2Vertex3
   numComponents    = const 3
   peekControlPoint = peek
   pokeControlPoint = poke

instance ControlPoint Vertex4 where
   map1Target       = const Map1Vertex4
   map2Target       = const Map2Vertex4
   enableCap1       = const CapMap1Vertex4
   enableCap2       = const CapMap2Vertex4
   numComponents    = const 4
   peekControlPoint = peek
   pokeControlPoint = poke

instance ControlPoint Index1 where
   map1Target       = const Map1Index
   map2Target       = const Map2Index
   enableCap1       = const CapMap1Index
   enableCap2       = const CapMap2Index
   numComponents    = const 1
   peekControlPoint = peek
   pokeControlPoint = poke

instance ControlPoint Color4 where
   map1Target       = const Map1Color4
   map2Target       = const Map2Color4
   enableCap1       = const CapMap1Color4
   enableCap2       = const CapMap2Color4
   numComponents    = const 4
   peekControlPoint = peek
   pokeControlPoint = poke

instance ControlPoint Normal3 where
   map1Target       = const Map1Normal
   map2Target       = const Map2Normal
   enableCap1       = const CapMap1Normal
   enableCap2       = const CapMap2Normal
   numComponents    = const 3
   peekControlPoint = peek
   pokeControlPoint = poke

instance ControlPoint TexCoord1 where
   map1Target       = const Map1TextureCoord1
   map2Target       = const Map2TextureCoord1
   enableCap1       = const CapMap1TextureCoord1
   enableCap2       = const CapMap2TextureCoord1
   numComponents    = const 1
   peekControlPoint = peek
   pokeControlPoint = poke

instance ControlPoint TexCoord2 where
   map1Target       = const Map1TextureCoord2
   map2Target       = const Map2TextureCoord2
   enableCap1       = const CapMap1TextureCoord2
   enableCap2       = const CapMap2TextureCoord2
   numComponents    = const 2
   peekControlPoint = peek
   pokeControlPoint = poke

instance ControlPoint TexCoord3 where
   map1Target       = const Map1TextureCoord3
   map2Target       = const Map2TextureCoord3
   enableCap1       = const CapMap1TextureCoord3
   enableCap2       = const CapMap2TextureCoord3
   numComponents    = const 3
   peekControlPoint = peek
   pokeControlPoint = poke

instance ControlPoint TexCoord4 where
   map1Target       = const Map1TextureCoord4
   map2Target       = const Map2TextureCoord4
   enableCap1       = const CapMap1TextureCoord4
   enableCap2       = const CapMap2TextureCoord4
   numComponents    = const 4
   peekControlPoint = peek
   pokeControlPoint = poke

--------------------------------------------------------------------------------

peekControlPoints1 ::
   (ControlPoint c, Domain d) => MapDescriptor d -> Ptr d -> IO [c d]
peekControlPoints1 descriptor ptr =
   mapM peekControlPoint (controlPointPtrs1 descriptor ptr)

peekControlPoints2 ::
      (ControlPoint c, Domain d)
   => MapDescriptor d -> MapDescriptor d -> Ptr d -> IO [[c d]]
peekControlPoints2 uDescriptor vDescriptor ptr =
   mapM (mapM peekControlPoint) (controlPointPtrs2 uDescriptor vDescriptor ptr)

pokeControlPoints1 ::
   (ControlPoint c, Domain d) => MapDescriptor d -> Ptr d -> [c d] -> IO ()
pokeControlPoints1 descriptor ptr =
   zipWithM_ pokeControlPoint (controlPointPtrs1 descriptor ptr)

pokeControlPoints2 ::
      (ControlPoint c, Domain d)
   => MapDescriptor d -> MapDescriptor d -> Ptr d -> [[c d]] -> IO ()
pokeControlPoints2 uDescriptor vDescriptor ptr =
   zipWithM_ (zipWithM_ pokeControlPoint)
             (controlPointPtrs2 uDescriptor vDescriptor ptr)

controlPointPtrs1 :: Domain d => MapDescriptor d -> Ptr d -> [Ptr a]
controlPointPtrs1 (MapDescriptor _ stride order _) ptr =
   [ ptr `plusPtr` (o * s) | o <- [ 0 .. fromIntegral order - 1 ] ]
   where s = sizeOfPtr ptr * fromIntegral stride

controlPointPtrs2 ::
   Domain d => MapDescriptor d -> MapDescriptor d -> Ptr d -> [[Ptr a]]
controlPointPtrs2 uDescriptor vDescriptor ptr =
   [ controlPointPtrs1 vDescriptor p | p <- controlPointPtrs1 uDescriptor ptr ]

sizeOfPtr :: Storable a => Ptr a -> Int
sizeOfPtr = (flip (const sizeOf) :: Storable a => Ptr a -> a -> Int) undefined

--------------------------------------------------------------------------------

data MapTarget =
     Map1Color4
   | Map1Index
   | Map1Normal
   | Map1TextureCoord1
   | Map1TextureCoord2
   | Map1TextureCoord3
   | Map1TextureCoord4
   | Map1Vertex3
   | Map1Vertex4
   | Map2Color4
   | Map2Index
   | Map2Normal
   | Map2TextureCoord1
   | Map2TextureCoord2
   | Map2TextureCoord3
   | Map2TextureCoord4
   | Map2Vertex3
   | Map2Vertex4

marshalMapTarget :: MapTarget -> GLenum
marshalMapTarget x = case x of
   Map1Color4 -> 0xd90
   Map1Index -> 0xd91
   Map1Normal -> 0xd92
   Map1TextureCoord1 -> 0xd93
   Map1TextureCoord2 -> 0xd94
   Map1TextureCoord3 -> 0xd95
   Map1TextureCoord4 -> 0xd96
   Map1Vertex3 -> 0xd97
   Map1Vertex4 -> 0xd98
   Map2Color4 -> 0xdb0
   Map2Index -> 0xdb1
   Map2Normal -> 0xdb2
   Map2TextureCoord1 -> 0xdb3
   Map2TextureCoord2 -> 0xdb4
   Map2TextureCoord3 -> 0xdb5
   Map2TextureCoord4 -> 0xdb6
   Map2Vertex3 -> 0xdb7
   Map2Vertex4 -> 0xdb8

--------------------------------------------------------------------------------

class Map1 m where
   withNewMap1 :: (ControlPoint c, Domain d)
      => MapDescriptor d -> (Ptr d -> IO ()) -> IO (m c d)

   withMap1 :: (ControlPoint c, Domain d)
      => m c d -> (MapDescriptor d -> Ptr d -> IO a) -> IO a

   newMap1 :: (ControlPoint c, Domain d)
      => (d, d) -> [c d] -> IO (m c d)

   getMap1Components :: (ControlPoint c, Domain d)
      => m c d -> IO ((d, d), [c d])

   withNewMap1 descriptor@(MapDescriptor domain _ _ _) act = do
      allocaArray (totalComponents1 descriptor) $ \ptr -> do
         act ptr
         controlPoints <- peekControlPoints1 descriptor ptr
         newMap1 domain controlPoints

   withMap1 m act = do
      (domain, controlPoints) <- getMap1Components m
      let stride = numComponents (head controlPoints)
          order = genericLength controlPoints
          descriptor = MapDescriptor domain stride order (fromIntegral stride)
      allocaArray (totalComponents1 descriptor) $ \ptr -> do
         pokeControlPoints1 descriptor ptr controlPoints
         act descriptor ptr

   newMap1 domain controlPoints = do
      let stride = numComponents (head controlPoints)
          order = genericLength controlPoints
          descriptor = MapDescriptor domain stride order (fromIntegral stride)
      withNewMap1 descriptor $ \ptr ->
         pokeControlPoints1 descriptor ptr controlPoints

   getMap1Components m =
      withMap1 m $ \descriptor@(MapDescriptor domain _ _ _) ptr -> do
         controlPoints <- peekControlPoints1 descriptor ptr
         return (domain, controlPoints)

--------------------------------------------------------------------------------

data (ControlPoint c, Domain d) => GLmap1 c d =
   GLmap1 (MapDescriptor d) (ForeignPtr d)
#ifdef __HADDOCK__
-- Help Haddock a bit, because it doesn't do any instance inference.
instance Eq d => Eq (GLmap1 c d)
instance Ord d => Ord (GLmap1 c d)
instance Show d => Show (GLmap1 c d)
#else
   deriving ( Eq, Ord, Show )
#endif

instance Map1 GLmap1 where
   withNewMap1 descriptor act = do
      fp <- mallocForeignPtrArray (totalComponents1 descriptor)
      withForeignPtr fp act
      return $ GLmap1 descriptor fp

   withMap1 (GLmap1 descriptor fp) act =
      withForeignPtr fp $ act descriptor

--------------------------------------------------------------------------------
 
map1 :: (Map1 m, ControlPoint c, Domain d) => StateVar (Maybe (m c d))
map1 = makeMap1StateVar enableCap1 getMap1 setMap1

makeMap1StateVar ::
      (c d -> EnableCap) -> (c d -> IO (m c d)) -> (c d -> m c d -> IO ())
   -> StateVar (Maybe (m c d))
makeMap1StateVar getCap getAct setAct =
   makeStateVarMaybe
      (return (getCap undefined))
      (getAct undefined)
      (setAct undefined)

getMap1 :: (Map1 m, ControlPoint c, Domain d) => c d -> IO (m c d)
getMap1 dummyControlPoint = do
   let target = marshalMapTarget (map1Target dummyControlPoint)
       numComp = fromIntegral (numComponents dummyControlPoint)
   domain <- allocaArray 2 $ \ptr -> do
      glGetMapv target (marshalGetMapQuery Domain) ptr
      peek2 (,) ptr
   order <- alloca $ \ptr -> do
      glGetMapiv target (marshalGetMapQuery Order) ptr
      liftM fromIntegral $ peek ptr
   withNewMap1 (MapDescriptor domain (numComponents dummyControlPoint) order numComp) $
      glGetMapv target (marshalGetMapQuery Coeff)

setMap1 :: (Map1 m, ControlPoint c, Domain d) => c d -> m c d -> IO ()
setMap1 dummyControlPoint m =
   withMap1 m $ \(MapDescriptor (u1, u2) stride order _) ->
      glMap1 (marshalMapTarget (map1Target dummyControlPoint)) u1 u2
             (fromIntegral stride) (fromIntegral order)

foreign import CALLCONV unsafe "glMap1f" glMap1f ::
      GLenum
   -> GLfloat -> GLfloat -> GLint -> GLint
   -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glMap1d" glMap1d ::
      GLenum
   -> GLdouble -> GLdouble -> GLint -> GLint
   -> Ptr GLdouble -> IO ()

--------------------------------------------------------------------------------

class Map2 m where
   withNewMap2 :: (ControlPoint c, Domain d)
      => MapDescriptor d -> MapDescriptor d -> (Ptr d -> IO ()) -> IO (m c d)

   withMap2 :: (ControlPoint c, Domain d)
      => m c d -> (MapDescriptor d -> MapDescriptor d -> Ptr d -> IO a) -> IO a

   newMap2 :: (ControlPoint c, Domain d)
      => (d, d) -> (d, d) -> [[c d]] -> IO (m c d)

   getMap2Components :: (ControlPoint c, Domain d)
      => m c d -> IO ((d, d), (d, d), [[c d]])

   withNewMap2 uDescriptor@(MapDescriptor uDomain _ _ _)
               vDescriptor@(MapDescriptor vDomain _ _ _) act =
      allocaArray (totalComponents2 uDescriptor vDescriptor) $ \ptr -> do
         act ptr
         controlPoints <- peekControlPoints2 uDescriptor vDescriptor ptr
         newMap2 uDomain vDomain controlPoints

   withMap2 m act = do
      (uDomain, vDomain, controlPoints) <- getMap2Components m
      let vStride = numComponents (head (head controlPoints))
          vOrder = genericLength (head controlPoints)
          uStride = vStride * fromIntegral vOrder
          uOrder = genericLength controlPoints
          numComp = fromIntegral vStride
          uDescriptor = MapDescriptor uDomain uStride uOrder numComp
          vDescriptor = MapDescriptor vDomain vStride vOrder numComp
      allocaArray (totalComponents2 uDescriptor vDescriptor) $ \ptr -> do
         pokeControlPoints2 uDescriptor vDescriptor ptr controlPoints
         act uDescriptor vDescriptor ptr

   newMap2 uDomain vDomain controlPoints = do
      let vStride = numComponents (head (head controlPoints))
          vOrder = genericLength (head controlPoints)
          uStride = vStride * fromIntegral vOrder
          uOrder = genericLength controlPoints
          numComp = fromIntegral vStride
          uDescriptor = MapDescriptor uDomain uStride uOrder numComp
          vDescriptor = MapDescriptor vDomain vStride vOrder numComp
      withNewMap2 uDescriptor vDescriptor $ \ptr ->
         pokeControlPoints2 uDescriptor vDescriptor ptr controlPoints

   getMap2Components m =
      withMap2 m $ \uDescriptor@(MapDescriptor uDomain _ _ _)
                    vDescriptor@(MapDescriptor vDomain _ _ _) ptr -> do
         controlPoints <- peekControlPoints2 uDescriptor vDescriptor ptr
         return (uDomain, vDomain, controlPoints)

--------------------------------------------------------------------------------

data (ControlPoint c, Domain d) => GLmap2 c d =
   GLmap2 (MapDescriptor d)  (MapDescriptor d) (ForeignPtr d)
#ifdef __HADDOCK__
-- Help Haddock a bit, because it doesn't do any instance inference.
instance Eq d => Eq (GLmap2 c d)
instance Ord d => Ord (GLmap2 c d)
instance Show d => Show (GLmap2 c d)
#else
   deriving ( Eq, Ord, Show )
#endif

instance Map2 GLmap2 where
   withNewMap2 uDescriptor vDescriptor act = do
      fp <- mallocForeignPtrArray (totalComponents2 uDescriptor vDescriptor)
      withForeignPtr fp act
      return $ GLmap2 uDescriptor vDescriptor fp

   withMap2 (GLmap2 uDescriptor vDescriptor fp) act =
      withForeignPtr fp $ act uDescriptor vDescriptor

--------------------------------------------------------------------------------

map2 :: (Map2 m, ControlPoint c, Domain d) => StateVar (Maybe (m c d))
map2 = makeMap2StateVar enableCap2 getMap2 setMap2

makeMap2StateVar ::
      (c d -> EnableCap) -> (c d -> IO (m c d)) -> (c d -> m c d -> IO ())
   -> StateVar (Maybe (m c d))
makeMap2StateVar getCap getAct setAct =
   makeStateVarMaybe
      (return (getCap undefined))
      (getAct undefined)
      (setAct undefined)

getMap2 :: (Map2 m, ControlPoint c, Domain d) => c d -> IO (m c d)
getMap2 dummyControlPoint = do
   let target = marshalMapTarget (map2Target dummyControlPoint)
   (uDomain, vDomain) <- allocaArray 4 $ \ptr -> do
      glGetMapv target (marshalGetMapQuery Domain) ptr
      peek4 (\u1 u2 v1 v2 -> ((u1, u2), (v1, v2))) ptr
   (uOrder, vOrder) <- allocaArray 2 $ \ptr -> do
      glGetMapiv target (marshalGetMapQuery Order) ptr
      peek2 (,) ptr
   let vStride = numComponents dummyControlPoint
       uStride = vStride * fromIntegral vOrder
   withNewMap2 (MapDescriptor uDomain uStride uOrder (fromIntegral vStride))
               (MapDescriptor vDomain vStride vOrder (fromIntegral vStride)) $
      glGetMapv target (marshalGetMapQuery Coeff)

setMap2 :: (Map2 m, ControlPoint c, Domain d) => c d -> m c d -> IO ()
setMap2 dummyControlPoint m =
   withMap2 m $ \(MapDescriptor (u1, u2) uStride uOrder _)
                 (MapDescriptor (v1, v2) vStride vOrder _) ->
      glMap2 (marshalMapTarget (map2Target dummyControlPoint))
             u1 u2 (fromIntegral uStride) (fromIntegral uOrder)
             v1 v2 (fromIntegral vStride) (fromIntegral vOrder)

foreign import CALLCONV unsafe "glMap2f" glMap2f ::
      GLenum
   -> GLfloat -> GLfloat -> GLint -> GLint
   -> GLfloat -> GLfloat -> GLint -> GLint
   -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glMap2d" glMap2d ::
      GLenum
   -> GLdouble -> GLdouble -> GLint -> GLint
   -> GLdouble -> GLdouble -> GLint -> GLint
   -> Ptr GLdouble -> IO ()

--------------------------------------------------------------------------------

data GetMapQuery =
     Coeff
   | Order
   | Domain

marshalGetMapQuery :: GetMapQuery -> GLenum
marshalGetMapQuery x = case x of
   Coeff -> 0xa00
   Order -> 0xa01
   Domain -> 0xa02

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glGetMapfv" glGetMapfv ::
   GLenum -> GLenum -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glGetMapdv" glGetMapdv ::
   GLenum -> GLenum -> Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glGetMapiv" glGetMapiv ::
   GLenum -> GLenum -> Ptr GLint -> IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glEvalCoord1f" glEvalCoord1f ::
   GLfloat -> IO ()

foreign import CALLCONV unsafe "glEvalCoord1fv" glEvalCoord1fv ::
   Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glEvalCoord1d" glEvalCoord1d ::
   GLdouble -> IO ()

foreign import CALLCONV unsafe "glEvalCoord1dv" glEvalCoord1dv ::
   Ptr GLdouble -> IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glEvalCoord2f" glEvalCoord2f ::
   GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glEvalCoord2fv" glEvalCoord2fv ::
   Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glEvalCoord2d" glEvalCoord2d ::
   GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glEvalCoord2dv" glEvalCoord2dv ::
   Ptr GLdouble -> IO ()

--------------------------------------------------------------------------------

mapGrid1 :: Domain d => StateVar (GLint, (d, d))
mapGrid1 =
   makeStateVar
      (do n <- getInteger1 id GetMap1GridSegments
          domain <- get2 (,) GetMap1GridDomain
          return (n, domain))
      (\(n, (u1, u2)) -> glMapGrid1 n u1 u2)

foreign import CALLCONV unsafe "glMapGrid1f" glMapGrid1f ::
   GLint -> GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glMapGrid1d" glMapGrid1d ::
   GLint -> GLdouble -> GLdouble -> IO ()

mapGrid2 :: Domain d => StateVar ((GLint, (d, d)), (GLint, (d, d)))
mapGrid2 =
   makeStateVar
      (do (un, vn) <- getInteger2 (,) GetMap2GridSegments
          (u1, u2, v1, v2) <- get4 (,,,) GetMap2GridDomain
          return ((un, (u1, u2)), (vn, (v1, v2))))
      (\((un, (u1, u2)), (vn, (v1, v2))) -> glMapGrid2 un u1 u2 vn v1 v2)

foreign import CALLCONV unsafe "glMapGrid2f" glMapGrid2f ::
   GLint -> GLfloat -> GLfloat -> GLint -> GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glMapGrid2d" glMapGrid2d ::
   GLint -> GLdouble -> GLdouble -> GLint -> GLdouble -> GLdouble -> IO ()

--------------------------------------------------------------------------------

evalMesh1 :: PolygonMode -> (GLint, GLint) -> IO ()
evalMesh1 m (p1, p2) = glEvalMesh1 (marshalPolygonMode m) p1 p2

foreign import CALLCONV unsafe "glEvalMesh1" glEvalMesh1 ::
   GLenum -> GLint -> GLint -> IO ()

evalMesh2 :: PolygonMode -> (GLint, GLint) -> (GLint, GLint) -> IO ()
evalMesh2 m (p1, p2) (q1, q2) = glEvalMesh2 (marshalPolygonMode m) p1 p2 q1 q2

foreign import CALLCONV unsafe "glEvalMesh2" glEvalMesh2 ::
   GLenum -> GLint -> GLint -> GLint -> GLint -> IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glEvalPoint1" evalPoint1 ::
   GLint -> IO ()

evalPoint2 :: (GLint, GLint) -> IO ()
evalPoint2 = uncurry glEvalPoint2

foreign import CALLCONV unsafe "glEvalPoint2" glEvalPoint2 ::
   GLint -> GLint -> IO ()

--------------------------------------------------------------------------------

autoNormal :: StateVar Capability
autoNormal = makeCapability CapAutoNormal
