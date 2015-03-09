{-# LANGUAGE KindSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Evaluators
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 5.1 (Evaluators) of the OpenGL 2.1 specs.
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

import Control.Monad
import Data.List
import Data.StateVar
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.ControlPoint
import Graphics.Rendering.OpenGL.GL.Domain
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.PolygonMode
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.VertexArrays
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

type Order = GLint

maxOrder :: GettableStateVar Order
maxOrder = makeGettableStateVar (getInteger1 id GetMaxEvalOrder)

--------------------------------------------------------------------------------

data MapDescriptor d =
   MapDescriptor (d, d) Stride Order NumComponents
   deriving ( Eq, Ord, Show )

totalComponents1 :: Domain d => MapDescriptor d -> Int
totalComponents1 (MapDescriptor _ stride order numComp) =
   fromIntegral stride * (fromIntegral order - 1) + fromIntegral numComp

totalComponents2 :: Domain d => MapDescriptor d -> MapDescriptor d -> Int
totalComponents2 uDescriptor vDescriptor@(MapDescriptor _ _ _ numComp) =
   totalComponents1 uDescriptor + totalComponents1 vDescriptor - fromIntegral numComp

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
sizeOfPtr = sizeOf . (const undefined :: Ptr a -> a)

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

data GLmap1 (c :: * -> *) d =
   GLmap1 (MapDescriptor d) (ForeignPtr d)
   deriving ( Eq, Ord, Show )

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
   let target = map1Target dummyControlPoint
       numComp = fromIntegral (numComponents dummyControlPoint)
   domain <- allocaArray 2 $ \ptr -> do
      glGetMapv target (marshalGetMapQuery Domain) ptr
      peek2 (,) ptr
   order <- with 0 $ \ptr -> do
      glGetMapiv target (marshalGetMapQuery Order) ptr
      fmap fromIntegral $ peek ptr
   withNewMap1 (MapDescriptor domain (numComponents dummyControlPoint) order numComp) $
      glGetMapv target (marshalGetMapQuery Coeff)

setMap1 :: (Map1 m, ControlPoint c, Domain d) => c d -> m c d -> IO ()
setMap1 dummyControlPoint m =
   withMap1 m $ \(MapDescriptor (u1, u2) stride order _) ->
      glMap1 (map1Target dummyControlPoint) u1 u2
             (fromIntegral stride) (fromIntegral order)

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

data GLmap2 (c :: * -> *) d =
   GLmap2 (MapDescriptor d)  (MapDescriptor d) (ForeignPtr d)
   deriving ( Eq, Ord, Show )

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
   let target = map2Target dummyControlPoint
   (uDomain, vDomain) <- allocaArray 4 $ \ptr -> do
      glGetMapv target (marshalGetMapQuery Domain) ptr
      peek4 (\u1 u2 v1 v2 -> ((u1, u2), (v1, v2))) ptr
   (uOrder, vOrder) <- withArray [0,0] $ \ptr -> do
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
      glMap2 (map2Target dummyControlPoint)
             u1 u2 (fromIntegral uStride) (fromIntegral uOrder)
             v1 v2 (fromIntegral vStride) (fromIntegral vOrder)

--------------------------------------------------------------------------------

data GetMapQuery =
     Coeff
   | Order
   | Domain

marshalGetMapQuery :: GetMapQuery -> GLenum
marshalGetMapQuery x = case x of
   Coeff -> gl_COEFF
   Order -> gl_ORDER
   Domain -> gl_DOMAIN

--------------------------------------------------------------------------------

mapGrid1 :: Domain d => StateVar (GLint, (d, d))
mapGrid1 =
   makeStateVar
      (do n <- getInteger1 id GetMap1GridSegments
          domain <- get2 (,) GetMap1GridDomain
          return (n, domain))
      (\(n, (u1, u2)) -> glMapGrid1 n u1 u2)

mapGrid2 :: Domain d => StateVar ((GLint, (d, d)), (GLint, (d, d)))
mapGrid2 =
   makeStateVar
      (do (un, vn) <- getInteger2 (,) GetMap2GridSegments
          (u1, u2, v1, v2) <- get4 (,,,) GetMap2GridDomain
          return ((un, (u1, u2)), (vn, (v1, v2))))
      (\((un, (u1, u2)), (vn, (v1, v2))) -> glMapGrid2 un u1 u2 vn v1 v2)

--------------------------------------------------------------------------------

evalMesh1 :: PolygonMode -> (GLint, GLint) -> IO ()
evalMesh1 m (p1, p2) = glEvalMesh1 (marshalPolygonMode m) p1 p2

evalMesh2 :: PolygonMode -> (GLint, GLint) -> (GLint, GLint) -> IO ()
evalMesh2 m (p1, p2) (q1, q2) = glEvalMesh2 (marshalPolygonMode m) p1 p2 q1 q2

--------------------------------------------------------------------------------

evalPoint1 :: GLint -> IO ()
evalPoint1 = glEvalPoint1

evalPoint2 :: (GLint, GLint) -> IO ()
evalPoint2 = uncurry glEvalPoint2

--------------------------------------------------------------------------------

autoNormal :: StateVar Capability
autoNormal = makeCapability CapAutoNormal
