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
   -- * Defining Evaluator Maps
   map1Vertex, map1Index, map1Color, map1Normal, map1TexCoord,
   map2Vertex, map2Index, map2Color, map2Normal, map2TexCoord,

   -- * Using Evaluator Maps

   -- ** Evaluating an Arbitrary Coordinate Value
   EvalCoord1(..), EvalCoord2(..),

   -- ** Using Evenly Spaced Coordinate Values

   -- *** Defining a Grid
   Grid(..),

   -- *** Evaluating a Whole Mesh
   evalMesh1, evalMesh2,

   -- *** Evaluating a Single Point on a Mesh
   evalPoint1, evalPoint2,

   -- * Evaluator Limits
   maxEvalOrder,

   -- * Normal Generation
   autoNormal
) where

import Control.Monad ( liftM )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( allocaArray, withArray, peekArray, pokeArray )
import Foreign.Ptr ( Ptr, castPtr, plusPtr )
import Foreign.Storable ( Storable(peek,sizeOf) )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapAutoNormal,CapMap1Color4,CapMap1Index,CapMap1Normal,
             CapMap1TextureCoord1,CapMap1TextureCoord2,CapMap1TextureCoord3,
             CapMap1TextureCoord4,CapMap1Vertex3,CapMap1Vertex4,CapMap2Color4,
             CapMap2Index,CapMap2Normal,CapMap2TextureCoord1,
             CapMap2TextureCoord2,CapMap2TextureCoord3,CapMap2TextureCoord4,
             CapMap2Vertex3,CapMap2Vertex4),
   makeCapability, makeStateVarMaybe )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLint, GLsizei, GLfloat, GLdouble, Capability )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek2, peek4 )
import Graphics.Rendering.OpenGL.GL.PrimitiveMode ( marshalPrimitiveMode )
import Graphics.Rendering.OpenGL.GL.BeginEnd ( PrimitiveMode )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetMaxEvalOrder,
            GetMap1GridSegments,GetMap1GridDomain,
            GetMap2GridSegments,GetMap2GridDomain),
   getSizei1, getInteger1, getInteger2, getFloat2, getFloat4, getDouble2,
   getDouble4 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.VertexSpec (
   Vertex4(..), TexCoord4(..), Normal3(..), Color4(..), Index1(..) )

--------------------------------------------------------------------------------

#include "HsOpenGLTypes.h"

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

mapTargetToEnableCap :: MapTarget -> EnableCap
mapTargetToEnableCap x = case x of
   Map1Color4 -> CapMap1Color4
   Map1Index -> CapMap1Index
   Map1Normal -> CapMap1Normal
   Map1TextureCoord1 -> CapMap1TextureCoord1
   Map1TextureCoord2 -> CapMap1TextureCoord2
   Map1TextureCoord3 -> CapMap1TextureCoord3
   Map1TextureCoord4 -> CapMap1TextureCoord4
   Map1Vertex3 -> CapMap1Vertex3
   Map1Vertex4 -> CapMap1Vertex4
   Map2Color4 -> CapMap2Color4
   Map2Index -> CapMap2Index
   Map2Normal -> CapMap2Normal
   Map2TextureCoord1 -> CapMap2TextureCoord1
   Map2TextureCoord2 -> CapMap2TextureCoord2
   Map2TextureCoord3 -> CapMap2TextureCoord3
   Map2TextureCoord4 -> CapMap2TextureCoord4
   Map2Vertex3 -> CapMap2Vertex3
   Map2Vertex4 -> CapMap2Vertex4

--------------------------------------------------------------------------------

map1Vertex :: StateVar (Maybe ((GLfloat, GLfloat), [Vertex4 GLfloat]))
map1Vertex = makeMap1 Map1Vertex4

map1TexCoord :: StateVar (Maybe ((GLfloat, GLfloat), [TexCoord4 GLfloat]))
map1TexCoord = makeMap1 Map1TextureCoord4

map1Normal :: StateVar (Maybe ((GLfloat, GLfloat), [Normal3 GLfloat]))
map1Normal = makeMap1 Map1Normal

map1Color :: StateVar (Maybe ((GLfloat, GLfloat), [Color4 GLfloat]))
map1Color = makeMap1 Map1Color4

map1Index :: StateVar (Maybe ((GLfloat, GLfloat), [Index1 GLint]))
map1Index = makeMap1 Map1Index

--------------------------------------------------------------------------------

makeMap1 ::
   Storable a => MapTarget -> StateVar (Maybe ((GLfloat, GLfloat), [a]))
makeMap1 target =
   makeStateVarMaybe
      (return (mapTargetToEnableCap target))
      (getMap1 target)
      (setMap1 target)

getMap1 :: Storable a => MapTarget -> IO ((GLfloat, GLfloat), [a])
getMap1 target = do
   domain <- allocaArray 2 $ \buf -> do
      getMapf target Domain buf
      peek2 (,) buf
   order <- alloca $ \buf -> do
      getMapi target Order buf
      liftM fromIntegral $ peek buf
   elements <- allocaArray order $ \buf -> do
      getMapf target Coeff (castPtr buf)
      peekArray order buf
   return (domain, elements)

setMap1 :: Storable a => MapTarget -> ((GLfloat, GLfloat), [a]) ->  IO ()
setMap1 target ((u1, u2), elements) = do
   let uOrder = fromIntegral (length elements)
   withArray elements $
      glMap1f (marshalMapTarget target) u1 u2 uOrder uOrder . castPtr

foreign import CALLCONV unsafe "glMap1f" glMap1f ::
      GLenum
   -> GLfloat -> GLfloat -> GLint -> GLint
   -> Ptr GLfloat -> IO ()

--------------------------------------------------------------------------------

map2Vertex ::
   StateVar (Maybe ((GLfloat, GLfloat), (GLfloat, GLfloat), [[Vertex4 GLfloat]]))
map2Vertex = makeMap2 Map2Vertex4

map2TexCoord ::
   StateVar (Maybe ((GLfloat, GLfloat), (GLfloat, GLfloat), [[TexCoord4 GLfloat]]))
map2TexCoord = makeMap2 Map2TextureCoord4

map2Normal ::
   StateVar (Maybe ((GLfloat, GLfloat), (GLfloat, GLfloat), [[Normal3 GLfloat]]))
map2Normal = makeMap2 Map2Normal

map2Color ::
   StateVar (Maybe ((GLfloat, GLfloat), (GLfloat, GLfloat), [[Color4 GLfloat]]))
map2Color = makeMap2 Map2Color4

map2Index ::
   StateVar (Maybe ((GLfloat, GLfloat), (GLfloat, GLfloat), [[Index1 GLint]]))
map2Index = makeMap2 Map2Index

--------------------------------------------------------------------------------

makeMap2 ::
      Storable a
   => MapTarget -> StateVar (Maybe ((GLfloat, GLfloat), (GLfloat, GLfloat), [[a]]))
makeMap2 target =
   makeStateVarMaybe
      (return (mapTargetToEnableCap target))
      (getMap2 target)
      (setMap2 target)

getMap2 ::
      Storable a
   => MapTarget -> IO ((GLfloat, GLfloat), (GLfloat, GLfloat), [[a]])
getMap2 target = do
   (uDomain, vDomain) <- allocaArray 4 $ \buf -> do
      getMapf target Domain buf
      peek4 (\u1 u2 v1 v2 -> ((u1, u2), (v1, v2))) buf
   (uOrder, vOrder) <- allocaArray 2 $ \buf -> do
      getMapi target Order buf
      peek2 (\uo vo -> ((fromIntegral :: GLint -> Int) uo,
                        (fromIntegral :: GLint -> Int) vo))
            buf
   elements <- allocaArray (uOrder * vOrder) $ \buf -> do
      getMapf target Coeff (castPtr buf)
      peekArray2 uOrder vOrder buf
   return (uDomain, vDomain, elements)

peekArray2 :: Storable a => Int -> Int -> Ptr a -> IO [[a]]
peekArray2 numRows numColumns ptr | numRows <= 0 = return []
                                  | otherwise  = f (numRows - 1) []
  where f 0 acc = do e <- peekArray numColumns ptr
                     return (e:acc)
        f n acc = do e <- peekArray numColumns (ptr `addPtr` (n * numColumns))
                     f (n - 1) (e:acc)

addPtr :: Storable a => Ptr a -> Int -> Ptr a
addPtr = doAddPtr undefined
   where doAddPtr :: Storable a => a -> Ptr a -> Int -> Ptr a
         doAddPtr dummy ptr n = plusPtr ptr (n * sizeOf dummy)
setMap2 ::
      Storable a
   => MapTarget -> ((GLfloat, GLfloat), (GLfloat, GLfloat), [[a]]) ->  IO ()
setMap2 target ((u1, u2), (v1, v2), elements) = do
   let uOrder = length elements
       vOrder = maximum $ map length elements
   allocaArray (uOrder * vOrder) $ \buf -> do
      pokeArray buf (concat elements)
      glMap2f (marshalMapTarget target)
              u1 u2 (fromIntegral (uOrder * vOrder)) (fromIntegral uOrder)
              v1 v2 (fromIntegral vOrder)            (fromIntegral vOrder) $
              (castPtr buf)

foreign import CALLCONV unsafe "glMap2f" glMap2f ::
      GLenum
   -> GLfloat -> GLfloat -> GLint -> GLint
   -> GLfloat -> GLfloat -> GLint -> GLint
   -> Ptr GLfloat -> IO ()

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

getMapf :: MapTarget -> GetMapQuery -> Ptr GLfloat -> IO ()
getMapf target = glGetMapfv (marshalMapTarget target) . marshalGetMapQuery

foreign import CALLCONV unsafe "glGetMapfv" glGetMapfv ::
   GLenum -> GLenum -> Ptr GLfloat -> IO ()

getMapd :: MapTarget -> GetMapQuery -> Ptr GLdouble -> IO ()
getMapd target = glGetMapdv (marshalMapTarget target) . marshalGetMapQuery

foreign import CALLCONV unsafe "glGetMapdv" glGetMapdv ::
   GLenum -> GLenum -> Ptr GLdouble -> IO ()

getMapi :: MapTarget -> GetMapQuery -> Ptr GLint -> IO ()
getMapi target = glGetMapiv (marshalMapTarget target) . marshalGetMapQuery

foreign import CALLCONV unsafe "glGetMapiv" glGetMapiv ::
   GLenum -> GLenum -> Ptr GLint -> IO ()

--------------------------------------------------------------------------------

class EvalCoord1 c where
    evalCoord1  :: c -> IO ()
    evalCoord1v :: Ptr c -> IO ()

instance EvalCoord1 GLfloat_ where
    evalCoord1  = glEvalCoord1f
    evalCoord1v = glEvalCoord1fv

foreign import CALLCONV unsafe "glEvalCoord1f" glEvalCoord1f ::
   GLfloat -> IO ()

foreign import CALLCONV unsafe "glEvalCoord1fv" glEvalCoord1fv ::
   Ptr GLfloat -> IO ()

instance EvalCoord1 GLdouble_ where
    evalCoord1  = glEvalCoord1d
    evalCoord1v = glEvalCoord1dv

foreign import CALLCONV unsafe "glEvalCoord1d" glEvalCoord1d ::
   GLdouble -> IO ()

foreign import CALLCONV unsafe "glEvalCoord1dv" glEvalCoord1dv ::
   Ptr GLdouble -> IO ()

--------------------------------------------------------------------------------

class EvalCoord2 c where
    evalCoord2  :: c -> c -> IO ()
    evalCoord2v :: Ptr c -> IO ()

instance EvalCoord2 GLfloat_ where
    evalCoord2  = glEvalCoord2f
    evalCoord2v = glEvalCoord2fv

foreign import CALLCONV unsafe "glEvalCoord2f" glEvalCoord2f ::
   GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glEvalCoord2fv" glEvalCoord2fv ::
   Ptr GLfloat -> IO ()

instance EvalCoord2 GLdouble_ where
    evalCoord2  = glEvalCoord2d
    evalCoord2v = glEvalCoord2dv

foreign import CALLCONV unsafe "glEvalCoord2d" glEvalCoord2d ::
   GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glEvalCoord2dv" glEvalCoord2dv ::
   Ptr GLdouble -> IO ()

--------------------------------------------------------------------------------

class Grid a where
   map1Grid :: StateVar (GLint, (a, a))
   map2Grid :: StateVar ((GLint, (a, a)), (GLint, (a, a)))

instance Grid GLfloat_ where
   map1Grid = map1GridX getFloat2 glMapGrid1f
   map2Grid = map2GridX getFloat4 glMapGrid2f

foreign import CALLCONV unsafe "glMapGrid1f" glMapGrid1f ::
   GLint -> GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glMapGrid2f" glMapGrid2f ::
   GLint -> GLfloat -> GLfloat -> GLint -> GLfloat -> GLfloat -> IO ()

instance Grid GLdouble_ where
   map1Grid = map1GridX getDouble2 glMapGrid1d
   map2Grid = map2GridX getDouble4 glMapGrid2d

foreign import CALLCONV unsafe "glMapGrid1d" glMapGrid1d ::
   GLint -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glMapGrid2d" glMapGrid2d ::
   GLint -> GLdouble -> GLdouble -> GLint -> GLdouble -> GLdouble -> IO ()

map1GridX ::
      ((a -> a -> (a, a)) -> GetPName -> IO (a, a))
   -> (GLint -> a -> a -> IO ())
   -> StateVar (GLint, (a, a))
map1GridX get2 glMapGrid1 =
   makeStateVar
      (do n <- getInteger1 id GetMap1GridSegments
          domain <- get2 (,) GetMap1GridDomain
          return (n, domain))
      (\(n, (u1, u2)) -> glMapGrid1 n u1 u2)

map2GridX ::
      ((a -> a -> a -> a -> (a, a, a, a)) -> GetPName -> IO (a, a, a, a))
   -> (GLint -> a -> a -> GLint -> a -> a -> IO ())
   -> StateVar ((GLint, (a, a)), (GLint, (a, a)))
map2GridX get4 glMapGrid2 =
   makeStateVar
      (do (un, vn) <- getInteger2 (,) GetMap2GridSegments
          (u1, u2, v1, v2) <- get4 (,,,) GetMap2GridDomain
          return ((un, (u1, u2)), (vn, (v1, v2))))
      (\((un, (u1, u2)), (vn, (v1, v2))) -> glMapGrid2 un u1 u2 vn v1 v2)

--------------------------------------------------------------------------------

evalMesh1 :: PrimitiveMode -> (GLint, GLint) -> IO ()
evalMesh1 m (p1, p2) = glEvalMesh1 (marshalPrimitiveMode m) p1 p2

foreign import CALLCONV unsafe "glEvalMesh1" glEvalMesh1 ::
   GLenum -> GLint -> GLint -> IO ()

evalMesh2 :: PrimitiveMode -> (GLint, GLint) -> (GLint, GLint) -> IO ()
evalMesh2 m (p1, p2) (q1, q2) = glEvalMesh2 (marshalPrimitiveMode m) p1 p2 q1 q2

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

maxEvalOrder :: GettableStateVar GLsizei
maxEvalOrder = makeGettableStateVar (getSizei1 id GetMaxEvalOrder)

--------------------------------------------------------------------------------

autoNormal :: StateVar Capability
autoNormal = makeCapability CapAutoNormal
