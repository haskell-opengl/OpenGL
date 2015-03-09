--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Fog
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 3.10 (Fog) of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Fog (
   fog,
   FogMode(..), fogMode,
   fogColor, fogIndex,
   FogCoordSrc(..), fogCoordSrc,
   FogDistanceMode(..), fogDistanceMode
) where

import Data.StateVar
import Foreign.Marshal.Utils
import Foreign.Ptr
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

fog :: StateVar Capability
fog = makeCapability CapFog

--------------------------------------------------------------------------------

data FogParameter =
     FogIndex
   | FogDensity
   | FogStart
   | FogEnd
   | FogMode
   | FogColor
   | FogCoordSrc
   | FogDistanceMode

marshalFogParameter :: FogParameter -> GLenum
marshalFogParameter x = case x of
   FogIndex -> gl_FOG_INDEX
   FogDensity -> gl_FOG_DENSITY
   FogStart -> gl_FOG_START
   FogEnd -> gl_FOG_END
   FogMode -> gl_FOG_MODE
   FogColor -> gl_FOG_COLOR
   FogCoordSrc -> gl_FOG_COORD_SRC
   FogDistanceMode -> gl_FOG_DISTANCE_MODE_NV

--------------------------------------------------------------------------------

data FogMode' =
     Linear'
   | Exp'
   | Exp2'

marshalFogMode' :: FogMode' -> GLint
marshalFogMode' x = fromIntegral $ case x of
   Linear' -> gl_LINEAR
   Exp' -> gl_EXP
   Exp2' -> gl_EXP2

unmarshalFogMode' :: GLint -> FogMode'
unmarshalFogMode' x
   | y == gl_LINEAR = Linear'
   | y == gl_EXP = Exp'
   | y == gl_EXP2 = Exp2'
   | otherwise = error ("unmarshalFogMode': illegal value " ++ show x)
   where y = fromIntegral x

--------------------------------------------------------------------------------

data FogMode =
     Linear GLfloat GLfloat
   | Exp GLfloat
   | Exp2 GLfloat
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

fogMode :: StateVar FogMode
fogMode = makeStateVar getFogMode setFogMode

getFogMode :: IO FogMode
getFogMode = do
   mode <- getInteger1 unmarshalFogMode' GetFogMode
   case mode of
      Linear' -> do
         start <- getFloat1 id GetFogStart
         end <- getFloat1 id GetFogEnd
         return $ Linear start end
      Exp' -> getFloat1 Exp GetFogDensity
      Exp2' -> getFloat1 Exp2 GetFogDensity

setFogMode :: FogMode -> IO ()
setFogMode (Linear start end) = do
   fogi FogMode (marshalFogMode' Linear')
   fogf FogStart start
   fogf FogEnd end
setFogMode (Exp density) = do
   fogi FogMode (marshalFogMode' Exp')
   fogf FogDensity density
setFogMode (Exp2 density) = do
   fogi FogMode (marshalFogMode' Exp2')
   fogf FogDensity density

--------------------------------------------------------------------------------

fogi :: FogParameter -> GLint -> IO ()
fogi = glFogi . marshalFogParameter

fogf :: FogParameter -> GLfloat -> IO ()
fogf = glFogf . marshalFogParameter

fogfv :: FogParameter -> Ptr (Color4 GLfloat) -> IO ()
fogfv param ptr = glFogfv (marshalFogParameter param) (castPtr ptr)

--------------------------------------------------------------------------------

fogColor :: StateVar (Color4 GLclampf)
fogColor =
   makeStateVar
      (getClampf4 Color4 GetFogColor)
      (\c -> with c $ (fogfv FogColor . castPtr))

--------------------------------------------------------------------------------

fogIndex :: StateVar (Index1 GLint)
fogIndex =
   makeStateVar
      (getInteger1 Index1 GetFogIndex)
      (\(Index1 i) -> fogi FogIndex i)

--------------------------------------------------------------------------------

data FogCoordSrc =
     FogCoord
   | FragmentDepth
   deriving ( Eq, Ord, Show )

marshalFogCoordSrc :: FogCoordSrc -> GLint
marshalFogCoordSrc x = fromIntegral $ case x of
   FogCoord -> gl_FOG_COORD
   FragmentDepth -> gl_FRAGMENT_DEPTH

unmarshalFogCoordSrc :: GLint -> FogCoordSrc
unmarshalFogCoordSrc x
   | y == gl_FOG_COORD = FogCoord
   | y == gl_FRAGMENT_DEPTH = FragmentDepth
   | otherwise = error ("unmarshalFogCoordSrc: illegal value " ++ show x)
   where y = fromIntegral x

--------------------------------------------------------------------------------

fogCoordSrc :: StateVar FogCoordSrc
fogCoordSrc =
   makeStateVar
      (getInteger1 unmarshalFogCoordSrc GetFogCoordSrc)
      (fogi FogCoordSrc . marshalFogCoordSrc)

--------------------------------------------------------------------------------

data FogDistanceMode =
     EyeRadial
   | EyePlaneSigned
   | EyePlaneAbsolute
   deriving ( Eq, Ord, Show )

marshalFogDistanceMode :: FogDistanceMode -> GLint
marshalFogDistanceMode x = fromIntegral $ case x of
   EyeRadial -> gl_EYE_RADIAL_NV
   EyePlaneSigned ->gl_EYE_PLANE
   EyePlaneAbsolute -> gl_EYE_PLANE_ABSOLUTE_NV

unmarshalFogDistanceMode :: GLint -> FogDistanceMode
unmarshalFogDistanceMode x
   | y == gl_EYE_RADIAL_NV = EyeRadial
   | y == gl_EYE_PLANE = EyePlaneSigned
   | y == gl_EYE_PLANE_ABSOLUTE_NV = EyePlaneAbsolute
   | otherwise = error ("unmarshalFogDistanceMode: illegal value " ++ show x)
   where y = fromIntegral x

--------------------------------------------------------------------------------

fogDistanceMode :: StateVar FogDistanceMode
fogDistanceMode =
   makeStateVar
      (getInteger1 unmarshalFogDistanceMode GetFogDistanceMode)
      (fogi FogDistanceMode . marshalFogDistanceMode)
