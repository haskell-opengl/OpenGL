--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Fog
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 3.10 (Fog) of the OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Fog (
   fog,
   FogMode(..), fogMode,
   fogColor, fogIndex,
   FogCoordSrc(..), fogCoordSrc,
   FogDistanceMode(..), fogDistanceMode
) where

import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapFog), makeCapability )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLint, GLfloat, Capability )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetFogIndex,GetFogDensity,GetFogStart,GetFogEnd,GetFogMode,
   GetFogColor,GetFogCoordSrc,GetFogDistanceMode),
   getInteger1, getFloat1, getFloat4 )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.VertexSpec (
   Color4(Color4), Index1(Index1) )

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
   FogIndex -> 0xb61
   FogDensity -> 0xb62
   FogStart -> 0xb63
   FogEnd -> 0xb64
   FogMode -> 0xb65
   FogColor -> 0xb66
   FogCoordSrc -> 0x8450
   FogDistanceMode -> 0x855a

--------------------------------------------------------------------------------

data FogMode' =
     Linear'
   | Exp'
   | Exp2'

marshalFogMode' :: FogMode' -> GLint
marshalFogMode' x = case x of
   Linear' -> 0x2601
   Exp' -> 0x800
   Exp2' -> 0x801

unmarshalFogMode' :: GLint -> FogMode'
unmarshalFogMode' x
   | x == 0x2601 = Linear'
   | x == 0x800 = Exp'
   | x == 0x801 = Exp2'
   | otherwise = error ("unmarshalFogMode': illegal value " ++ show x)

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

foreign import CALLCONV unsafe "glFogi" glFogi :: GLenum -> GLint -> IO ()

fogf :: FogParameter -> GLfloat -> IO ()
fogf = glFogf . marshalFogParameter

foreign import CALLCONV unsafe "glFogf" glFogf :: GLenum -> GLfloat -> IO ()

fogfv :: FogParameter -> Ptr (Color4 GLfloat) -> IO ()
fogfv = glFogfv . marshalFogParameter

foreign import CALLCONV unsafe "glFogfv" glFogfv ::
   GLenum -> Ptr (Color4 GLfloat) -> IO ()

--------------------------------------------------------------------------------

fogColor :: StateVar (Color4 GLfloat)
fogColor =
   makeStateVar (getFloat4 Color4 GetFogColor) (\c -> with c $ fogfv FogColor)

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
marshalFogCoordSrc x = case x of
   FogCoord -> 0x8451
   FragmentDepth -> 0x8452

unmarshalFogCoordSrc :: GLint -> FogCoordSrc
unmarshalFogCoordSrc x
   | x == 0x8451 = FogCoord
   | x == 0x8452 = FragmentDepth
   | otherwise = error ("unmarshalFogCoordSrc: illegal value " ++ show x)

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
marshalFogDistanceMode x = case x of
   EyeRadial -> 0x855b
   EyePlaneSigned -> 0x2502
   EyePlaneAbsolute -> 0x855c

unmarshalFogDistanceMode :: GLint -> FogDistanceMode
unmarshalFogDistanceMode x
   | x== 0x855b = EyeRadial
   | x == 0x2502 = EyePlaneSigned
   | x == 0x855c = EyePlaneAbsolute
   | otherwise = error ("unmarshalFogDistanceMode: illegal value " ++ show x)

--------------------------------------------------------------------------------

fogDistanceMode :: StateVar FogDistanceMode
fogDistanceMode =
   makeStateVar
      (getInteger1 unmarshalFogDistanceMode GetFogDistanceMode)
      (fogi FogDistanceMode . marshalFogDistanceMode)
