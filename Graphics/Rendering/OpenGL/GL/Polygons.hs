--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Polygons
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 3.5 (Polygons) of the OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Polygons (
   polygonSmooth, cullFace,
   PolygonStipple, makePolygonStipple, getPolygonStippleBytes, polygonStipple,
   PolygonMode(..), polygonMode, polygonOffset,
   polygonOffsetPoint, polygonOffsetLine, polygonOffsetFill
) where

import Foreign.Marshal.Array ( allocaArray, peekArray, withArray )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLubyte, GLfloat )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapPolygonSmooth,CapCullFace,CapPolygonStipple,
             CapPolygonOffsetPoint,CapPolygonOffsetLine,CapPolygonOffsetFill),
   makeCapability )
import Graphics.Rendering.OpenGL.GL.Face (
   Face(..), marshalFace, unmarshalFace )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetCullFaceMode,GetPolygonMode,GetPolygonOffsetFactor,
            GetPolygonOffsetUnits),
   getInteger1, getInteger2, getFloat1 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasGetter(get), HasSetter(($=)), StateVar, makeStateVar )

--------------------------------------------------------------------------------

polygonSmooth :: StateVar Bool
polygonSmooth = makeCapability CapPolygonSmooth

--------------------------------------------------------------------------------

cullFace :: StateVar (Maybe Face)
cullFace = makeStateVar getCullFace setCullFace

getCullFace :: IO (Maybe Face)
getCullFace = do
   enabled <- get cullFaceEnabled
   if enabled
      then getInteger1 (Just . unmarshalFace . fromIntegral) GetCullFaceMode
      else return Nothing

setCullFace :: Maybe Face -> IO ()
setCullFace Nothing = cullFaceEnabled $= False
setCullFace (Just face) = do
   cullFaceEnabled $= True
   glCullFace (marshalFace face)

foreign import CALLCONV unsafe "glCullFace" glCullFace :: GLenum -> IO ()

cullFaceEnabled :: StateVar Bool
cullFaceEnabled = makeCapability CapCullFace

--------------------------------------------------------------------------------

newtype PolygonStipple = PolygonStipple [GLubyte]
   deriving ( Eq, Ord, Show )

numPolygonStippleBytes :: Int
numPolygonStippleBytes = 32

makePolygonStipple :: [GLubyte] -> PolygonStipple
makePolygonStipple pattern
   | length pattern == numPolygonStippleBytes = PolygonStipple pattern
   | otherwise =
        error ("makePolygonStipple: expected " ++ show numPolygonStippleBytes ++
               " pattern bytes")

getPolygonStippleBytes :: PolygonStipple -> [GLubyte]
getPolygonStippleBytes (PolygonStipple pattern) = pattern

--------------------------------------------------------------------------------

polygonStipple :: StateVar (Maybe PolygonStipple)
polygonStipple = makeStateVar getPolygonStipple setPolygonStipple

getPolygonStipple :: IO (Maybe PolygonStipple)
getPolygonStipple = do
   enabled <- get polygonStippleEnabled
   if enabled
      then do pattern <- allocaArray numPolygonStippleBytes $ \buf -> do
                            glGetPolygonStipple buf
                            peekArray numPolygonStippleBytes buf
              return $ Just (PolygonStipple pattern)
      else return Nothing

foreign import CALLCONV unsafe "glGetPolygonStipple" glGetPolygonStipple ::
   Ptr GLubyte -> IO ()

setPolygonStipple :: Maybe PolygonStipple -> IO ()
setPolygonStipple Nothing = polygonStippleEnabled $= False
setPolygonStipple (Just (PolygonStipple pattern)) = do
   polygonStippleEnabled $= True
   withArray pattern $ glPolygonStipple

foreign import CALLCONV unsafe "glPolygonStipple" glPolygonStipple ::
   Ptr GLubyte -> IO ()

polygonStippleEnabled :: StateVar Bool
polygonStippleEnabled = makeCapability CapPolygonStipple

--------------------------------------------------------------------------------

data PolygonMode =
     Point
   | Line
   | Fill
   deriving ( Eq, Ord, Show )

marshalPolygonMode :: PolygonMode -> GLenum
marshalPolygonMode x = case x of
   Point -> 0x1b00
   Line -> 0x1b01
   Fill -> 0x1b02

unmarshalPolygonMode :: GLenum -> PolygonMode
unmarshalPolygonMode x
   | x == 0x1b00 = Point
   | x == 0x1b01 = Line
   | x == 0x1b02 = Fill
   | otherwise = error ("unmarshalPolygonMode: illegal value " ++ show x)

--------------------------------------------------------------------------------

polygonMode :: StateVar (PolygonMode, PolygonMode)
polygonMode = makeStateVar getPolygonMode setPolygonMode

getPolygonMode :: IO (PolygonMode, PolygonMode)
getPolygonMode = getInteger2 (\front back -> (un front, un back)) GetPolygonMode
   where un = unmarshalPolygonMode . fromIntegral

setPolygonMode :: (PolygonMode, PolygonMode) -> IO ()
setPolygonMode (front, back) = do
   glPolygonMode (marshalFace Front) (marshalPolygonMode front)
   glPolygonMode (marshalFace Back ) (marshalPolygonMode back )

foreign import CALLCONV unsafe "glPolygonMode" glPolygonMode ::
   GLenum -> GLenum -> IO ()

--------------------------------------------------------------------------------

polygonOffset :: StateVar (GLfloat, GLfloat)
polygonOffset = makeStateVar getPolygonOffset setPolygonOffset

getPolygonOffset :: IO (GLfloat, GLfloat)
getPolygonOffset = do
   factor <- getFloat1 id GetPolygonOffsetFactor
   units  <- getFloat1 id GetPolygonOffsetUnits
   return (factor, units)

setPolygonOffset :: (GLfloat, GLfloat) -> IO ()
setPolygonOffset (factor, units) = glPolygonOffset factor units

foreign import CALLCONV unsafe "glPolygonOffset" glPolygonOffset ::
   GLfloat -> GLfloat -> IO ()

--------------------------------------------------------------------------------

polygonOffsetPoint :: StateVar Bool
polygonOffsetPoint = makeCapability CapPolygonOffsetPoint

polygonOffsetLine :: StateVar Bool
polygonOffsetLine = makeCapability CapPolygonOffsetLine

polygonOffsetFill :: StateVar Bool
polygonOffsetFill = makeCapability CapPolygonOffsetFill
