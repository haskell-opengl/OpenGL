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

import Control.Monad ( liftM2 )
import Foreign.Marshal.Array ( withArray )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapPolygonSmooth,CapCullFace,CapPolygonStipple,
             CapPolygonOffsetPoint,CapPolygonOffsetLine,CapPolygonOffsetFill),
   makeCapability, makeStateVarMaybe )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLubyte, GLfloat, Capability )
import Graphics.Rendering.OpenGL.GL.Face ( marshalFace, unmarshalFace )
import Graphics.Rendering.OpenGL.GL.Colors ( Face(..) )
import Graphics.Rendering.OpenGL.GL.PolygonMode (
   PolygonMode(..), marshalPolygonMode, unmarshalPolygonMode )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetCullFaceMode,GetPolygonMode,GetPolygonOffsetFactor,
            GetPolygonOffsetUnits),
   getInteger2, getEnum1, getFloat1, getArrayWith )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar, makeStateVar )

--------------------------------------------------------------------------------

polygonSmooth :: StateVar Capability
polygonSmooth = makeCapability CapPolygonSmooth

--------------------------------------------------------------------------------

cullFace :: StateVar (Maybe Face)
cullFace = makeStateVarMaybe (return CapCullFace)
                             (getEnum1 unmarshalFace GetCullFaceMode)
                             (glCullFace . marshalFace)

foreign import CALLCONV unsafe "glCullFace" glCullFace :: GLenum -> IO ()

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
polygonStipple =
   makeStateVarMaybe
      (return CapPolygonStipple)
      (getArrayWith PolygonStipple numPolygonStippleBytes glGetPolygonStipple)
      (\(PolygonStipple pattern) -> withArray pattern $ glPolygonStipple)

foreign import CALLCONV unsafe "glGetPolygonStipple" glGetPolygonStipple ::
   Ptr GLubyte -> IO ()

foreign import CALLCONV unsafe "glPolygonStipple" glPolygonStipple ::
   Ptr GLubyte -> IO ()

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
polygonOffset =
   makeStateVar (liftM2 (,) (getFloat1 id GetPolygonOffsetFactor)
                            (getFloat1 id GetPolygonOffsetUnits))
                (uncurry glPolygonOffset)

foreign import CALLCONV unsafe "glPolygonOffset" glPolygonOffset ::
   GLfloat -> GLfloat -> IO ()

--------------------------------------------------------------------------------

polygonOffsetPoint :: StateVar Capability
polygonOffsetPoint = makeCapability CapPolygonOffsetPoint

polygonOffsetLine :: StateVar Capability
polygonOffsetLine = makeCapability CapPolygonOffsetLine

polygonOffsetFill :: StateVar Capability
polygonOffsetFill = makeCapability CapPolygonOffsetFill
