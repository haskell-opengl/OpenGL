--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Polygons
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 3.5 (Polygons) of the OpenGL 1.5 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Polygons (
   polygonSmooth, cullFace,
   PolygonStipple(..), GLpolygonstipple, polygonStipple,
   PolygonMode(..), polygonMode, polygonOffset,
   polygonOffsetPoint, polygonOffsetLine, polygonOffsetFill
) where

import Control.Monad ( liftM2 )
import Foreign.ForeignPtr ( ForeignPtr, mallocForeignPtrArray, withForeignPtr )
import Foreign.Marshal.Array ( allocaArray, withArray, peekArray, pokeArray )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapPolygonSmooth,CapCullFace,CapPolygonStipple,
             CapPolygonOffsetPoint,CapPolygonOffsetLine,CapPolygonOffsetFill),
   makeCapability, makeStateVarMaybe )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLubyte, GLfloat, Capability )
import Graphics.Rendering.OpenGL.GL.Face ( marshalFace, unmarshalFace )
import Graphics.Rendering.OpenGL.GL.Colors ( Face(..) )
import Graphics.Rendering.OpenGL.GL.PixelRectangles (
   PixelStoreDirection(..), rowLength, skipRows, skipPixels )
import Graphics.Rendering.OpenGL.GL.PolygonMode (
   PolygonMode(..), marshalPolygonMode, unmarshalPolygonMode )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetCullFaceMode,GetPolygonMode,GetPolygonOffsetFactor,
            GetPolygonOffsetUnits),
   getInteger2, getEnum1, getFloat1 )
import Graphics.Rendering.OpenGL.GL.SavingState (
   ClientAttributeGroup(PixelStoreAttributes), preservingClientAttrib )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasSetter(($=)), StateVar, makeStateVar )

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

numPolygonStippleBytes :: Int
numPolygonStippleBytes = 128   -- 32x32 bits divided into GLubytes

class PolygonStipple s where
   withNewPolygonStipple :: (Ptr GLubyte -> IO ()) -> IO s
   withPolygonStipple :: s -> (Ptr GLubyte -> IO a) -> IO a
   newPolygonStipple :: [GLubyte] -> IO s
   getPolygonStippleComponents :: s -> IO [GLubyte]

   withNewPolygonStipple act =
      allocaArray numPolygonStippleBytes $ \p -> do
         act p
         components <- peekArray numPolygonStippleBytes p
         newPolygonStipple components

   withPolygonStipple s act = do
      components <- getPolygonStippleComponents s
      withArray components act

   newPolygonStipple components =
      withNewPolygonStipple $
         flip pokeArray (take numPolygonStippleBytes components)

   getPolygonStippleComponents s =
      withPolygonStipple s $ peekArray numPolygonStippleBytes

--------------------------------------------------------------------------------

data GLpolygonstipple = GLpolygonstipple (ForeignPtr GLubyte)
   deriving ( Eq, Ord, Show )

instance PolygonStipple GLpolygonstipple where
   withNewPolygonStipple f = do
      fp <- mallocForeignPtrArray numPolygonStippleBytes
      withForeignPtr fp f
      return $ GLpolygonstipple fp

   withPolygonStipple (GLpolygonstipple fp) = withForeignPtr fp

--------------------------------------------------------------------------------

polygonStipple :: PolygonStipple s => StateVar (Maybe s)
polygonStipple =
   makeStateVarMaybe (return CapPolygonStipple)
      (withoutGaps Pack $ withNewPolygonStipple glGetPolygonStipple)
      (\s -> withoutGaps Unpack $ withPolygonStipple s glPolygonStipple)

-- Note: No need to set rowAlignment, our memory allocator always returns a
-- region which is at least 8-byte aligned (the maximum)
withoutGaps :: PixelStoreDirection -> IO a -> IO a
withoutGaps direction action =
   preservingClientAttrib [ PixelStoreAttributes ] $ do
      rowLength  direction $= 0
      skipRows   direction $= 0
      skipPixels direction $= 0
      action

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
