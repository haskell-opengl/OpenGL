--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Polygons
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 3.5 (Polygons) of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Polygons (
   polygonSmooth, cullFace,
   PolygonStipple(..), GLpolygonstipple, polygonStipple,
   PolygonMode(..), polygonMode, polygonOffset,
   polygonOffsetPoint, polygonOffsetLine, polygonOffsetFill
) where

import Control.Monad
import Data.StateVar
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.Face
import Graphics.Rendering.OpenGL.GL.PixelRectangles
import Graphics.Rendering.OpenGL.GL.PolygonMode
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.SavingState
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

polygonSmooth :: StateVar Capability
polygonSmooth = makeCapability CapPolygonSmooth

--------------------------------------------------------------------------------

cullFace :: StateVar (Maybe Face)
cullFace = makeStateVarMaybe (return CapCullFace)
                             (getEnum1 unmarshalFace GetCullFaceMode)
                             (glCullFace . marshalFace)

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

--------------------------------------------------------------------------------

polygonMode :: StateVar (PolygonMode, PolygonMode)
polygonMode = makeStateVar getPolygonMode setPolygonMode

getPolygonMode :: IO (PolygonMode, PolygonMode)
getPolygonMode = getInteger2 (\front back -> (un front, un back)) GetPolygonMode
   where un = unmarshalPolygonMode . fromIntegral

setPolygonMode :: (PolygonMode, PolygonMode) -> IO ()
setPolygonMode (front, back)
   -- OpenGL 3 deprecated separate polygon draw modes, so try to avoid them.
   | front == back = setPM FrontAndBack front
   | otherwise = do setPM Front front; setPM Back back
   where setPM f m = glPolygonMode (marshalFace f) (marshalPolygonMode m)

--------------------------------------------------------------------------------

polygonOffset :: StateVar (GLfloat, GLfloat)
polygonOffset =
   makeStateVar (liftM2 (,) (getFloat1 id GetPolygonOffsetFactor)
                            (getFloat1 id GetPolygonOffsetUnits))
                (uncurry glPolygonOffset)

--------------------------------------------------------------------------------

polygonOffsetPoint :: StateVar Capability
polygonOffsetPoint = makeCapability CapPolygonOffsetPoint

polygonOffsetLine :: StateVar Capability
polygonOffsetLine = makeCapability CapPolygonOffsetLine

polygonOffsetFill :: StateVar Capability
polygonOffsetFill = makeCapability CapPolygonOffsetFill
