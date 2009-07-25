-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelData
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal helper module.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PixelData (
   PixelData(..), withPixelData
) where

import Foreign.Ptr
import Graphics.Rendering.OpenGL.GL.DataType
import Graphics.Rendering.OpenGL.GL.PixelFormat
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

data PixelData a = PixelData PixelFormat DataType (Ptr a)
   deriving ( Eq, Ord, Show )

withPixelData :: PixelData a -> (GLenum -> GLenum -> Ptr a -> b) -> b
withPixelData (PixelData pixelFormat dataType ptr) f =
   f (marshalPixelFormat pixelFormat) (marshalDataType dataType) ptr
