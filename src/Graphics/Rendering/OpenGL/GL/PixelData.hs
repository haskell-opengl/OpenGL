{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelData
-- Copyright   :  (c) Sven Panne 2002-2019
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
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
import Graphics.GL

--------------------------------------------------------------------------------

data PixelData a = PixelData PixelFormat DataType (Ptr a)
   deriving ( Eq, Ord, Show )

withPixelData :: PixelData a -> (GLenum -> GLenum -> Ptr a -> b) -> b
withPixelData (PixelData pixelFormat dataType ptr) f =
   f (marshalPixelFormat pixelFormat) (marshalDataType dataType) ptr
