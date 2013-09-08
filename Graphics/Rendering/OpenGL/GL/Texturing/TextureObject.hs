{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.TextureObject
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for handling texture objects.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing.TextureObject (
   TextureObject(..)
) where

import Foreign.Marshal.Array
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.ObjectName
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

newtype TextureObject = TextureObject { textureID :: GLuint }
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

instance ObjectName TextureObject where
   isObjectName = fmap unmarshalGLboolean . glIsTexture . textureID

   deleteObjectNames textureObjects =
      withArrayLen (map textureID textureObjects) $
         glDeleteTextures . fromIntegral

instance GeneratableObjectName TextureObject where
   genObjectNames n =
      allocaArray n $ \buf -> do
        glGenTextures (fromIntegral n) buf
        fmap (map TextureObject) $ peekArray n buf
