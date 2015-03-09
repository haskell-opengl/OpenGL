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

import Control.Monad.IO.Class
import Data.ObjectName
import Foreign.Marshal.Array ( allocaArray, peekArray, withArrayLen )
import Graphics.Rendering.OpenGL.GL.DebugOutput
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

newtype TextureObject = TextureObject { textureID :: GLuint }
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

instance ObjectName TextureObject where
   isObjectName = liftIO . fmap unmarshalGLboolean . glIsTexture . textureID

   deleteObjectNames textureObjects =
      liftIO . withArrayLen (map textureID textureObjects) $
         glDeleteTextures . fromIntegral

instance GeneratableObjectName TextureObject where
   genObjectNames n =
      liftIO . allocaArray n $ \buf -> do
        glGenTextures (fromIntegral n) buf
        fmap (map TextureObject) $ peekArray n buf

instance CanBeLabeled TextureObject where
   objectLabel = objectNameLabel gl_TEXTURE . textureID
