{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObject
-- Copyright   :  (c) Sven Panne 2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling RenderBufferObjects.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObject (
   RenderbufferObject(..)
) where

import Foreign.Marshal
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.ObjectName
import Graphics.Rendering.OpenGL.Raw
       
--------------------------------------------------------------------------------

data RenderbufferObject = RenderbufferObject { renderbufferID :: GLuint}

instance ObjectName RenderbufferObject where
   isObjectName = fmap unmarshalGLboolean . glIsRenderbuffer . renderbufferID

   deleteObjectNames objs =
      withArrayLen (map renderbufferID objs) $
         glDeleteRenderbuffers . fromIntegral

instance GeneratableObjectName RenderbufferObject where
   genObjectNames n =
      allocaArray n $ \buf -> do
         glGenRenderbuffers (fromIntegral n) buf
         fmap (map RenderbufferObject) $ peekArray n buf
