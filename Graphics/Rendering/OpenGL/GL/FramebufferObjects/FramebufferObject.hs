{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObject
-- Copyright   :  (c) Sven Panne 2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for handling FrameBufferObjects.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObject (
   FramebufferObject(..)
) where

import Foreign.Marshal
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.ObjectName
import Graphics.Rendering.OpenGL.Raw.Core31
       
--------------------------------------------------------------------------------

data FramebufferObject = FramebufferObject { framebufferID :: GLuint }
   deriving ( Eq, Ord, Show )

instance ObjectName FramebufferObject where
    isObjectName = fmap unmarshalGLboolean . glIsFramebuffer . framebufferID

    deleteObjectNames objs =
       withArrayLen (map framebufferID objs) $
          glDeleteFramebuffers . fromIntegral

instance GeneratableObjectName FramebufferObject where
    genObjectNames n =
       allocaArray n $ \buf -> do
          glGenFramebuffers (fromIntegral n) buf
          fmap (map FramebufferObject) $ peekArray n buf
