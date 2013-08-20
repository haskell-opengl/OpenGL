-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObject
-- Copyright   :  (c) Sven Panne 2013
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling FrameBufferObjects.
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
