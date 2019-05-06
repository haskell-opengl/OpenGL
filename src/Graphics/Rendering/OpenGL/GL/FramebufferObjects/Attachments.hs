-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments
-- Copyright   :  (c) Sven Panne 2011-2019, Lars Corbijn 2011-2016
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments (
   FramebufferObjectAttachment(..),

   fboaToBufferMode, fboaFromBufferMode,

   FramebufferAttachment(..),

   framebufferRenderbuffer, framebufferTexture1D, framebufferTexture2D,
   framebufferTexture3D, framebufferTextureLayer
) where

import Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjectAttachment
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferTarget
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObject
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferTarget
import Graphics.Rendering.OpenGL.GL.Texturing.Specification
import Graphics.Rendering.OpenGL.GL.Texturing.TextureObject
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.GL

-----------------------------------------------------------------------------

framebufferRenderbuffer :: FramebufferTarget -> FramebufferObjectAttachment
   -> RenderbufferTarget -> RenderbufferObject -> IO ()
framebufferRenderbuffer fbt fba rbt (RenderbufferObject rboi) =
   maybe recordInvalidValue (\mfba ->  glFramebufferRenderbuffer (marshalFramebufferTarget fbt)
      mfba (marshalRenderbufferTarget rbt) rboi) $ marshalFramebufferObjectAttachment fba

framebufferTexture1D :: FramebufferTarget -> FramebufferObjectAttachment
   -> TextureTarget1D -> TextureObject -> Level -> IO ()
framebufferTexture1D fbt fba tt (TextureObject t) l  =  maybe recordInvalidValue
   (\mfba -> glFramebufferTexture1D (marshalFramebufferTarget fbt) mfba
      (marshalQueryableTextureTarget tt) t l) $ marshalFramebufferObjectAttachment fba

-- Note: Typing is too permissive, no TEXTURE_1D_ARRAY allowed per 4.4. spec.
framebufferTexture2D :: FramebufferTarget -> FramebufferObjectAttachment
   -> TextureTarget2D -> TextureObject -> Level -> IO ()
framebufferTexture2D fbt fba tt (TextureObject t) l = maybe recordInvalidValue
   (\mfba -> glFramebufferTexture2D (marshalFramebufferTarget fbt) mfba
      (marshalQueryableTextureTarget tt) t l)
         $ marshalFramebufferObjectAttachment fba

-- Note: Typing is too permissive, no TEXTURE_2D_ARRAY or TEXTURE_2D_MULTISAMPLE_ARRAY allowed per 4.4. spec.
framebufferTexture3D :: FramebufferTarget -> FramebufferObjectAttachment
   -> TextureTarget3D -> TextureObject -> Level -> GLint -> IO ()
framebufferTexture3D fbt fba tt (TextureObject t) le la = maybe recordInvalidValue
   (\mfba -> glFramebufferTexture3D (marshalFramebufferTarget fbt) mfba
      (marshalQueryableTextureTarget tt) t le la) $ marshalFramebufferObjectAttachment fba

framebufferTextureLayer :: FramebufferTarget -> FramebufferObjectAttachment
   -> TextureObject -> Level -> GLint -> IO()
framebufferTextureLayer fbt fba (TextureObject t) le la = maybe recordInvalidValue
   (\mfba -> glFramebufferTextureLayer (marshalFramebufferTarget fbt)
      mfba t le la) $ marshalFramebufferObjectAttachment fba
