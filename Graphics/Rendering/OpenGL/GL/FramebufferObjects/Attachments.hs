-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <sven.panne@aedion.de>
-- Stability   :
-- Portability :
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
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects
import Graphics.Rendering.OpenGL.GL.Texturing.Specification
import Graphics.Rendering.OpenGL.GL.Texturing.TextureObject
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.Rendering.OpenGL.Raw.Core31

-----------------------------------------------------------------------------

framebufferRenderbuffer :: FramebufferTarget -> FramebufferObjectAttachment
   -> RenderbufferTarget -> RenderbufferObject -> IO ()
framebufferRenderbuffer fbt fba rbt (RenderbufferObject rboi) =
   maybe recordInvalidValue (\mfba ->  glFramebufferRenderbuffer (marshalFramebufferTarget fbt)
      mfba (marshalRenderbufferTarget rbt) rboi) $ marshalFramebufferObjectAttachment fba

framebufferTexture1D :: FramebufferTarget -> FramebufferObjectAttachment
   -> TextureObject -> Level -> IO ()
framebufferTexture1D fbt fba (TextureObject t) l  =  maybe recordInvalidValue
   (\mfba -> glFramebufferTexture1D (marshalFramebufferTarget fbt) mfba
      (marshalTextureTarget Texture1D) t l) $ marshalFramebufferObjectAttachment fba

framebufferTexture2D :: FramebufferTarget -> FramebufferObjectAttachment
   -> Maybe CubeMapTarget-> TextureObject -> Level -> IO ()
framebufferTexture2D fbt fba mcmt (TextureObject t) l = maybe recordInvalidValue
   (\mfba -> glFramebufferTexture2D (marshalFramebufferTarget fbt) mfba
      (maybe (marshalTextureTarget Texture2D) marshalCubeMapTarget mcmt) t l)
         $ marshalFramebufferObjectAttachment fba

framebufferTexture3D :: FramebufferTarget -> FramebufferObjectAttachment
   -> TextureObject -> Level -> GLint -> IO ()
framebufferTexture3D fbt fba (TextureObject t) le la = maybe recordInvalidValue
   (\mfba -> glFramebufferTexture3D (marshalFramebufferTarget fbt) mfba
      (marshalTextureTarget Texture1D) t le la) $ marshalFramebufferObjectAttachment fba

framebufferTextureLayer :: FramebufferTarget -> FramebufferObjectAttachment
   -> TextureObject -> Level -> GLint -> IO()
framebufferTextureLayer fbt fba (TextureObject t) le la = maybe recordInvalidValue
   (\mfba -> glFramebufferTextureLayer (marshalFramebufferTarget fbt)
      mfba t le la) $ marshalFramebufferObjectAttachment fba
