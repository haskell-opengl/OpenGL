-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixellikeObject
-- Copyright   :  (c) Sven Panne 2011-2019, Lars Corbijn 2011-2016
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PixellikeObject (
  PixellikeObjectGetPName(..),
  PixellikeObjectTarget(pixellikeObjTarParam),
) where

import Data.StateVar
import Foreign.Marshal.Utils
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjectAttachment
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferTarget
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferTarget
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.Texturing.Specification
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget
import Graphics.GL

-----------------------------------------------------------------------------

data PixellikeObjectGetPName =
     RedSize
   | BlueSize
   | GreenSize
   | AlphaSize
   | DepthSize
   | StencilSize

class PixellikeObjectTarget t where
   --dummy t to include it in the type class
   marshalPixellikeOT :: t -> PixellikeObjectGetPName -> GLenum
   pixObjTarQueryFunc :: t -> GLenum -> IO GLint
   pixellikeObjTarParam :: t -> PixellikeObjectGetPName -> GettableStateVar GLint
   pixellikeObjTarParam t p = makeGettableStateVar (pixObjTarQueryFunc t $ marshalPixellikeOT t p)

instance PixellikeObjectTarget RenderbufferTarget where
   marshalPixellikeOT _ x = case x of
      RedSize -> GL_RENDERBUFFER_RED_SIZE
      BlueSize -> GL_RENDERBUFFER_BLUE_SIZE
      GreenSize -> GL_RENDERBUFFER_GREEN_SIZE
      AlphaSize -> GL_RENDERBUFFER_ALPHA_SIZE
      DepthSize -> GL_RENDERBUFFER_DEPTH_SIZE
      StencilSize -> GL_RENDERBUFFER_STENCIL_SIZE
   pixObjTarQueryFunc t = getRBParameteriv t id

data FramebufferTargetAttachment =
    FramebufferTargetAttachment FramebufferTarget FramebufferObjectAttachment

instance PixellikeObjectTarget FramebufferTargetAttachment where
   marshalPixellikeOT _ x = case x of
      RedSize -> GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE
      BlueSize -> GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE
      GreenSize -> GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE
      AlphaSize -> GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE
      DepthSize -> GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE
      StencilSize -> GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE
   pixObjTarQueryFunc (FramebufferTargetAttachment fbt fba) =
      getFBAParameteriv fbt fba id

data TextureTargetFull t = TextureTargetFull t Level

instance QueryableTextureTarget t => PixellikeObjectTarget (TextureTargetFull t) where
   marshalPixellikeOT _ x = case x of
      RedSize -> GL_TEXTURE_RED_SIZE
      BlueSize -> GL_TEXTURE_BLUE_SIZE
      GreenSize -> GL_TEXTURE_GREEN_SIZE
      AlphaSize -> GL_TEXTURE_ALPHA_SIZE
      DepthSize -> GL_TEXTURE_DEPTH_SIZE
      StencilSize -> GL_TEXTURE_STENCIL_SIZE
   pixObjTarQueryFunc (TextureTargetFull t level) p =
      with 0 $ \buf -> do
      glGetTexLevelParameteriv (marshalQueryableTextureTarget t) level p buf
      peek1 id buf
