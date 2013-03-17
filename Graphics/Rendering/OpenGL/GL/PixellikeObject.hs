-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Rendering.OpenGL.GL.PixellikeObject
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <sven.panne@aedion.de>
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PixellikeObject (
  PixellikeObjectGetPName(..),
  PixellikeObjectTarget(pixellikeObjTarParam),
) where

import Foreign.Marshal.Alloc
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.Raw.Core31

import Graphics.Rendering.OpenGL.GL.Texturing.Specification
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget

import Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects

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
      RedSize -> gl_RENDERBUFFER_RED_SIZE
      BlueSize -> gl_RENDERBUFFER_BLUE_SIZE
      GreenSize -> gl_RENDERBUFFER_GREEN_SIZE
      AlphaSize -> gl_RENDERBUFFER_ALPHA_SIZE
      DepthSize -> gl_RENDERBUFFER_DEPTH_SIZE
      StencilSize -> gl_RENDERBUFFER_STENCIL_SIZE
   pixObjTarQueryFunc t = getRBParameteriv t id

data FramebufferTargetAttachment =
    FramebufferTargetAttachment FramebufferTarget FramebufferObjectAttachment

instance PixellikeObjectTarget FramebufferTargetAttachment where
   marshalPixellikeOT _ x = case x of
      RedSize -> gl_FRAMEBUFFER_ATTACHMENT_RED_SIZE
      BlueSize -> gl_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE
      GreenSize -> gl_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE
      AlphaSize -> gl_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE
      DepthSize -> gl_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE
      StencilSize -> gl_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE
   pixObjTarQueryFunc (FramebufferTargetAttachment fbt fba) =
      getFBAParameteriv fbt fba id

data TextureTargetFull = TextureTargetFull (Either TextureTarget CubeMapTarget) Level
instance PixellikeObjectTarget TextureTargetFull where
   marshalPixellikeOT _ x = case x of
      RedSize -> gl_TEXTURE_RED_SIZE
      BlueSize -> gl_TEXTURE_BLUE_SIZE
      GreenSize -> gl_TEXTURE_GREEN_SIZE
      AlphaSize -> gl_TEXTURE_ALPHA_SIZE
      DepthSize -> gl_TEXTURE_DEPTH_SIZE
      StencilSize -> gl_TEXTURE_STENCIL_SIZE
   pixObjTarQueryFunc (TextureTargetFull t level) p =
      alloca $ \buf -> do
      glGetTexLevelParameteriv (marshalTarget t) level p buf
      peek1 id buf
        where marshalTarget = either marshalTextureTarget marshalCubeMapTarget
