-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Rendering.OpenGL.GL.FramebufferObjects
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

module Graphics.Rendering.OpenGL.GL.FramebufferObjects (
   FramebufferObject(),
   defaultFramebufferObject,
   FramebufferTarget(..),

   bindFramebuffer,

   RenderbufferObject(),
   noRenderbufferObject,
   RenderbufferTarget(..),
   RenderbufferSize(..), Samples(..),

   bindRenderbuffer,

   renderbufferStorage,renderbufferStorageMultiSample,

   FramebufferAttachment(..),

) where

import Data.ObjectName
import Data.StateVar
import Foreign.Marshal.Array
import Graphics.Rendering.OpenGL.Raw.Core31

import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.Texturing.Objects
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat
import Graphics.Rendering.OpenGL.GL.Texturing.Specification
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget

-----------------------------------------------------------------------------

data FramebufferObject = FramebufferObject{ fbufferID :: GLuint }

instance ObjectName FramebufferObject where
    genObjectNames n =
       allocaArray n $ \buf -> do
          glGenFramebuffers (fromIntegral n) buf
          fmap (map FramebufferObject) $ peekArray n buf
    deleteObjectNames objs = withArrayLen (map fbufferID objs) $
       glDeleteFramebuffers . fromIntegral
    isObjectName = fmap unmarshalGLboolean . glIsFramebuffer . fbufferID

defaultFramebufferObject :: FramebufferObject
defaultFramebufferObject = FramebufferObject 0

-----------------------------------------------------------------------------

data FramebufferTarget =
     DrawFramebuffer
   | ReadFramebuffer
   | Framebuffer

marshalFramebufferTarget :: FramebufferTarget -> GLenum
marshalFramebufferTarget xs = case xs of
   DrawFramebuffer -> gl_DRAW_FRAMEBUFFER
   ReadFramebuffer -> gl_READ_FRAMEBUFFER
   Framebuffer -> gl_FRAMEBUFFER

marshalFramebufferTargetBinding :: FramebufferTarget -> GetPName
marshalFramebufferTargetBinding x = case x of
   DrawFramebuffer -> GetDrawFramebufferBinding
   ReadFramebuffer -> GetReadFramebufferBinding
   Framebuffer -> GetFramebufferBinding

-----------------------------------------------------------------------------

bindFramebuffer :: FramebufferTarget -> StateVar FramebufferObject
bindFramebuffer fbt =
    makeStateVar (getBoundFramebuffer fbt) (setFramebuffer fbt)

getBoundFramebuffer :: FramebufferTarget -> IO FramebufferObject
getBoundFramebuffer = getInteger1 (FramebufferObject . fromIntegral)
   . marshalFramebufferTargetBinding

setFramebuffer :: FramebufferTarget -> FramebufferObject -> IO ()
setFramebuffer fbt = glBindFramebuffer (marshalFramebufferTarget fbt) . fbufferID

-----------------------------------------------------------------------------

data RenderbufferObject = RenderbufferObject{ rbufferID :: GLuint}

instance ObjectName RenderbufferObject where
   genObjectNames n =
      allocaArray n $ \buf -> do
         glGenRenderbuffers (fromIntegral n) buf
         fmap (map RenderbufferObject) $ peekArray n buf
   deleteObjectNames objs = withArrayLen (map rbufferID objs) $
      glDeleteRenderbuffers . fromIntegral
   isObjectName = fmap unmarshalGLboolean . glIsRenderbuffer . rbufferID

noRenderbufferObject :: RenderbufferObject
noRenderbufferObject = RenderbufferObject 0

-----------------------------------------------------------------------------

data RenderbufferTarget =
   Renderbuffer

marshalRenderbufferTarget :: RenderbufferTarget -> GLenum
marshalRenderbufferTarget x = case x of
    Renderbuffer -> gl_RENDERBUFFER

marshalRenderbufferTargetBinding :: RenderbufferTarget -> GetPName
marshalRenderbufferTargetBinding x = case x of
    Renderbuffer -> GetRenderbufferBinding
-----------------------------------------------------------------------------

data RenderbufferSize = RenderbufferSize !GLsizei !GLsizei

newtype Samples = Samples GLsizei
-----------------------------------------------------------------------------

bindRenderbuffer :: RenderbufferTarget -> StateVar RenderbufferObject
bindRenderbuffer rbt =
    makeStateVar (getBoundRenderbuffer rbt) (setRenderbuffer rbt)

getBoundRenderbuffer :: RenderbufferTarget -> IO RenderbufferObject
getBoundRenderbuffer = getInteger1 (RenderbufferObject . fromIntegral)
   . marshalRenderbufferTargetBinding

setRenderbuffer :: RenderbufferTarget -> RenderbufferObject -> IO ()
setRenderbuffer rbt = glBindRenderbuffer (marshalRenderbufferTarget rbt)
   . rbufferID

-----------------------------------------------------------------------------

renderbufferStorageMultiSample :: RenderbufferTarget -> Samples
   -> PixelInternalFormat -> RenderbufferSize -> IO ()
renderbufferStorageMultiSample rbt (Samples s) pif (RenderbufferSize w h) =
   glRenderbufferStorageMultisample (marshalRenderbufferTarget rbt) s
       (marshalPixelInternalFormat' pif) w h


renderbufferStorage :: RenderbufferTarget -> PixelInternalFormat
   -> RenderbufferSize -> IO ()
renderbufferStorage rbt pif (RenderbufferSize w h) =
    glRenderbufferStorage (marshalRenderbufferTarget rbt)
       (marshalPixelInternalFormat' pif) w h

-----------------------------------------------------------------------------

data FramebufferAttachment =
     ColorAttachment !GLuint
   | DepthAttachment
   | StencilAttachment
   | DepthStencilAttachment

marshalFramebufferAttachment :: FramebufferAttachment -> GLenum
marshalFramebufferAttachment x = case x of
   ColorAttachment c -> if c >= maxColorAttachments
      then error $ "marshalFramebufferAttachment: index out of range" ++ show c
      else gl_COLOR_ATTACHMENT0 + fromIntegral c
   DepthAttachment -> gl_DEPTH_ATTACHMENT
   StencilAttachment -> gl_STENCIL_ATTACHMENT
   DepthStencilAttachment -> gl_DEPTH_STENCIL_ATTACHMENT

maxColorAttachments :: GLuint
maxColorAttachments = 16

-----------------------------------------------------------------------------

framebufferRenderbuffer :: FramebufferTarget -> FramebufferAttachment
   -> RenderbufferTarget -> RenderbufferObject -> IO ()
framebufferRenderbuffer fbt fba rbt (RenderbufferObject rboi) =
   glFramebufferRenderbuffer (marshalFramebufferTarget fbt)
      (marshalFramebufferAttachment fba) (marshalRenderbufferTarget rbt) rboi

framebufferTexture1D :: FramebufferTarget -> FramebufferAttachment
   -> TextureObject -> Level -> IO ()
framebufferTexture1D fbt fba (TextureObject t) l  = glFramebufferTexture1D
   (marshalFramebufferTarget fbt) (marshalFramebufferAttachment fba)
      (marshalTextureTarget Texture1D) t l

framebufferTexture2D :: FramebufferTarget -> FramebufferAttachment
   -> Maybe CubeMapTarget-> TextureObject -> Level -> IO ()
framebufferTexture2D fbt fba mcmt (TextureObject t) l = glFramebufferTexture2D
   (marshalFramebufferTarget fbt) (marshalFramebufferAttachment fba)
      (maybe (marshalTextureTarget Texture2D) marshalCubeMapTarget mcmt) t l

framebufferTexture3D :: FramebufferTarget -> FramebufferAttachment
   -> TextureObject -> Level -> GLint -> IO ()
framebufferTexture3D fbt fba (TextureObject t) le la = glFramebufferTexture3D
   (marshalFramebufferTarget fbt) (marshalFramebufferAttachment fba)
      (marshalTextureTarget Texture1D) t le la

framebufferTextureLayer :: FramebufferTarget -> FramebufferAttachment
   -> TextureObject -> Level -> GLint -> IO()
framebufferTextureLayer fbt fba (TextureObject t) le la =
   glFramebufferTextureLayer (marshalFramebufferTarget fbt)
      (marshalFramebufferAttachment fba) t le la
