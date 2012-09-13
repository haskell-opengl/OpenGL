-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments
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

module Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments (
   FramebufferObjectAttachment(..),

   fboaToBufferMode, fboaFromBufferMode,

   maxColorAttachments,

   FramebufferAttachment(..),

   framebufferRenderbuffer, framebufferTexture1D, framebufferTexture2D,
   framebufferTexture3D, framebufferTextureLayer,

   getFBAParameteriv,
) where

import Data.Maybe (fromMaybe)
import Foreign.Marshal
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal

import Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects

import Graphics.Rendering.OpenGL.GL.BufferMode
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.Texturing.Objects
import Graphics.Rendering.OpenGL.GL.Texturing.Specification
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget
-----------------------------------------------------------------------------

data FramebufferObjectAttachment =
     ColorAttachment !GLuint
   | DepthAttachment
   | StencilAttachment
   | DepthStencilAttachment
   deriving (Eq, Show)

marshalFramebufferObjectAttachment :: FramebufferObjectAttachment -> Maybe GLenum
marshalFramebufferObjectAttachment x = case x of
   ColorAttachment c -> let ec = fromIntegral c in if ec >= maxColorAttachments
      then Nothing
      else Just $ gl_COLOR_ATTACHMENT0 + ec
   DepthAttachment -> Just gl_DEPTH_ATTACHMENT
   StencilAttachment -> Just gl_STENCIL_ATTACHMENT
   DepthStencilAttachment -> Just gl_DEPTH_STENCIL_ATTACHMENT

fboaToBufferMode :: FramebufferObjectAttachment -> Maybe BufferMode
fboaToBufferMode (ColorAttachment i) = Just . FBOColorAttachment $ fromIntegral i
fboaToBufferMode _                   = Nothing

fboaFromBufferMode :: BufferMode -> Maybe FramebufferObjectAttachment
fboaFromBufferMode (FBOColorAttachment i) = Just . ColorAttachment $ fromIntegral i
fboaFromBufferMode _                      = Nothing

unmarshalFramebufferObjectAttachment :: GLenum -> FramebufferObjectAttachment
unmarshalFramebufferObjectAttachment x = maybe
   (error $ "unmarshalFramebufferObjectAttachment: unknown enum value " ++ show x) id $
      unmarshalFramebufferObjectAttachmentSafe x
--unmarshalFramebufferObjectAttachment x
--   | x == gl_DEPTH_ATTACHMENT = DepthAttachment
--   | x == gl_STENCIL_ATTACHMENT = StencilAttachment
--   | x == gl_DEPTH_STENCIL_ATTACHMENT = DepthStencilAttachment
--   | x >= gl_COLOR_ATTACHMENT0 && x <= gl_COLOR_ATTACHMENT15
--      = ColorAttachment . fromIntegral $ x - gl_COLOR_ATTACHMENT0
--   | otherwise = error $ "unmarshalFramebufferObjectAttachment: unknown enum value " ++ show x

unmarshalFramebufferObjectAttachmentSafe :: GLenum -> Maybe FramebufferObjectAttachment
unmarshalFramebufferObjectAttachmentSafe x
   | x == gl_DEPTH_ATTACHMENT = Just DepthAttachment
   | x == gl_STENCIL_ATTACHMENT = Just StencilAttachment
   | x == gl_DEPTH_STENCIL_ATTACHMENT = Just DepthStencilAttachment
   | x >= gl_COLOR_ATTACHMENT0 && x <= gl_COLOR_ATTACHMENT0 + maxColorAttachments
      = Just . ColorAttachment . fromIntegral $ x - gl_COLOR_ATTACHMENT0
   | otherwise = Nothing

-----------------------------------------------------------------------------

class Show a => FramebufferAttachment a where
   marshalAttachment :: a -> Maybe GLenum
   unmarshalAttachment :: GLenum -> a
   unmarshalAttachmentSafe :: GLenum -> Maybe a
instance FramebufferAttachment FramebufferObjectAttachment where
   marshalAttachment = marshalFramebufferObjectAttachment
   unmarshalAttachment = unmarshalFramebufferObjectAttachment
   unmarshalAttachmentSafe = unmarshalFramebufferObjectAttachmentSafe
instance FramebufferAttachment BufferMode where
   marshalAttachment = marshalBufferMode
   unmarshalAttachment = unmarshalBufferMode
   unmarshalAttachmentSafe = unmarshalBufferModeSafe

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


-----------------------------------------------------------------------------

getFBAParameteriv :: FramebufferAttachment fba => FramebufferTarget -> fba
    -> (GLint -> a) -> GLenum -> IO a
getFBAParameteriv fbt fba f p = alloca $ \buf -> do
   glGetFramebufferAttachmentParameteriv (marshalFramebufferTarget fbt)
      mfba p buf
   peek1 f buf
      where mfba = fromMaybe (error $ "invalid value" ++ show fba) (marshalAttachment fba)
