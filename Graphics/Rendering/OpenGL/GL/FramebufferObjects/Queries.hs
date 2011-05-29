-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Rendering.OpenGL.GL.FramebufferObjects.Queries
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

module Graphics.Rendering.OpenGL.GL.FramebufferObjects.Queries (
   GetFramebufferAttachmentPName(..),

   attachmentObjectType,

   GetRenderbufferPName(..),
) where
-- TODO add a datatype and type class for querying red,blue, etc. size of
-- Objects (Framebuffer, Renderbuffer, Texture)
import Data.StateVar
import Foreign.Marshal
import Graphics.Rendering.OpenGL.Raw.Core31

import Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments

import Graphics.Rendering.OpenGL.GL.PeekPoke

-----------------------------------------------------------------------------

data GetFramebufferAttachmentPName =
     AttachmentObjectType
   | AttachmentObjectName
   | AttachmentTextureLevel
   | AttachmentTextureCubeMapFace
   | AttachmentTextureLayer
   | AttachmentRedSize
   | AttachmentGreenSize
   | AttachmentBlueSize
   | AttachmentAlphaSize
   | AttachmentDepthSize
   | AttachmentStencilSize
   | AttachmentComponentType
   | AttachmentColorEncoding

marshalGetFBAPname :: GetFramebufferAttachmentPName -> GLenum
marshalGetFBAPname x = case x of
   AttachmentObjectType -> gl_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE
   AttachmentObjectName -> gl_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME
   AttachmentTextureLevel -> gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL
   AttachmentTextureCubeMapFace -> gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
   AttachmentTextureLayer -> gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER
   AttachmentRedSize -> gl_FRAMEBUFFER_ATTACHMENT_RED_SIZE
   AttachmentGreenSize -> gl_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE
   AttachmentBlueSize -> gl_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE
   AttachmentAlphaSize -> gl_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE
   AttachmentDepthSize -> gl_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE
   AttachmentStencilSize -> gl_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE
   AttachmentComponentType -> gl_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE
   AttachmentColorEncoding -> gl_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING

getFBAParameteriv :: FramebufferAttachment fba => FramebufferTarget -> fba
    -> (GLint -> a) -> GetFramebufferAttachmentPName -> IO a
getFBAParameteriv fbt fba f p = alloca $ \buf -> do
   glGetFramebufferAttachmentParameteriv (marshalFramebufferTarget fbt)
      (marshalAttachment fba) (marshalGetFBAPname p) buf
   peek1 f buf

-----------------------------------------------------------------------------

data AttachmentObjectType =
     DefaultFramebufferAttachment
   | TextureAttachment
   | RenderbufferAttachment

unmarshalAttachmentObjectType :: GLenum -> Maybe AttachmentObjectType
unmarshalAttachmentObjectType x
   | x == gl_FRAMEBUFFER_DEFAULT = Just DefaultFramebufferAttachment
   | x == gl_TEXTURE = Just TextureAttachment
   | x == gl_RENDERBUFFER = Just RenderbufferAttachment
   | x == gl_NONE = Nothing
   | otherwise = error $ "unmarshalAttachmentObject: unknown value " ++ show x

attachmentObjectType :: FramebufferAttachment fba => FramebufferTarget -> fba
   -> GettableStateVar (Maybe AttachmentObjectType)
attachmentObjectType fbt fba = makeGettableStateVar $ getFBAParameteriv fbt fba
   (unmarshalAttachmentObjectType . fromIntegral) AttachmentObjectType

-----------------------------------------------------------------------------

data GetRenderbufferPName =
     RenderbufferWidth
   | RenderbufferHeigth
   | RenderbufferInternalFormat
   | RenderbufferRedSize
   | RenderbufferSamples

marshalGetRBPname :: GetRenderbufferPName -> GLenum
marshalGetRBPname = undefined

getRBParameteriv :: RenderbufferTarget -> (GLint -> a)
   -> GetRenderbufferPName -> IO a
getRBParameteriv rbt f p = alloca $ \buf -> do
   glGetRenderbufferParameteriv (marshalRenderbufferTarget rbt)
      (marshalGetRBPname p) buf
   peek1 f buf

