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
   AttachmentObjectType(..), getFBAPName,
   attachmentObjectType,

   renderbufferWidth, renderbufferHeight,
   renderbufferInternalFormat, renderbufferSamples,

   renderbufferRedSize, renderbufferBlueSize, renderbufferGreenSize,
   renderbufferAlphaSize, renderbufferDepthSize, renderbufferStencilSize,
) where

import Data.StateVar
import Graphics.Rendering.OpenGL.Raw.Core31

import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat

import Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments


-----------------------------------------------------------------------------

data GetFramebufferAttachmentPName =
     AttachmentObjectType
   | AttachmentObjectName
   | AttachmentTextureLevel
   | AttachmentTextureCubeMapFace
   | AttachmentTextureLayer
   | AttachmentComponentType
   | AttachmentColorEncoding

marshalGetFBAPName :: GetFramebufferAttachmentPName -> GLenum
marshalGetFBAPName x = case x of
   AttachmentObjectType -> gl_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE
   AttachmentObjectName -> gl_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME
   AttachmentTextureLevel -> gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL
   AttachmentTextureCubeMapFace -> gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
   AttachmentTextureLayer -> gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER
   AttachmentComponentType -> gl_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE
   AttachmentColorEncoding -> gl_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING

getFBAPName :: FramebufferAttachment fba => FramebufferTarget -> fba
    -> (GLint -> a) -> GetFramebufferAttachmentPName -> IO a
getFBAPName fbt fba f p = getFBAParameteriv fbt fba f (marshalGetFBAPName p)

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
attachmentObjectType fbt fba = makeGettableStateVar $ getFBAPName fbt fba
   (unmarshalAttachmentObjectType . fromIntegral) AttachmentObjectType

-----------------------------------------------------------------------------


data GetRenderbufferPName =
     RenderbufferWidth
   | RenderbufferHeight
   | RenderbufferInternalFormat
   | RenderbufferSamples
   | RenderbufferRedSize
   | RenderbufferBlueSize
   | RenderbufferGreenSize
   | RenderbufferAlphaSize
   | RenderbufferDepthSize
   | RenderbufferStencilSize

marshalGetRBPname :: GetRenderbufferPName -> GLenum
marshalGetRBPname x = case x of
    RenderbufferWidth -> gl_RENDERBUFFER_WIDTH
    RenderbufferHeight -> gl_RENDERBUFFER_HEIGHT
    RenderbufferInternalFormat -> gl_RENDERBUFFER_INTERNAL_FORMAT
    RenderbufferSamples -> gl_RENDERBUFFER_SAMPLES
    RenderbufferRedSize -> gl_RENDERBUFFER_RED_SIZE
    RenderbufferBlueSize -> gl_RENDERBUFFER_BLUE_SIZE
    RenderbufferGreenSize -> gl_RENDERBUFFER_GREEN_SIZE
    RenderbufferAlphaSize -> gl_RENDERBUFFER_ALPHA_SIZE
    RenderbufferDepthSize -> gl_RENDERBUFFER_DEPTH_SIZE
    RenderbufferStencilSize -> gl_RENDERBUFFER_STENCIL_SIZE

getRBPName :: RenderbufferTarget -> (GLint -> a) ->
   GetRenderbufferPName -> IO a
getRBPName rbt f = getRBParameteriv rbt f . marshalGetRBPname

-----------------------------------------------------------------------------

renderbufferWidth :: RenderbufferTarget -> GettableStateVar GLsizei
renderbufferWidth rbt = makeGettableStateVar $
   getRBPName rbt fromIntegral RenderbufferWidth

renderbufferHeight :: RenderbufferTarget -> GettableStateVar GLsizei
renderbufferHeight rbt = makeGettableStateVar $
   getRBPName rbt fromIntegral RenderbufferHeight

renderbufferInternalFormat :: RenderbufferTarget
   -> GettableStateVar PixelInternalFormat
renderbufferInternalFormat rbt = makeGettableStateVar $
   getRBPName rbt unmarshalPixelInternalFormat RenderbufferInternalFormat

renderbufferSamples :: RenderbufferTarget -> GettableStateVar Samples
renderbufferSamples rbt = makeGettableStateVar $
   getRBPName rbt (Samples . fromIntegral) RenderbufferSamples

renderbufferRedSize :: RenderbufferTarget -> GettableStateVar GLint
renderbufferRedSize rbt = makeGettableStateVar $
   getRBPName rbt id RenderbufferRedSize

renderbufferGreenSize :: RenderbufferTarget -> GettableStateVar GLint
renderbufferGreenSize rbt = makeGettableStateVar $
   getRBPName rbt id RenderbufferGreenSize

renderbufferBlueSize :: RenderbufferTarget -> GettableStateVar GLint
renderbufferBlueSize rbt = makeGettableStateVar $
   getRBPName rbt id RenderbufferBlueSize

renderbufferAlphaSize :: RenderbufferTarget -> GettableStateVar GLint
renderbufferAlphaSize rbt = makeGettableStateVar $
   getRBPName rbt id RenderbufferAlphaSize

renderbufferDepthSize :: RenderbufferTarget -> GettableStateVar GLint
renderbufferDepthSize rbt = makeGettableStateVar $
   getRBPName rbt id RenderbufferDepthSize

renderbufferStencilSize :: RenderbufferTarget -> GettableStateVar GLint
renderbufferStencilSize rbt = makeGettableStateVar $
   getRBPName rbt id RenderbufferStencilSize
