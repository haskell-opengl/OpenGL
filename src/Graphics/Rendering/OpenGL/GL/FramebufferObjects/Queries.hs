-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.FramebufferObjects.Queries
-- Copyright   :  (c) Sven Panne 2011-2019, Lars Corbijn 2011-2016
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.FramebufferObjects.Queries (
   AttachmentObjectType(..), attachmentObjectType, attachmentObject,
   attachmentTextureLayer, attachmentTextureLevel,
   attachmentTextureTextureTargetCubeMapFace,

   attachmentRedSize, attachmentBlueSize, attachmentGreenSize,
   attachmentAlphaSize, attachmentDepthSize, attachmentStencilSize,

   renderbufferWidth, renderbufferHeight,
   renderbufferInternalFormat, renderbufferSamples,

   renderbufferRedSize, renderbufferBlueSize, renderbufferGreenSize,
   renderbufferAlphaSize, renderbufferDepthSize, renderbufferStencilSize,
) where

import Data.StateVar
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjectAttachment
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferTarget
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObject
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferTarget
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat
import Graphics.Rendering.OpenGL.GL.Texturing.Specification(Level)
import Graphics.Rendering.OpenGL.GL.Texturing.TextureObject
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget
import Graphics.GL

-----------------------------------------------------------------------------

data GetFramebufferAttachmentPName =
     AttachmentObjectType
   | AttachmentObjectName
   | AttachmentTextureLevel
   | AttachmentTextureCubeMapFace
   | AttachmentTextureLayer
   | AttachmentComponentType
   | AttachmentColorEncoding
   | AttachmentRedSize
   | AttachmentBlueSize
   | AttachmentGreenSize
   | AttachmentAlphaSize
   | AttachmentDepthSize
   | AttachmentStencilSize

marshalGetFBAPName :: GetFramebufferAttachmentPName -> GLenum
marshalGetFBAPName x = case x of
   AttachmentObjectType -> GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE
   AttachmentObjectName -> GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME
   AttachmentTextureLevel -> GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL
   AttachmentTextureCubeMapFace -> GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
   AttachmentTextureLayer -> GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER
   AttachmentComponentType -> GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE -- TODO impement usefull function
   AttachmentColorEncoding -> GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING -- TODO impement usefull function
   AttachmentRedSize -> GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE
   AttachmentBlueSize -> GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE
   AttachmentGreenSize -> GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE
   AttachmentAlphaSize -> GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE
   AttachmentDepthSize -> GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE
   AttachmentStencilSize -> GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE

getFBAPName :: FramebufferAttachment fba => FramebufferTarget -> fba
    -> (GLint -> a) -> GetFramebufferAttachmentPName -> IO a
getFBAPName fbt fba f p = getFBAParameteriv fbt fba f (marshalGetFBAPName p)

-----------------------------------------------------------------------------

data AttachmentObjectType =
     DefaultFramebufferAttachment
   | TextureAttachment
   | RenderbufferAttachment
   deriving ( Eq, Ord, Show )

unmarshalAttachmentObjectType :: GLenum -> Maybe AttachmentObjectType
unmarshalAttachmentObjectType x
   | x == GL_FRAMEBUFFER_DEFAULT = Just DefaultFramebufferAttachment
   | x == GL_TEXTURE = Just TextureAttachment
   | x == GL_RENDERBUFFER = Just RenderbufferAttachment
   | x == GL_NONE = Nothing
   | otherwise = error $ "unmarshalAttachmentObject: unknown value " ++ show x

attachmentObjectType :: FramebufferAttachment fba => FramebufferTarget -> fba
   -> GettableStateVar (Maybe AttachmentObjectType)
attachmentObjectType fbt fba = makeGettableStateVar $ getFBAPName fbt fba
   (unmarshalAttachmentObjectType . fromIntegral) AttachmentObjectType

-- | tries to retrieve the object that is bound to the attachment point of the
-- given framebuffertarget. If the object type of it is None or the default, then
-- `Nothing` is returned, otherwise the bound `RenderbufferObject` or `TextureObject`
attachmentObject :: FramebufferAttachment fba => FramebufferTarget -> fba
   -> GettableStateVar (Maybe (Either RenderbufferObject TextureObject))
attachmentObject fbt fba = makeGettableStateVar getter
   where getter = do
            objT <- get $ attachmentObjectType fbt fba
            case objT of
               Nothing                             -> return $ Nothing
               (Just DefaultFramebufferAttachment) -> return $ Nothing
               (Just TextureAttachment)            -> getObjectName (Right . TextureObject)
               (Just RenderbufferAttachment)       -> getObjectName (Left . RenderbufferObject)
         getObjectName :: Num n => (n -> Either RenderbufferObject TextureObject) -> IO (Maybe (Either RenderbufferObject TextureObject))
         getObjectName con = getFBAPName fbt fba (Just . con . fromIntegral) AttachmentObjectName

attachmentTextureLayer :: FramebufferAttachment fba => FramebufferTarget -> fba
   -> GettableStateVar GLint
attachmentTextureLayer fbt fba = makeGettableStateVar $
   getFBAPName fbt fba id AttachmentTextureLayer

attachmentTextureLevel :: FramebufferAttachment fba => FramebufferTarget -> fba
   -> GettableStateVar Level
attachmentTextureLevel fbt fba = makeGettableStateVar $
   getFBAPName fbt fba id AttachmentTextureLevel

attachmentTextureTextureTargetCubeMapFace :: FramebufferAttachment fba => FramebufferTarget -> fba
   -> GettableStateVar TextureTargetCubeMapFace
attachmentTextureTextureTargetCubeMapFace fbt fba = makeGettableStateVar $
   getFBAPName fbt fba (unmarshalTextureTargetCubeMapFace . fromIntegral) AttachmentTextureLevel

-----------------------------------------------------------------------------

attachmentRedSize :: FramebufferAttachment fba => FramebufferTarget -> fba
   -> GettableStateVar GLint
attachmentRedSize fbt fba = makeGettableStateVar $ getFBAPName fbt fba
    id AttachmentRedSize

attachmentGreenSize :: FramebufferAttachment fba => FramebufferTarget -> fba
   -> GettableStateVar GLint
attachmentGreenSize fbt fba = makeGettableStateVar $ getFBAPName fbt fba
    id AttachmentGreenSize

attachmentBlueSize :: FramebufferAttachment fba => FramebufferTarget -> fba
   -> GettableStateVar GLint
attachmentBlueSize fbt fba = makeGettableStateVar $ getFBAPName fbt fba
    id AttachmentBlueSize

attachmentAlphaSize :: FramebufferAttachment fba => FramebufferTarget -> fba
   -> GettableStateVar GLint
attachmentAlphaSize fbt fba = makeGettableStateVar $ getFBAPName fbt fba
    id AttachmentAlphaSize

attachmentDepthSize :: FramebufferAttachment fba => FramebufferTarget -> fba
   -> GettableStateVar GLint
attachmentDepthSize fbt fba = makeGettableStateVar $ getFBAPName fbt fba
    id AttachmentDepthSize

attachmentStencilSize :: FramebufferAttachment fba => FramebufferTarget -> fba
   -> GettableStateVar GLint
attachmentStencilSize fbt fba = makeGettableStateVar $ getFBAPName fbt fba
    id AttachmentStencilSize

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
    RenderbufferWidth -> GL_RENDERBUFFER_WIDTH
    RenderbufferHeight -> GL_RENDERBUFFER_HEIGHT
    RenderbufferInternalFormat -> GL_RENDERBUFFER_INTERNAL_FORMAT
    RenderbufferSamples -> GL_RENDERBUFFER_SAMPLES
    RenderbufferRedSize -> GL_RENDERBUFFER_RED_SIZE
    RenderbufferBlueSize -> GL_RENDERBUFFER_BLUE_SIZE
    RenderbufferGreenSize -> GL_RENDERBUFFER_GREEN_SIZE
    RenderbufferAlphaSize -> GL_RENDERBUFFER_ALPHA_SIZE
    RenderbufferDepthSize -> GL_RENDERBUFFER_DEPTH_SIZE
    RenderbufferStencilSize -> GL_RENDERBUFFER_STENCIL_SIZE

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
