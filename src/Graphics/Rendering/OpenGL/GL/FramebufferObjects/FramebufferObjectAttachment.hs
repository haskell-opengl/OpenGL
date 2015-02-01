{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjectAttachment
-- Copyright   :  (c) Sven Panne 2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for marshaling FrameBufferObjectAttachments.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjectAttachment (
   FramebufferObjectAttachment(..),
   marshalFramebufferObjectAttachment,
   unmarshalFramebufferObjectAttachment,
   unmarshalFramebufferObjectAttachmentSafe,
   fboaToBufferMode, fboaFromBufferMode,

   FramebufferAttachment(..), getFBAParameteriv
) where

import Data.Maybe
import Foreign.Marshal
import Graphics.Rendering.OpenGL.GL.BufferMode
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferTarget
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

data FramebufferObjectAttachment =
     ColorAttachment !GLuint
   | DepthAttachment
   | StencilAttachment
   | DepthStencilAttachment
   deriving ( Eq, Ord, Show )

marshalFramebufferObjectAttachment :: FramebufferObjectAttachment -> Maybe GLenum
marshalFramebufferObjectAttachment x = case x of
   ColorAttachment c -> let ec = fromIntegral c in if ec >= maxColorAttachments
      then Nothing
      else Just $ gl_COLOR_ATTACHMENT0 + ec
   DepthAttachment -> Just gl_DEPTH_ATTACHMENT
   StencilAttachment -> Just gl_STENCIL_ATTACHMENT
   DepthStencilAttachment -> Just gl_DEPTH_STENCIL_ATTACHMENT

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

fboaToBufferMode :: FramebufferObjectAttachment -> Maybe BufferMode
fboaToBufferMode (ColorAttachment i) = Just . FBOColorAttachment $ fromIntegral i
fboaToBufferMode _                   = Nothing

fboaFromBufferMode :: BufferMode -> Maybe FramebufferObjectAttachment
fboaFromBufferMode (FBOColorAttachment i) = Just . ColorAttachment $ fromIntegral i
fboaFromBufferMode _                      = Nothing

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

getFBAParameteriv :: FramebufferAttachment fba => FramebufferTarget -> fba
    -> (GLint -> a) -> GLenum -> IO a
getFBAParameteriv fbt fba f p = with 0 $ \buf -> do
   glGetFramebufferAttachmentParameteriv (marshalFramebufferTarget fbt)
      mfba p buf
   peek1 f buf
      where mfba = fromMaybe (error $ "invalid value" ++ show fba) (marshalAttachment fba)
