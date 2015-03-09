{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObject
-- Copyright   :  (c) Sven Panne 2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling RenderBufferObjects.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObject (
   RenderbufferObject(..)
) where


import Control.Monad.IO.Class
import Data.ObjectName
import Foreign.Marshal ( allocaArray, peekArray, withArrayLen )
import Graphics.Rendering.OpenGL.GL.DebugOutput
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw
       
--------------------------------------------------------------------------------

newtype RenderbufferObject = RenderbufferObject { renderbufferID :: GLuint}
   deriving ( Eq, Ord, Show )

instance ObjectName RenderbufferObject where
   isObjectName =
     liftIO . fmap unmarshalGLboolean . glIsRenderbuffer . renderbufferID

   deleteObjectNames objs =
      liftIO . withArrayLen (map renderbufferID objs) $
         glDeleteRenderbuffers . fromIntegral

instance GeneratableObjectName RenderbufferObject where
   genObjectNames n =
      liftIO . allocaArray n $ \buf -> do
         glGenRenderbuffers (fromIntegral n) buf
         fmap (map RenderbufferObject) $ peekArray n buf

instance CanBeLabeled RenderbufferObject where
   objectLabel = objectNameLabel gl_RENDERBUFFER . renderbufferID
