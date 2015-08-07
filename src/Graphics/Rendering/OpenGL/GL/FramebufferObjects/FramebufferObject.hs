{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObject
-- Copyright   :  (c) Sven Panne 2013-2015
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for handling FramebufferObjects.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObject (
   FramebufferObject(..)
) where

import Control.Monad.IO.Class
import Data.ObjectName
import Foreign.Marshal.Array ( allocaArray, peekArray, withArrayLen )
import Graphics.Rendering.OpenGL.GL.DebugOutput
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw
       
--------------------------------------------------------------------------------

newtype FramebufferObject = FramebufferObject { framebufferID :: GLuint }
   deriving ( Eq, Ord, Show )

instance ObjectName FramebufferObject where
    isObjectName =
      liftIO . fmap unmarshalGLboolean . glIsFramebuffer . framebufferID

    deleteObjectNames objs =
       liftIO . withArrayLen (map framebufferID objs) $
          glDeleteFramebuffers . fromIntegral

instance GeneratableObjectName FramebufferObject where
    genObjectNames n =
       liftIO . allocaArray n $ \buf -> do
          glGenFramebuffers (fromIntegral n) buf
          fmap (map FramebufferObject) $ peekArray n buf

instance CanBeLabeled FramebufferObject where
   objectLabel = objectNameLabel gl_FRAMEBUFFER . framebufferID
