{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Shaders.Shader
-- Copyright   :  (c) Sven Panne 2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for handling shader objects.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Shaders.Shader (
   Shader(..)
) where

import Control.Monad.IO.Class
import Data.ObjectName
import Graphics.Rendering.OpenGL.GL.DebugOutput
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

newtype Shader = Shader { shaderID :: GLuint }
   deriving ( Eq, Ord, Show )

instance ObjectName Shader where
   isObjectName = liftIO . fmap unmarshalGLboolean . glIsShader . shaderID
   deleteObjectName = liftIO . glDeleteShader . shaderID

instance CanBeLabeled Shader where
   objectLabel = objectNameLabel gl_SHADER . shaderID
