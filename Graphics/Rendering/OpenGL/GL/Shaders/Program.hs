{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Shaders.Program
-- Copyright   :  (c) Sven Panne 2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for handling program objects and related
-- queries.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Shaders.Program (
   Program(..), GetProgramPName(..), programVar
) where

import Foreign.Marshal.Alloc
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.ObjectName
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

newtype Program = Program { programID :: GLuint }
   deriving ( Eq, Ord, Show )

instance ObjectName Program where
   isObjectName = fmap unmarshalGLboolean . glIsProgram . programID
   deleteObjectName = glDeleteProgram . programID

--------------------------------------------------------------------------------

data GetProgramPName =
     ProgramDeleteStatus
   | LinkStatus
   | ValidateStatus
   | ProgramInfoLogLength
   | AttachedShaders
   | ActiveAttributes
   | ActiveAttributeMaxLength
   | ActiveUniforms
   | ActiveUniformMaxLength
   | TransformFeedbackBufferMode
   | TransformFeedbackVaryings
   | TransformFeedbackVaryingMaxLength

marshalGetProgramPName :: GetProgramPName -> GLenum
marshalGetProgramPName x = case x of
   ProgramDeleteStatus -> gl_DELETE_STATUS
   LinkStatus -> gl_LINK_STATUS
   ValidateStatus -> gl_VALIDATE_STATUS
   ProgramInfoLogLength -> gl_INFO_LOG_LENGTH
   AttachedShaders -> gl_ATTACHED_SHADERS
   ActiveAttributes -> gl_ACTIVE_ATTRIBUTES
   ActiveAttributeMaxLength -> gl_ACTIVE_ATTRIBUTE_MAX_LENGTH
   ActiveUniforms -> gl_ACTIVE_UNIFORMS
   ActiveUniformMaxLength -> gl_ACTIVE_UNIFORM_MAX_LENGTH
   TransformFeedbackBufferMode -> gl_TRANSFORM_FEEDBACK_BUFFER_MODE
   TransformFeedbackVaryings -> gl_TRANSFORM_FEEDBACK_VARYINGS
   TransformFeedbackVaryingMaxLength -> gl_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH

programVar :: (GLint -> a) -> GetProgramPName -> Program -> GettableStateVar a
programVar f p program =
   makeGettableStateVar $
      alloca $ \buf -> do
         glGetProgramiv (programID program) (marshalGetProgramPName p) buf
         peek1 f buf
