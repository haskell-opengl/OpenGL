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
   Program(..),
   GetProgramPName(..), marshalGetProgramPName,
   programVar1, programVar3
) where

import Control.Monad.IO.Class
import Data.ObjectName
import Data.StateVar
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.DebugOutput
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

newtype Program = Program { programID :: GLuint }
   deriving ( Eq, Ord, Show )

instance ObjectName Program where
   isObjectName = liftIO . fmap unmarshalGLboolean . glIsProgram . programID
   deleteObjectName = liftIO . glDeleteProgram . programID

instance CanBeLabeled Program where
   objectLabel = objectNameLabel gl_PROGRAM . programID

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
   | ActiveUniformBlocks
   | ActiveUniformBlockMaxNameLength
   | GeometryVerticesOut
   | GeometryInputType
   | GeometryOutputType
   | GeometryShaderInvocations
   | TessControlOutputVertices
   | TessGenMode
   | TessGenSpacing
   | TessGenVertexOrder
   | TessGenPointMode
   | ComputeWorkGroupSize  -- 3 integers!
   | ProgramSeparable
   | ProgramBinaryRetrievableHint
   | ActiveAtomicCounterBuffers
   | ProgramBinaryLength

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
   ActiveUniformBlocks -> gl_ACTIVE_UNIFORM_BLOCKS
   ActiveUniformBlockMaxNameLength -> gl_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH
   GeometryVerticesOut -> gl_GEOMETRY_VERTICES_OUT
   GeometryInputType -> gl_GEOMETRY_INPUT_TYPE
   GeometryOutputType -> gl_GEOMETRY_OUTPUT_TYPE
   GeometryShaderInvocations -> gl_GEOMETRY_SHADER_INVOCATIONS
   TessControlOutputVertices -> gl_TESS_CONTROL_OUTPUT_VERTICES
   TessGenMode -> gl_TESS_GEN_MODE
   TessGenSpacing -> gl_TESS_GEN_SPACING
   TessGenVertexOrder -> gl_TESS_GEN_VERTEX_ORDER
   TessGenPointMode -> gl_TESS_GEN_POINT_MODE
   ComputeWorkGroupSize ->  gl_COMPUTE_WORK_GROUP_SIZE
   ProgramSeparable -> gl_PROGRAM_SEPARABLE
   ProgramBinaryRetrievableHint -> gl_PROGRAM_BINARY_RETRIEVABLE_HINT
   ActiveAtomicCounterBuffers -> gl_ACTIVE_ATOMIC_COUNTER_BUFFERS
   ProgramBinaryLength -> gl_PROGRAM_BINARY_LENGTH

programVar1 :: (GLint -> a) -> GetProgramPName -> Program -> GettableStateVar a
programVar1 = programVarN . peek1

programVar3 :: (GLint -> GLint -> GLint -> a) -> GetProgramPName -> Program -> GettableStateVar a
programVar3 = programVarN . peek3

programVarN :: (Ptr GLint -> IO a) -> GetProgramPName -> Program -> GettableStateVar a
programVarN f p program =
   makeGettableStateVar $
      with 0 $ \buf -> do
         glGetProgramiv (programID program) (marshalGetProgramPName p) buf
         f buf
