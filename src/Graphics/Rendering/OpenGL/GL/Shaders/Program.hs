{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Shaders.Program
-- Copyright   :  (c) Sven Panne 2019
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
import Graphics.GL

--------------------------------------------------------------------------------

newtype Program = Program { programID :: GLuint }
   deriving ( Eq, Ord, Show )

instance ObjectName Program where
   isObjectName = liftIO . fmap unmarshalGLboolean . glIsProgram . programID
   deleteObjectName = liftIO . glDeleteProgram . programID

instance CanBeLabeled Program where
   objectLabel = objectNameLabel GL_PROGRAM . programID

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
   ProgramDeleteStatus -> GL_DELETE_STATUS
   LinkStatus -> GL_LINK_STATUS
   ValidateStatus -> GL_VALIDATE_STATUS
   ProgramInfoLogLength -> GL_INFO_LOG_LENGTH
   AttachedShaders -> GL_ATTACHED_SHADERS
   ActiveAttributes -> GL_ACTIVE_ATTRIBUTES
   ActiveAttributeMaxLength -> GL_ACTIVE_ATTRIBUTE_MAX_LENGTH
   ActiveUniforms -> GL_ACTIVE_UNIFORMS
   ActiveUniformMaxLength -> GL_ACTIVE_UNIFORM_MAX_LENGTH
   TransformFeedbackBufferMode -> GL_TRANSFORM_FEEDBACK_BUFFER_MODE
   TransformFeedbackVaryings -> GL_TRANSFORM_FEEDBACK_VARYINGS
   TransformFeedbackVaryingMaxLength -> GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH
   ActiveUniformBlocks -> GL_ACTIVE_UNIFORM_BLOCKS
   ActiveUniformBlockMaxNameLength -> GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH
   GeometryVerticesOut -> GL_GEOMETRY_VERTICES_OUT
   GeometryInputType -> GL_GEOMETRY_INPUT_TYPE
   GeometryOutputType -> GL_GEOMETRY_OUTPUT_TYPE
   GeometryShaderInvocations -> GL_GEOMETRY_SHADER_INVOCATIONS
   TessControlOutputVertices -> GL_TESS_CONTROL_OUTPUT_VERTICES
   TessGenMode -> GL_TESS_GEN_MODE
   TessGenSpacing -> GL_TESS_GEN_SPACING
   TessGenVertexOrder -> GL_TESS_GEN_VERTEX_ORDER
   TessGenPointMode -> GL_TESS_GEN_POINT_MODE
   ComputeWorkGroupSize -> GL_COMPUTE_WORK_GROUP_SIZE
   ProgramSeparable -> GL_PROGRAM_SEPARABLE
   ProgramBinaryRetrievableHint -> GL_PROGRAM_BINARY_RETRIEVABLE_HINT
   ActiveAtomicCounterBuffers -> GL_ACTIVE_ATOMIC_COUNTER_BUFFERS
   ProgramBinaryLength -> GL_PROGRAM_BINARY_LENGTH

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
