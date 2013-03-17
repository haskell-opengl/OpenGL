-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Shaders.Program
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <sven.panne@aedion.de>
-- Stability   :
-- Portability :
--
-- This module correspons with section 2.20.2 (Program Objects) of the OpenGL
-- 3.1 spec.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Shaders.Program (

   -- * Program Objects
   Program(..), programDeleteStatus, attachedShaders, linkProgram, linkStatus,
   programInfoLog, validateProgram, validateStatus, currentProgram,

   bindFragDataLocation, getFragDataLocation,

   -- * internals
   GetProgramPName(..), programVar, getCurrentProgram
) where

import Control.Monad
import Data.List
import Data.Maybe (fromMaybe)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Graphics.Rendering.OpenGL.GL.ObjectName
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.Framebuffer
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.GLstring
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw.Core31

import Graphics.Rendering.OpenGL.GL.Shaders.Shaders

--------------------------------------------------------------------------------

newtype Program = Program { programID :: GLuint }
   deriving ( Eq, Ord, Show )

instance ObjectName Program where
   genObjectNames n = replicateM n $ fmap Program glCreateProgram
   deleteObjectNames = mapM_ (glDeleteProgram . programID)
   isObjectName = fmap unmarshalGLboolean . glIsProgram . programID

--------------------------------------------------------------------------------

attachedShaders :: Program -> StateVar ([VertexShader],[FragmentShader])
attachedShaders program =
   makeStateVar (getAttachedShaders program) (setAttachedShaders program)

getAttachedShaders :: Program -> IO ([VertexShader],[FragmentShader])
getAttachedShaders program = getAttachedShaderIDs program >>= splitShaderIDs

getAttachedShaderIDs :: Program -> IO [GLuint]
getAttachedShaderIDs program = do
   numShaders <- get (numAttachedShaders program)
   allocaArray (fromIntegral numShaders) $ \buf -> do
      glGetAttachedShaders (programID program) numShaders nullPtr buf
      peekArray (fromIntegral numShaders) buf

splitShaderIDs :: [GLuint] -> IO ([VertexShader],[FragmentShader])
splitShaderIDs ids = do
   (vs, fs) <- partitionM isVertexShaderID ids
   return (map VertexShader vs, map FragmentShader fs)

isVertexShaderID :: GLuint -> IO Bool
isVertexShaderID x = do
   t <- get (shaderTypeEnum (VertexShader x))
   return $ t == shaderType (undefined :: VertexShader)

partitionM :: (a -> IO Bool) -> [a] -> IO ([a],[a])
partitionM p = foldM select ([],[])
   where select (ts, fs) x = do
            b <- p x
            return $ if b then (x:ts, fs) else (ts, x:fs)

setAttachedShaders :: Program -> ([VertexShader],[FragmentShader]) -> IO ()
setAttachedShaders p@(Program program) (vs, fs) = do
   currentIDs <- getAttachedShaderIDs p
   let newIDs = map shaderID vs ++ map shaderID fs
   mapM_ (glAttachShader program) (newIDs \\ currentIDs)
   mapM_ (glDetachShader program) (currentIDs \\ newIDs)

--------------------------------------------------------------------------------

linkProgram :: Program -> IO ()
linkProgram (Program program) = glLinkProgram program

currentProgram :: StateVar (Maybe Program)
currentProgram =
   makeStateVar
      (do p <- getCurrentProgram
          return $ if p == noProgram then Nothing else Just p)
      ((\(Program p) -> glUseProgram p) . fromMaybe noProgram)

getCurrentProgram :: IO Program
getCurrentProgram = fmap Program $ getInteger1 fromIntegral GetCurrentProgram

noProgram :: Program
noProgram = Program 0

validateProgram :: Program -> IO ()
validateProgram (Program program) = glValidateProgram program

programInfoLog :: Program -> GettableStateVar String
programInfoLog p =
   stringQuery (programInfoLogLength p) (glGetProgramInfoLog (programID p))

--------------------------------------------------------------------------------

programDeleteStatus :: Program -> GettableStateVar Bool
programDeleteStatus = programVar unmarshalGLboolean ProgramDeleteStatus

linkStatus :: Program -> GettableStateVar Bool
linkStatus = programVar unmarshalGLboolean LinkStatus

validateStatus :: Program -> GettableStateVar Bool
validateStatus = programVar unmarshalGLboolean ValidateStatus

programInfoLogLength :: Program -> GettableStateVar GLsizei
programInfoLogLength = programVar fromIntegral ProgramInfoLogLength

numAttachedShaders :: Program -> GettableStateVar GLsizei
numAttachedShaders = programVar fromIntegral AttachedShaders

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

--------------------------------------------------------------------------------

-- | 'bindFragDataLocation' binds a varying variable, specified by program and name, to a
-- drawbuffer. The effects only take place after succesfull linking of the program.
-- invalid arguments and conditions are
-- - an index larger than maxDrawBufferIndex
-- - names starting with 'gl_'
-- linking failure will ocure when
-- - one of the arguments was invalid
-- - more than one varying varuable name is bound to the same index
-- It's not an error to specify unused variables, those will be ingored.
bindFragDataLocation :: Program -> String -> SettableStateVar DrawBufferIndex
bindFragDataLocation (Program program) varName = makeSettableStateVar $ \ind ->
   withGLString varName $ glBindFragDataLocation program ind

-- | query the binding of a given variable, specified by program and name. The program has to be
-- linked. The result is Nothing if an error occures or the name is not a name of a varying
-- variable. If the program hasn't been linked an 'InvalidOperation' error is generated.
getFragDataLocation :: Program -> String -> IO (Maybe DrawBufferIndex)
getFragDataLocation (Program program) varName = do
   r <- withGLString varName $ glGetFragDataLocation program
   if r < 0
    then return Nothing
    else return . Just $ fromIntegral r
