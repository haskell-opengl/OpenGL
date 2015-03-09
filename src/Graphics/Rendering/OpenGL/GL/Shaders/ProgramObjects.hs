-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects
-- Copyright   :  (c) Sven Panne 2006-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 7.3 (Program Objects) of the OpenGL 4.4
-- spec.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects (
   -- * Program Objects
   Program, createProgram, programDeleteStatus,
   attachShader, detachShader, attachedShaders,
   linkProgram, linkStatus,
   validateProgram, validateStatus,
   programInfoLog,
   currentProgram,
   programSeparable, programBinaryRetrievableHint,

   -- TODOs:
   --    glCreateShaderProgramv
   --    ProgramInterface type (from 7.3.1)
   --    glGetProgramInterfaceiv
   --    glGetProgramResourceIndex
   --    glGetProgramResourceName
   --    glGetProgramResourceiv
   --    glGetProgramResourceLocation
   --    glGetProgramResourceLocationIndex

   -- * Fragment Data
   bindFragDataLocation, getFragDataLocation
) where

import Data.List
import Data.Maybe
import Data.StateVar
import Foreign.Marshal.Array
import Foreign.Ptr
import Graphics.Rendering.OpenGL.GL.ByteString
import Graphics.Rendering.OpenGL.GL.Framebuffer
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import Graphics.Rendering.OpenGL.GL.Shaders.Shader
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

createProgram :: IO Program
createProgram = fmap Program glCreateProgram

--------------------------------------------------------------------------------

attachShader :: Program -> Shader -> IO ()
attachShader p s = glAttachShader (programID p) (shaderID s)

detachShader :: Program -> Shader -> IO ()
detachShader p s = glDetachShader (programID p) (shaderID s)

attachedShaders :: Program -> StateVar [Shader]
attachedShaders program =
   makeStateVar (getAttachedShaders program) (setAttachedShaders program)

getAttachedShaders :: Program -> IO [Shader]
getAttachedShaders program = do
   numShaders <- get (numAttachedShaders program)
   ids <- allocaArray (fromIntegral numShaders) $ \buf -> do
      glGetAttachedShaders (programID program) numShaders nullPtr buf
      peekArray (fromIntegral numShaders) buf
   return $ map Shader ids

setAttachedShaders :: Program -> [Shader] -> IO ()
setAttachedShaders program newShaders = do
   currentShaders <- getAttachedShaders program
   mapM_ (attachShader program) (newShaders \\ currentShaders)
   mapM_ (detachShader program) (currentShaders \\ newShaders)

--------------------------------------------------------------------------------

linkProgram :: Program -> IO ()
linkProgram = glLinkProgram . programID

currentProgram :: StateVar (Maybe Program)
currentProgram =
   makeStateVar
      (do p <- fmap Program $ getInteger1 fromIntegral GetCurrentProgram
          return $ if p == noProgram then Nothing else Just p)
      (glUseProgram . programID . fromMaybe noProgram)

noProgram :: Program
noProgram = Program 0

validateProgram :: Program -> IO ()
validateProgram = glValidateProgram . programID

programInfoLog :: Program -> GettableStateVar String
programInfoLog =
   makeGettableStateVar .
      fmap unpackUtf8 .
         stringQuery programInfoLogLength (glGetProgramInfoLog . programID)

--------------------------------------------------------------------------------

programSeparable :: Program -> StateVar Bool
programSeparable = programStateVarBool ProgramSeparable

programBinaryRetrievableHint :: Program -> StateVar Bool
programBinaryRetrievableHint = programStateVarBool ProgramBinaryRetrievableHint

programStateVarBool :: GetProgramPName -> Program -> StateVar Bool
programStateVarBool pname program =
   makeStateVar
      (get (programVar1 unmarshalGLboolean pname program))
      (glProgramParameteri (programID program)
                           (marshalGetProgramPName pname) . marshalGLboolean)

--------------------------------------------------------------------------------

programDeleteStatus :: Program -> GettableStateVar Bool
programDeleteStatus = programVar1 unmarshalGLboolean ProgramDeleteStatus

linkStatus :: Program -> GettableStateVar Bool
linkStatus = programVar1 unmarshalGLboolean LinkStatus

validateStatus :: Program -> GettableStateVar Bool
validateStatus = programVar1 unmarshalGLboolean ValidateStatus

programInfoLogLength :: Program -> GettableStateVar GLsizei
programInfoLogLength = programVar1 fromIntegral ProgramInfoLogLength

numAttachedShaders :: Program -> GettableStateVar GLsizei
numAttachedShaders = programVar1 fromIntegral AttachedShaders

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
   withGLstring varName $ glBindFragDataLocation program ind

-- | query the binding of a given variable, specified by program and name. The program has to be
-- linked. The result is Nothing if an error occures or the name is not a name of a varying
-- variable. If the program hasn't been linked an 'InvalidOperation' error is generated.
getFragDataLocation :: Program -> String -> IO (Maybe DrawBufferIndex)
getFragDataLocation (Program program) varName = do
   r <- withGLstring varName $ glGetFragDataLocation program
   if r < 0
    then return Nothing
    else return . Just $ fromIntegral r
