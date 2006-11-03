--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Shaders
-- Copyright   :  (c) Sven Panne 2002-2006
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to sections 2.15 (Vertex Shaders) and section 3.11
-- (Fragment Shaders) of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Shaders (
   -- * Shader Objects
   Shader, VertexShader, FragmentShader,
   shaderDeleteStatus, shaderSource, compileShader, compileStatus, shaderInfoLog,

   -- * Program Objects
   Program,
   programDeleteStatus, attachShader, detachShader, attachedShaders,
   linkProgram, linkStatus, programInfoLog, validateProgram, validateStatus,
   currentProgram,

   -- * Implementation limits related to GLSL
   maxCombinedTextureImageUnits, maxDrawBuffers, maxFragmentUniformComponents,
   maxTextureCoords, maxTextureImageUnits, maxVaryingFloats, maxVertexAttribs,
   maxVertexTextureImageUnits, maxVertexUniformComponents
) where

import Control.Monad ( replicateM, mapM_ )
import Control.Monad.Fix ( MonadFix(..) )
import Data.List ( genericLength )
import Foreign.C.String ( peekCAStringLen, withCAStringLen )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( allocaArray, withArray )
import Foreign.Marshal.Utils ( withMany )
import Foreign.Ptr ( Ptr, castPtr, nullPtr )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLboolean, GLchar, GLint, GLuint, GLenum, GLsizei )
import Graphics.Rendering.OpenGL.GL.BufferObjects ( ObjectName(..) )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.GLboolean ( unmarshalGLboolean )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek1 )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetMaxCombinedTextureImageUnits,GetMaxDrawBuffers,
            GetMaxFragmentUniformComponents,GetMaxTextureCoords,
            GetMaxTextureImageUnits,GetMaxVaryingFloats,GetMaxVertexAttribs,
            GetMaxVertexTextureImageUnits,GetMaxVertexUniformComponents,
            GetCurrentProgram),
   getInteger1, getSizei1 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasGetter(get), GettableStateVar, makeGettableStateVar, StateVar,
   makeStateVar )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"

--------------------------------------------------------------------------------

type GLStringLen = (Ptr GLchar, GLsizei)

peekGLstringLen :: GLStringLen -> IO String
peekGLstringLen (p,l) = peekCAStringLen (castPtr p, fromIntegral l)

withGLStringLen :: String -> (GLStringLen -> IO a) -> IO a
withGLStringLen s act =
   withCAStringLen s $ \(p,len) ->
      act (castPtr p, fromIntegral len)

--------------------------------------------------------------------------------

newtype VertexShader = VertexShader { vertexShaderID :: GLuint }
   deriving ( Eq, Ord, Show )

newtype FragmentShader = FragmentShader { fragmentShaderID :: GLuint }
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

class Shader s where
   shaderID :: s -> GLuint
   makeShader :: GLuint -> s
   shaderType :: s -> GLenum

instance Shader VertexShader where
   makeShader = VertexShader
   shaderID = vertexShaderID
   shaderType = const 0x8B31

instance Shader FragmentShader where
   makeShader = FragmentShader
   shaderID = fragmentShaderID
   shaderType = const 0x8B30

--------------------------------------------------------------------------------

instance ObjectName VertexShader where
   genObjectNames = genShaderNames
   deleteObjectNames = deleteShaderNames
   isObjectName = isShaderName

instance ObjectName FragmentShader where
   genObjectNames = genShaderNames
   deleteObjectNames = deleteShaderNames
   isObjectName = isShaderName

genShaderNames :: Shader s => Int -> IO [s]
genShaderNames n = replicateM n createShader

createShader :: Shader s => IO s
createShader = mfix (fmap makeShader . glCreateShader . shaderType)

deleteShaderNames :: Shader s => [s] -> IO ()
deleteShaderNames = mapM_ (glDeleteShader . shaderID)

isShaderName :: Shader s => s -> IO Bool
isShaderName = fmap unmarshalGLboolean . glIsShader . shaderID

EXTENSION_ENTRY("OpenGL 2.0",glCreateShader,GLenum -> IO GLuint)
EXTENSION_ENTRY("OpenGL 2.0",glDeleteShader,GLuint -> IO ())
EXTENSION_ENTRY("OpenGL 2.0",glIsShader,GLuint -> IO GLboolean)

--------------------------------------------------------------------------------

compileShader :: Shader s => s -> IO ()
compileShader = glCompileShader . shaderID

EXTENSION_ENTRY("OpenGL 2.0",glCompileShader,GLuint -> IO ())

--------------------------------------------------------------------------------

shaderSource :: Shader s => s -> StateVar [String]
shaderSource s = makeStateVar (getShaderSource s) (setShaderSource s)

setShaderSource :: Shader s => s -> [String] -> IO ()
setShaderSource s srcs = do
   let len = genericLength srcs
   withMany withGLStringLen srcs $ \charBufsAndLengths -> do
      let (charBufs, lengths) = unzip charBufsAndLengths
      withArray charBufs $ \charBufsBuf ->
         withArray lengths $ \lengthsBuf ->
            glShaderSource (shaderID s) len charBufsBuf lengthsBuf

EXTENSION_ENTRY("OpenGL 2.0",glShaderSource,GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLint -> IO ())

getShaderSource :: Shader s => s -> IO [String]
getShaderSource s = do
   src <- getString (get (shaderVar fromIntegral ShaderSourceLength s))
                    (glGetShaderSource (shaderID s))
   return [src]

EXTENSION_ENTRY("OpenGL 2.0",glGetShaderSource,GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())

getString :: IO GLsizei -> (GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) -> IO String
getString getLength getStr = do
   len <- getLength
   allocaArray (fromIntegral len) $ \buf -> do
      getStr len nullPtr buf
      peekGLstringLen (buf, len)

--------------------------------------------------------------------------------

shaderInfoLog :: Shader s => s -> GettableStateVar String
shaderInfoLog s =
   makeGettableStateVar $
      getString (get (shaderVar fromIntegral ShaderInfoLogLength s))
                (glGetShaderInfoLog (shaderID s))

EXTENSION_ENTRY("OpenGL 2.0",glGetShaderInfoLog,GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())

--------------------------------------------------------------------------------

shaderDeleteStatus :: Shader s => s -> GettableStateVar Bool
shaderDeleteStatus = shaderVar unmarshalGLboolean ShaderDeleteStatus

compileStatus :: Shader s => s -> GettableStateVar Bool
compileStatus = shaderVar unmarshalGLboolean CompileStatus

--------------------------------------------------------------------------------

data GetShaderPName =
     ShaderDeleteStatus
   | CompileStatus
   | ShaderInfoLogLength
   | ShaderSourceLength

marshalGetShaderPName :: GetShaderPName -> GLenum
marshalGetShaderPName x = case x of
   ShaderDeleteStatus -> 0x8B80
   CompileStatus -> 0x8B81
   ShaderInfoLogLength -> 0x8B84
   ShaderSourceLength -> 0x8B88

shaderVar :: Shader s => (GLint -> a) -> GetShaderPName -> s -> GettableStateVar a
shaderVar f p shader =
   makeGettableStateVar $
      alloca $ \buf -> do
         glGetShaderiv (shaderID shader) (marshalGetShaderPName p) buf
         peek1 f buf

EXTENSION_ENTRY("OpenGL 2.0",glGetShaderiv,GLuint -> GLenum -> Ptr GLint -> IO ())

--------------------------------------------------------------------------------

newtype Program = Program { programID :: GLuint }
   deriving ( Eq, Ord, Show )

instance ObjectName Program where
   genObjectNames n = replicateM n $ fmap Program glCreateProgram
   deleteObjectNames = mapM_ (glDeleteProgram . programID)
   isObjectName = fmap unmarshalGLboolean . glIsProgram . programID

EXTENSION_ENTRY("OpenGL 2.0",glCreateProgram,IO GLuint)
EXTENSION_ENTRY("OpenGL 2.0",glDeleteProgram,GLuint -> IO ())
EXTENSION_ENTRY("OpenGL 2.0",glIsProgram,GLuint -> IO GLboolean)

--------------------------------------------------------------------------------

attachShader :: Shader s => Program -> s -> IO ()
attachShader program = glAttachShader program . shaderID

EXTENSION_ENTRY("OpenGL 2.0",glAttachShader,Program -> GLuint -> IO ())

detachShader :: Shader s => Program -> s -> IO ()
detachShader program = glDetachShader program . shaderID

EXTENSION_ENTRY("OpenGL 2.0",glDetachShader,Program -> GLuint -> IO ())

attachedShaders :: Program -> GettableStateVar ([VertexShader],[FragmentShader])
attachedShaders p =
   makeGettableStateVar $ do
      glGetAttachedShaders (programID p) 0 nullPtr nullPtr -- ToDo!!!
      return ([],[])

EXTENSION_ENTRY("OpenGL 2.0",glGetAttachedShaders,GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLuint -> IO ())

--------------------------------------------------------------------------------

linkProgram :: Program -> IO ()
linkProgram = glLinkProgram

EXTENSION_ENTRY("OpenGL 2.0",glLinkProgram,Program -> IO ())

currentProgram :: StateVar (Maybe Program)
currentProgram =
   makeStateVar
      (do p <- getInteger1 fromIntegral GetCurrentProgram
          return (if p == 0 then Nothing else Just (Program p)))
      (glUseProgram . maybe (Program 0) id)

EXTENSION_ENTRY("OpenGL 2.0",glUseProgram,Program -> IO ())

validateProgram :: Program -> IO ()
validateProgram = glValidateProgram

EXTENSION_ENTRY("OpenGL 2.0",glValidateProgram,Program -> IO ())

programInfoLog :: Program -> GettableStateVar String
programInfoLog p =
   makeGettableStateVar $
      getString (get (programVar id ProgramInfoLogLength p))
                (glGetProgramInfoLog (programID p))

EXTENSION_ENTRY("OpenGL 2.0",glGetProgramInfoLog,GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())

--------------------------------------------------------------------------------

programDeleteStatus :: Program -> GettableStateVar Bool
programDeleteStatus = programVar unmarshalGLboolean ProgramDeleteStatus

linkStatus :: Program -> GettableStateVar Bool
linkStatus = programVar unmarshalGLboolean LinkStatus

validateStatus :: Program -> GettableStateVar Bool
validateStatus = programVar unmarshalGLboolean ValidateStatus

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

marshalGetProgramPName :: GetProgramPName -> GLenum
marshalGetProgramPName x = case x of
   ProgramDeleteStatus -> 0x8B80
   LinkStatus -> 0x8B82
   ValidateStatus -> 0x8B83
   ProgramInfoLogLength -> 0x8B84
   AttachedShaders -> 0x8B85
   ActiveAttributes -> 0x8B89
   ActiveAttributeMaxLength -> 0x8B8A
   ActiveUniforms -> 0x8B86
   ActiveUniformMaxLength -> 0x8B87

programVar :: (GLint -> a) -> GetProgramPName -> Program -> GettableStateVar a
programVar f p program =
   makeGettableStateVar $
      alloca $ \buf -> do
         glGetProgramiv (programID program) (marshalGetProgramPName p) buf
         peek1 f buf

EXTENSION_ENTRY("OpenGL 2.0",glGetProgramiv,GLuint -> GLenum -> Ptr GLint -> IO ())

--------------------------------------------------------------------------------

maxCombinedTextureImageUnits :: GettableStateVar GLint
maxCombinedTextureImageUnits = getLimit GetMaxCombinedTextureImageUnits

maxDrawBuffers :: GettableStateVar GLint
maxDrawBuffers = getLimit GetMaxDrawBuffers

maxFragmentUniformComponents :: GettableStateVar GLint
maxFragmentUniformComponents = getLimit GetMaxFragmentUniformComponents

maxTextureCoords :: GettableStateVar GLint
maxTextureCoords = getLimit GetMaxTextureCoords

maxTextureImageUnits :: GettableStateVar GLint
maxTextureImageUnits = getLimit GetMaxTextureImageUnits

maxVaryingFloats :: GettableStateVar GLint
maxVaryingFloats = getLimit GetMaxVaryingFloats

maxVertexAttribs :: GettableStateVar GLint
maxVertexAttribs = getLimit GetMaxVertexAttribs

maxVertexTextureImageUnits :: GettableStateVar GLint
maxVertexTextureImageUnits = getLimit GetMaxVertexTextureImageUnits

maxVertexUniformComponents :: GettableStateVar GLint
maxVertexUniformComponents = getLimit GetMaxVertexUniformComponents

getLimit :: GetPName -> GettableStateVar GLsizei
getLimit = makeGettableStateVar . getSizei1 id
