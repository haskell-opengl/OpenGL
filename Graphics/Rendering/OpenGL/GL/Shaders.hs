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
   Shader, VertexShader(VertexShader), FragmentShader(FragmentShader),
   compileShader,
   Program(Program),
   attachShader, detachShader, linkProgram, useProgram, validateProgram
) where

import Control.Monad ( replicateM, mapM_ )
import Control.Monad.Fix ( MonadFix(..) )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLboolean, GLchar, GLint, GLuint, GLenum, GLsizei )
import Graphics.Rendering.OpenGL.GL.BufferObjects ( ObjectName(..) )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.GLboolean ( unmarshalGLboolean )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"

--------------------------------------------------------------------------------

class Shader a where
   shaderID :: a -> GLuint
   makeShader :: GLuint -> a
   shaderType :: a -> GLenum

--------------------------------------------------------------------------------

newtype VertexShader = VertexShader { vertexShaderID :: GLuint }
   deriving ( Eq, Ord, Show )

instance Shader VertexShader where
   makeShader = VertexShader
   shaderID = vertexShaderID
   shaderType = const 0x8B31

newtype FragmentShader = FragmentShader { fragmentShaderID :: GLuint }
   deriving ( Eq, Ord, Show )

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

genShaderNames :: Shader a => Int -> IO [a]
genShaderNames n = replicateM n createShader

createShader :: Shader a => IO a
createShader = mfix (fmap makeShader . glCreateShader . shaderType)

deleteShaderNames :: Shader a => [a] -> IO ()
deleteShaderNames = mapM_ (glDeleteShader . shaderID)

isShaderName :: Shader a => a -> IO Bool
isShaderName = fmap unmarshalGLboolean . glIsShader . shaderID

EXTENSION_ENTRY("OpenGL 2.0",glCreateShader,GLenum -> IO GLuint)
EXTENSION_ENTRY("OpenGL 2.0",glDeleteShader,GLuint -> IO ())
EXTENSION_ENTRY("OpenGL 2.0",glIsShader,GLuint -> IO GLboolean)

--------------------------------------------------------------------------------

compileShader :: Shader a => a -> IO ()
compileShader = glCompileShader . shaderID

EXTENSION_ENTRY("OpenGL 2.0",glCompileShader,GLuint -> IO ())

EXTENSION_ENTRY("OpenGL 2.0",glShaderSource,GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLint -> IO ())
EXTENSION_ENTRY("OpenGL 2.0",glGetShaderSource,GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
EXTENSION_ENTRY("OpenGL 2.0",glGetShaderInfoLog,GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
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

attachShader :: Shader a => Program -> a -> IO ()
attachShader program = glAttachShader program . shaderID

EXTENSION_ENTRY("OpenGL 2.0",glAttachShader,Program -> GLuint -> IO ())

detachShader :: Shader a => Program -> a -> IO ()
detachShader program = glAttachShader program . shaderID

EXTENSION_ENTRY("OpenGL 2.0",glDetachShader,Program -> GLuint -> IO ())

EXTENSION_ENTRY("OpenGL 2.0",glGetAttachedShaders,GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLuint -> IO ())

--------------------------------------------------------------------------------

linkProgram :: Program -> IO ()
linkProgram = glLinkProgram

EXTENSION_ENTRY("OpenGL 2.0",glLinkProgram,Program -> IO ())

useProgram :: Program -> IO ()
useProgram = glUseProgram

EXTENSION_ENTRY("OpenGL 2.0",glUseProgram,Program -> IO ())

validateProgram :: Program -> IO ()
validateProgram = glValidateProgram

EXTENSION_ENTRY("OpenGL 2.0",glValidateProgram,Program -> IO ())

EXTENSION_ENTRY("OpenGL 2.0",glGetProgramInfoLog,GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
EXTENSION_ENTRY("OpenGL 2.0",glGetProgramiv,GLuint -> GLenum -> Ptr GLint -> IO ())
