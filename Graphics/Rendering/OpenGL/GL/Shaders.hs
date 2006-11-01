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
   VertexShader(VertexShader), FragmentShader(FragmentShader), Program(Program)
) where

import Control.Monad ( replicateM, mapM_ )
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

data ShaderType =
     VertexShader'
   | FragmentShader'
   deriving ( Eq, Ord, Show )

marshalShaderType :: ShaderType -> GLenum
marshalShaderType x = case x of
   VertexShader' -> 0x8B31
   FragmentShader' -> 0x8B30

--------------------------------------------------------------------------------

newtype VertexShader = VertexShader { vertexShaderID :: GLuint }
   deriving ( Eq, Ord, Show )

instance ObjectName VertexShader where
   genObjectNames = genShaderNames VertexShader' VertexShader
   deleteObjectNames = deleteShaderNames vertexShaderID
   isObjectName = isShaderName vertexShaderID

newtype FragmentShader = FragmentShader { fragmentShaderID :: GLuint }
   deriving ( Eq, Ord, Show )

instance ObjectName FragmentShader where
   genObjectNames = genShaderNames FragmentShader' FragmentShader
   deleteObjectNames = deleteShaderNames fragmentShaderID
   isObjectName = isShaderName fragmentShaderID

genShaderNames :: ShaderType -> (GLuint -> a) -> Int -> IO [a]
genShaderNames shaderType makeShader n =
   replicateM n . fmap makeShader . glCreateShader . marshalShaderType $ shaderType

deleteShaderNames :: (a -> GLuint) -> [a] -> IO ()
deleteShaderNames shaderID = mapM_ (glDeleteShader . shaderID)

isShaderName :: (a -> GLuint) -> a -> IO Bool
isShaderName shaderID = fmap unmarshalGLboolean . glIsShader . shaderID

EXTENSION_ENTRY("OpenGL 2.0",glCreateShader,GLenum -> IO GLuint)
EXTENSION_ENTRY("OpenGL 2.0",glDeleteShader,GLuint -> IO ())
EXTENSION_ENTRY("OpenGL 2.0",glIsShader,GLuint -> IO GLboolean)

--------------------------------------------------------------------------------

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

EXTENSION_ENTRY("OpenGL 2.0",glAttachShader,GLuint -> GLuint -> IO ())
EXTENSION_ENTRY("OpenGL 2.0",glDetachShader,GLuint -> GLuint -> IO ())
EXTENSION_ENTRY("OpenGL 2.0",glGetAttachedShaders,GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLuint -> IO ())

--------------------------------------------------------------------------------

EXTENSION_ENTRY("OpenGL 2.0",glLinkProgram,GLuint -> IO ())
EXTENSION_ENTRY("OpenGL 2.0",glUseProgram,GLuint -> IO ())
EXTENSION_ENTRY("OpenGL 2.0",glValidateProgram,GLuint -> IO ())
EXTENSION_ENTRY("OpenGL 2.0",glGetProgramInfoLog,GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
EXTENSION_ENTRY("OpenGL 2.0",glGetProgramiv,GLuint -> GLenum -> Ptr GLint -> IO ())
