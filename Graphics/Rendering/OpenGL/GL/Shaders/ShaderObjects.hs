-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects
-- Copyright   :  (c) Sven Panne 2006-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module correspons with section 2.20.1 (Shader Objects) of the OpenGL
-- 3.1 spec.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects (
   ShaderType(..), Shader, createShader, shaderType, shaderDeleteStatus,
   shaderSource, compileShader, compileStatus, shaderInfoLog
) where

import Data.List
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.GLstring
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.Shaders.Shader
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.Raw.ARB.ComputeShader
import Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

data ShaderType =
     VertexShader
   | GeometryShader
   | FragmentShader
   | ComputeShader
   deriving ( Eq, Ord, Show )

marshalShaderType :: ShaderType -> GLenum
marshalShaderType x = case x of
   VertexShader -> gl_VERTEX_SHADER
   GeometryShader -> gl_GEOMETRY_SHADER
   FragmentShader -> gl_FRAGMENT_SHADER
   ComputeShader -> gl_COMPUTE_SHADER

unmarshalShaderType :: GLenum -> ShaderType
unmarshalShaderType x
   | x == gl_VERTEX_SHADER = VertexShader
   | x == gl_GEOMETRY_SHADER = GeometryShader
   | x == gl_FRAGMENT_SHADER = FragmentShader
   | x == gl_COMPUTE_SHADER = ComputeShader
   | otherwise = error ("unmarshalShaderType: illegal value " ++ show x)

--------------------------------------------------------------------------------

createShader :: ShaderType -> IO Shader
createShader = fmap Shader . glCreateShader . marshalShaderType

--------------------------------------------------------------------------------

compileShader :: Shader -> IO ()
compileShader = glCompileShader . shaderID

--------------------------------------------------------------------------------

shaderSource :: Shader -> StateVar [String]
shaderSource shader =
   makeStateVar (getShaderSource shader) (setShaderSource shader)

setShaderSource :: Shader -> [String] -> IO ()
setShaderSource shader srcs = do
   let len = genericLength srcs
   withMany withGLstringLen srcs $ \charBufsAndLengths -> do
      let (charBufs, lengths) = unzip charBufsAndLengths
      withArray charBufs $ \charBufsBuf ->
         withArray (map fromIntegral lengths) $ \lengthsBuf ->
            glShaderSource (shaderID shader) len charBufsBuf lengthsBuf

getShaderSource :: Shader -> IO [String]
getShaderSource shader = do
   src <- get (stringQuery (shaderSourceLength shader)
                           (glGetShaderSource (shaderID shader)))
   return [src]

--------------------------------------------------------------------------------

shaderInfoLog :: Shader -> GettableStateVar String
shaderInfoLog shader =
   stringQuery (shaderInfoLogLength shader) (glGetShaderInfoLog (shaderID shader))

--------------------------------------------------------------------------------

shaderDeleteStatus :: Shader -> GettableStateVar Bool
shaderDeleteStatus = shaderVar unmarshalGLboolean ShaderDeleteStatus

compileStatus :: Shader -> GettableStateVar Bool
compileStatus = shaderVar unmarshalGLboolean CompileStatus

shaderInfoLogLength :: Shader -> GettableStateVar GLsizei
shaderInfoLogLength = shaderVar fromIntegral ShaderInfoLogLength

shaderSourceLength :: Shader -> GettableStateVar GLsizei
shaderSourceLength = shaderVar fromIntegral ShaderSourceLength

shaderType :: Shader -> GettableStateVar ShaderType
shaderType = shaderVar (unmarshalShaderType . fromIntegral) ShaderType

--------------------------------------------------------------------------------

data GetShaderPName =
     ShaderDeleteStatus
   | CompileStatus
   | ShaderInfoLogLength
   | ShaderSourceLength
   | ShaderType

marshalGetShaderPName :: GetShaderPName -> GLenum
marshalGetShaderPName x = case x of
   ShaderDeleteStatus -> gl_DELETE_STATUS
   CompileStatus -> gl_COMPILE_STATUS
   ShaderInfoLogLength -> gl_INFO_LOG_LENGTH
   ShaderSourceLength -> gl_SHADER_SOURCE_LENGTH
   ShaderType -> gl_SHADER_TYPE

shaderVar :: (GLint -> a) -> GetShaderPName -> Shader -> GettableStateVar a
shaderVar f p shader =
   makeGettableStateVar $
      alloca $ \buf -> do
         glGetShaderiv (shaderID shader) (marshalGetShaderPName p) buf
         peek1 f buf
