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
-- This module corresponds to section 7.1 (Shader Objects) and 7.13 (Shader,
-- Program, and Program Pipeline Queries) of the OpenGL 4.4 spec.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects (
   -- * Shader Objects
   shaderCompiler,
   ShaderType(..), Shader, createShader,
   shaderSourceBS, shaderSource, compileShader, releaseShaderCompiler,

   -- * Shader Queries
   shaderType, shaderDeleteStatus, compileStatus, shaderInfoLog,
   PrecisionType, shaderPrecisionFormat,

   -- * Bytestring utilities
   packUtf8, unpackUtf8
) where

import Control.Monad
import Data.StateVar
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.ByteString
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.Shaders.Shader
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

shaderCompiler :: GettableStateVar Bool
shaderCompiler =
   makeGettableStateVar (getBoolean1 unmarshalGLboolean GetShaderCompiler)

--------------------------------------------------------------------------------

data ShaderType =
     VertexShader
   | TessControlShader
   | TessEvaluationShader
   | GeometryShader
   | FragmentShader
   | ComputeShader
   deriving ( Eq, Ord, Show )

marshalShaderType :: ShaderType -> GLenum
marshalShaderType x = case x of
   VertexShader -> gl_VERTEX_SHADER
   TessControlShader -> gl_TESS_CONTROL_SHADER
   TessEvaluationShader -> gl_TESS_EVALUATION_SHADER
   GeometryShader -> gl_GEOMETRY_SHADER
   FragmentShader -> gl_FRAGMENT_SHADER
   ComputeShader -> gl_COMPUTE_SHADER

unmarshalShaderType :: GLenum -> ShaderType
unmarshalShaderType x
   | x == gl_VERTEX_SHADER = VertexShader
   | x == gl_TESS_CONTROL_SHADER = TessControlShader
   | x == gl_TESS_EVALUATION_SHADER = TessEvaluationShader
   | x == gl_GEOMETRY_SHADER = GeometryShader
   | x == gl_FRAGMENT_SHADER = FragmentShader
   | x == gl_COMPUTE_SHADER = ComputeShader
   | otherwise = error ("unmarshalShaderType: illegal value " ++ show x)

--------------------------------------------------------------------------------

createShader :: ShaderType -> IO Shader
createShader = fmap Shader . glCreateShader . marshalShaderType

--------------------------------------------------------------------------------

-- | UTF8 encoded.
shaderSourceBS :: Shader -> StateVar ByteString
shaderSourceBS shader =
   makeStateVar (getShaderSource shader) (setShaderSource shader)

getShaderSource :: Shader -> IO ByteString
getShaderSource = stringQuery shaderSourceLength (glGetShaderSource . shaderID)

shaderSourceLength :: Shader -> GettableStateVar GLsizei
shaderSourceLength = shaderVar fromIntegral ShaderSourceLength

setShaderSource :: Shader -> ByteString -> IO ()
setShaderSource shader src =
   withByteString src $ \srcPtr srcLength ->
      with srcPtr $ \srcPtrBuf ->
         with srcLength $ \srcLengthBuf ->
            glShaderSource (shaderID shader) 1 srcPtrBuf srcLengthBuf

{-# DEPRECATED shaderSource "Use a combination of 'shaderSourceBS' and 'packUtf8' or 'unpackUtf8' instead." #-}
shaderSource :: Shader -> StateVar [String]
shaderSource shader =
   makeStateVar
     (fmap ((:[]) . unpackUtf8) $ get (shaderSourceBS shader))
     ((shaderSourceBS shader $=) . packUtf8 . concat)

--------------------------------------------------------------------------------

compileShader :: Shader -> IO ()
compileShader = glCompileShader . shaderID

releaseShaderCompiler :: IO ()
releaseShaderCompiler = glReleaseShaderCompiler

--------------------------------------------------------------------------------

shaderType :: Shader -> GettableStateVar ShaderType
shaderType = shaderVar (unmarshalShaderType . fromIntegral) ShaderType

shaderDeleteStatus :: Shader -> GettableStateVar Bool
shaderDeleteStatus = shaderVar unmarshalGLboolean ShaderDeleteStatus

compileStatus :: Shader -> GettableStateVar Bool
compileStatus = shaderVar unmarshalGLboolean CompileStatus

shaderInfoLog :: Shader -> GettableStateVar String
shaderInfoLog =
   makeGettableStateVar .
      fmap unpackUtf8 .
         stringQuery shaderInfoLogLength (glGetShaderInfoLog . shaderID)

shaderInfoLogLength :: Shader -> GettableStateVar GLsizei
shaderInfoLogLength = shaderVar fromIntegral ShaderInfoLogLength

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
      with 0 $ \buf -> do
         glGetShaderiv (shaderID shader) (marshalGetShaderPName p) buf
         peek1 f buf

--------------------------------------------------------------------------------

data PrecisionType =
     LowFloat
   | MediumFloat
   | HighFloat
   | LowInt
   | MediumInt
   | HighInt
   deriving ( Eq, Ord, Show )

marshalPrecisionType :: PrecisionType -> GLenum
marshalPrecisionType x = case x of
   LowFloat -> gl_LOW_FLOAT
   MediumFloat -> gl_MEDIUM_FLOAT
   HighFloat -> gl_HIGH_FLOAT
   LowInt -> gl_LOW_INT
   MediumInt -> gl_MEDIUM_INT
   HighInt -> gl_HIGH_INT

--------------------------------------------------------------------------------

shaderPrecisionFormat :: ShaderType
                      -> PrecisionType
                      -> GettableStateVar ((GLint,GLint),GLint)
shaderPrecisionFormat st pt =
   makeGettableStateVar $
      allocaArray 2 $ \rangeBuf ->
         alloca $ \precisionBuf -> do
            glGetShaderPrecisionFormat (marshalShaderType st)
                                       (marshalPrecisionType pt)
                                       rangeBuf
                                       precisionBuf
            liftM2 (,) (peek2 (,) rangeBuf) (peek precisionBuf)
