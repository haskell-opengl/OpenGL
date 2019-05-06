{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Shaders.Uniform
-- Copyright   :  (c) Sven Panne 2006-2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module contains functions related to shader uniforms, this corresponds
-- to section 2.20.3 of the OpenGL 3.1 spec (Shader Variables).
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Shaders.Uniform (
   -- * Uniform variables
   UniformLocation(..), uniformLocation, activeUniforms, Uniform(..),
   UniformComponent,

   -- TODO: glGetUniformSubroutineuiv
) where

import Data.Maybe
import Data.StateVar
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.ByteString
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.MatrixComponent
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects
import Graphics.Rendering.OpenGL.GL.Shaders.Variables
import Graphics.Rendering.OpenGL.GL.Tensor
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.GL

--------------------------------------------------------------------------------

numActiveUniforms :: Program -> GettableStateVar GLuint
numActiveUniforms = programVar1 fromIntegral ActiveUniforms

activeUniformMaxLength :: Program -> GettableStateVar GLsizei
activeUniformMaxLength = programVar1 fromIntegral ActiveUniformMaxLength

--------------------------------------------------------------------------------

newtype UniformLocation = UniformLocation GLint
   deriving ( Eq, Ord, Show )

uniformLocation :: Program -> String -> GettableStateVar UniformLocation
uniformLocation (Program program) name =
   makeGettableStateVar $
      fmap UniformLocation $
         withGLstring name $
            glGetUniformLocation program

--------------------------------------------------------------------------------

activeUniforms :: Program -> GettableStateVar [(GLint,VariableType,String)]
activeUniforms =
   activeVars
      numActiveUniforms
      activeUniformMaxLength
      glGetActiveUniform
      unmarshalVariableType

--------------------------------------------------------------------------------

class Storable a => UniformComponent a where
   uniform1 :: UniformLocation -> a -> IO ()
   uniform2 :: UniformLocation -> a -> a -> IO ()
   uniform3 :: UniformLocation -> a -> a -> a -> IO ()
   uniform4 :: UniformLocation -> a -> a -> a -> a -> IO ()

   getUniform :: Storable (b a) => GLuint -> GLint -> Ptr (b a) -> IO ()

   uniform1v :: UniformLocation -> GLsizei -> Ptr a -> IO ()
   uniform2v :: UniformLocation -> GLsizei -> Ptr a -> IO ()
   uniform3v :: UniformLocation -> GLsizei -> Ptr a -> IO ()
   uniform4v :: UniformLocation -> GLsizei -> Ptr a -> IO ()

instance UniformComponent GLint where
   uniform1 (UniformLocation ul) = glUniform1i ul
   uniform2 (UniformLocation ul) = glUniform2i ul
   uniform3 (UniformLocation ul) = glUniform3i ul
   uniform4 (UniformLocation ul) = glUniform4i ul

   getUniform p ul = glGetUniformiv p ul . castPtr

   uniform1v (UniformLocation ul) = glUniform1iv ul
   uniform2v (UniformLocation ul) = glUniform2iv ul
   uniform3v (UniformLocation ul) = glUniform3iv ul
   uniform4v (UniformLocation ul) = glUniform4iv ul

instance UniformComponent GLuint where
   uniform1 (UniformLocation ul) = glUniform1ui ul
   uniform2 (UniformLocation ul) = glUniform2ui ul
   uniform3 (UniformLocation ul) = glUniform3ui ul
   uniform4 (UniformLocation ul) = glUniform4ui ul

   getUniform p ul = glGetUniformuiv p ul . castPtr

   uniform1v (UniformLocation ul) = glUniform1uiv ul
   uniform2v (UniformLocation ul) = glUniform2uiv ul
   uniform3v (UniformLocation ul) = glUniform3uiv ul
   uniform4v (UniformLocation ul) = glUniform4uiv ul

instance UniformComponent GLfloat where
   uniform1 (UniformLocation ul) = glUniform1f ul
   uniform2 (UniformLocation ul) = glUniform2f ul
   uniform3 (UniformLocation ul) = glUniform3f ul
   uniform4 (UniformLocation ul) = glUniform4f ul

   getUniform p ul = glGetUniformfv p ul . castPtr

   uniform1v (UniformLocation ul) = glUniform1fv ul
   uniform2v (UniformLocation ul) = glUniform2fv ul
   uniform3v (UniformLocation ul) = glUniform3fv ul
   uniform4v (UniformLocation ul) = glUniform4fv ul

instance UniformComponent GLdouble where
   uniform1 (UniformLocation ul) = glUniform1d ul
   uniform2 (UniformLocation ul) = glUniform2d ul
   uniform3 (UniformLocation ul) = glUniform3d ul
   uniform4 (UniformLocation ul) = glUniform4d ul

   getUniform p ul = glGetUniformdv p ul . castPtr

   uniform1v (UniformLocation ul) = glUniform1dv ul
   uniform2v (UniformLocation ul) = glUniform2dv ul
   uniform3v (UniformLocation ul) = glUniform3dv ul
   uniform4v (UniformLocation ul) = glUniform4dv ul

--------------------------------------------------------------------------------

class Uniform a where
   uniform :: UniformLocation -> StateVar a
   uniformv :: UniformLocation -> GLsizei -> Ptr a -> IO ()

maxComponentSize :: Int
maxComponentSize = sizeOf (undefined :: GLint) `max` sizeOf (undefined :: GLfloat)

maxNumComponents :: Int
maxNumComponents = 16

maxUniformBufferSize :: Int
maxUniformBufferSize = maxComponentSize * maxNumComponents

makeUniformVar :: (UniformComponent a, Storable (b a))
               => (UniformLocation -> b a -> IO ())
               -> UniformLocation -> StateVar (b a)
makeUniformVar setter location = makeStateVar getter (setter location)
   where getter = allocaBytes maxUniformBufferSize $ \buf -> do
                     getUniformWith getUniform location buf
                     peek buf

single :: (UniformLocation -> StateVar (Vertex1 a))
       -> (UniformLocation -> StateVar a)
single var location = makeStateVar (do Vertex1 x <- get (var location); return x)
                                   (\x -> var location $= Vertex1 x)

instance Uniform GLfloat where
   uniform = single uniform
   uniformv = uniform1v

instance Uniform GLint where
   uniform = single uniform
   uniformv = uniform1v

instance Uniform GLuint where
   uniform = single uniform
   uniformv = uniform1v

instance Uniform GLdouble where
   uniform = single uniform
   uniformv = uniform1v

instance UniformComponent a => Uniform (Vertex1 a) where
   uniform = makeUniformVar $ \location (Vertex1 x) -> uniform1 location x
   uniformv location count = uniform1v location count . (castPtr :: Ptr (Vertex1 b) -> Ptr b)

instance UniformComponent a => Uniform (Vertex2 a) where
   uniform = makeUniformVar $ \location (Vertex2 x y) -> uniform2 location x y
   uniformv location count = uniform2v location count . (castPtr :: Ptr (Vertex2 b) -> Ptr b)

instance UniformComponent a => Uniform (Vertex3 a) where
   uniform = makeUniformVar $ \location (Vertex3 x y z) -> uniform3 location x y z
   uniformv location count = uniform3v location count . (castPtr :: Ptr (Vertex3 b) -> Ptr b)

instance UniformComponent a => Uniform (Vertex4 a) where
   uniform = makeUniformVar $ \location (Vertex4 x y z w) -> uniform4 location x y z w
   uniformv location count = uniform4v location count . (castPtr :: Ptr (Vertex4 b) -> Ptr b)

instance UniformComponent a => Uniform (Vector1 a) where
   uniform = makeUniformVar $ \location (Vector1 x) -> uniform1 location x
   uniformv location count = uniform1v location count . (castPtr :: Ptr (Vector1 b) -> Ptr b)

instance UniformComponent a => Uniform (Vector2 a) where
   uniform = makeUniformVar $ \location (Vector2 x y) -> uniform2 location x y
   uniformv location count = uniform2v location count . (castPtr :: Ptr (Vector2 b) -> Ptr b)

instance UniformComponent a => Uniform (Vector3 a) where
   uniform = makeUniformVar $ \location (Vector3 x y z) -> uniform3 location x y z
   uniformv location count = uniform3v location count . (castPtr :: Ptr (Vector3 b) -> Ptr b)

instance UniformComponent a => Uniform (Vector4 a) where
   uniform = makeUniformVar $ \location (Vector4 x y z w) -> uniform4 location x y z w
   uniformv location count = uniform4v location count . (castPtr :: Ptr (Vector4 b) -> Ptr b)

instance UniformComponent a => Uniform (TexCoord1 a) where
   uniform = makeUniformVar $ \location (TexCoord1 s) -> uniform1 location s
   uniformv location count = uniform1v location count . (castPtr :: Ptr (TexCoord1 b) -> Ptr b)

instance UniformComponent a => Uniform (TexCoord2 a) where
   uniform = makeUniformVar $ \location (TexCoord2 s t) -> uniform2 location s t
   uniformv location count = uniform2v location count . (castPtr :: Ptr (TexCoord2 b) -> Ptr b)

instance UniformComponent a => Uniform (TexCoord3 a) where
   uniform = makeUniformVar $ \location (TexCoord3 s t r) -> uniform3 location s t  r
   uniformv location count = uniform3v location count . (castPtr :: Ptr (TexCoord3 b) -> Ptr b)

instance UniformComponent a => Uniform (TexCoord4 a) where
   uniform = makeUniformVar $ \location (TexCoord4 s t r q) -> uniform4 location s t  r q
   uniformv location count = uniform4v location count . (castPtr :: Ptr (TexCoord4 b) -> Ptr b)

instance UniformComponent a => Uniform (Normal3 a) where
   uniform = makeUniformVar $ \location (Normal3 x y z) -> uniform3 location x y z
   uniformv location count = uniform3v location count . (castPtr :: Ptr (Normal3 b) -> Ptr b)

instance UniformComponent a => Uniform (FogCoord1 a) where
   uniform = makeUniformVar $ \location (FogCoord1 c) -> uniform1 location c
   uniformv location count = uniform1v location count . (castPtr :: Ptr (FogCoord1 b) -> Ptr b)

instance UniformComponent a => Uniform (Color3 a) where
   uniform = makeUniformVar $ \location (Color3 r g b) -> uniform3 location r g b
   uniformv location count = uniform3v location count . (castPtr :: Ptr (Color3 b) -> Ptr b)

instance UniformComponent a => Uniform (Color4 a) where
   uniform = makeUniformVar $ \location (Color4 r g b a) -> uniform4 location r g b a
   uniformv location count = uniform4v location count . (castPtr :: Ptr (Color4 b) -> Ptr b)

instance UniformComponent a => Uniform (Index1 a) where
   uniform = makeUniformVar $ \location (Index1 i) -> uniform1 location i
   uniformv location count = uniform1v location count . (castPtr :: Ptr (Index1 b) -> Ptr b)

-- Nasty instance declaration as TextureUnit is not of the form Storable (b a) as required for
-- getUniform. Even worse is that it requires the `GLint` uniforms while it is an enum or
-- uint.
instance Uniform TextureUnit where
    uniform loc = makeStateVar getter setter
        where getter = allocaBytes (sizeOf (undefined :: GLint))  $ \buf -> do
                          getUniformWith glGetUniformiv loc buf
                          fmap (TextureUnit . fromIntegral) $ peek buf
              setter (TextureUnit tu) = uniform1 loc (fromIntegral tu :: GLint)
    uniformv location count = uniform1v location count . (castPtr :: Ptr TextureUnit -> Ptr GLint)

-- | Note: 'uniformv' expects all matrices to be in 'ColumnMajor' form.
instance MatrixComponent a => Uniform (GLmatrix a) where
   uniform loc@(UniformLocation ul) = makeStateVar getter setter
      where getter = withNewMatrix ColumnMajor $ getUniformWith getUniformv loc
            setter m = withMatrix m $ uniformMatrix4v ul 1 . isRowMajor
   uniformv (UniformLocation ul) count buf =
      uniformMatrix4v ul count (marshalGLboolean False) (castPtr buf `asTypeOf` elemType buf)
      where elemType = undefined :: MatrixComponent c => Ptr (GLmatrix c) -> Ptr c

isRowMajor :: MatrixOrder -> GLboolean
isRowMajor = marshalGLboolean . (RowMajor ==)

getUniformWith :: (GLuint -> GLint -> Ptr a -> IO ()) -> UniformLocation -> Ptr a -> IO ()
getUniformWith getter (UniformLocation ul) buf = do
   program <- fmap (programID . fromJust) $ get currentProgram
   getter program ul buf

--------------------------------------------------------------------------------
