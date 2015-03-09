-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Shaders.Uniform
-- Copyright   :  (c) Sven Panne 2006-2013
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

{-# LANGUAGE TypeSynonymInstances #-}

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
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects
import Graphics.Rendering.OpenGL.GL.Shaders.Variables
import Graphics.Rendering.OpenGL.GL.Tensor
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.Raw

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

   getUniform :: Storable (b a) => Program -> UniformLocation -> Ptr (b a) -> IO ()

   uniform1v :: UniformLocation -> GLsizei -> Ptr a -> IO ()
   uniform2v :: UniformLocation -> GLsizei -> Ptr a -> IO ()
   uniform3v :: UniformLocation -> GLsizei -> Ptr a -> IO ()
   uniform4v :: UniformLocation -> GLsizei -> Ptr a -> IO ()

instance UniformComponent GLint where
   uniform1 (UniformLocation ul) = glUniform1i ul
   uniform2 (UniformLocation ul) = glUniform2i ul
   uniform3 (UniformLocation ul) = glUniform3i ul
   uniform4 (UniformLocation ul) = glUniform4i ul

   getUniform (Program p) (UniformLocation ul) = glGetUniformiv p ul . castPtr

   uniform1v (UniformLocation ul) = glUniform1iv ul
   uniform2v (UniformLocation ul) = glUniform2iv ul
   uniform3v (UniformLocation ul) = glUniform3iv ul
   uniform4v (UniformLocation ul) = glUniform4iv ul

instance UniformComponent GLuint where
   uniform1 (UniformLocation ul) = glUniform1ui ul
   uniform2 (UniformLocation ul) = glUniform2ui ul
   uniform3 (UniformLocation ul) = glUniform3ui ul
   uniform4 (UniformLocation ul) = glUniform4ui ul

   getUniform (Program p) (UniformLocation ul) = glGetUniformuiv p ul . castPtr

   uniform1v (UniformLocation ul) = glUniform1uiv ul
   uniform2v (UniformLocation ul) = glUniform2uiv ul
   uniform3v (UniformLocation ul) = glUniform3uiv ul
   uniform4v (UniformLocation ul) = glUniform4uiv ul

instance UniformComponent GLfloat where
   uniform1 (UniformLocation ul) = glUniform1f ul
   uniform2 (UniformLocation ul) = glUniform2f ul
   uniform3 (UniformLocation ul) = glUniform3f ul
   uniform4 (UniformLocation ul) = glUniform4f ul

   getUniform (Program p) (UniformLocation ul) = glGetUniformfv p ul . castPtr

   uniform1v (UniformLocation ul) = glUniform1fv ul
   uniform2v (UniformLocation ul) = glUniform2fv ul
   uniform3v (UniformLocation ul) = glUniform3fv ul
   uniform4v (UniformLocation ul) = glUniform4fv ul

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
   where getter = do program <- fmap fromJust $ get currentProgram
                     allocaBytes maxUniformBufferSize $ \buf -> do
                        getUniform program location buf
                        peek buf

instance UniformComponent a => Uniform (Vertex2 a) where
   uniform = makeUniformVar $ \location (Vertex2 x y) -> uniform2 location x y
   uniformv location count = uniform2v location count . (castPtr :: Ptr (Vertex2 b) -> Ptr b)

instance UniformComponent a => Uniform (Vertex3 a) where
   uniform = makeUniformVar $ \location (Vertex3 x y z) -> uniform3 location x y z
   uniformv location count = uniform3v location count . (castPtr :: Ptr (Vertex3 b) -> Ptr b)

instance UniformComponent a => Uniform (Vertex4 a) where
   uniform = makeUniformVar $ \location (Vertex4 x y z w) -> uniform4 location x y z w
   uniformv location count = uniform4v location count . (castPtr :: Ptr (Vertex4 b) -> Ptr b)

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
    uniform loc@(UniformLocation ul)  = makeStateVar getter setter
        where setter (TextureUnit tu) = uniform1 loc (fromIntegral tu :: GLint)
              getter = do program <- fmap fromJust $ get currentProgram
                          allocaBytes (sizeOf (undefined :: GLint))  $ \buf -> do
                             glGetUniformiv (programID program) ul buf
                             tuID <- peek buf
                             return . TextureUnit $ fromIntegral tuID
    uniformv location count = uniform1v location count . (castPtr :: Ptr TextureUnit -> Ptr GLint)

--------------------------------------------------------------------------------
