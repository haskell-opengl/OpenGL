{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.CoordTrans
-- Copyright   :  (c) Sven Panne 2002-2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for handling matrix components.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.MatrixComponent where

import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.Tensor
import Graphics.GL

--------------------------------------------------------------------------------

class Storable c => MatrixComponent c where
   getMatrix :: GetPNameMatrix p => p -> Ptr c -> IO ()
   loadMatrix :: Ptr c -> IO ()
   loadTransposeMatrix :: Ptr c -> IO ()
   multMatrix_ :: Ptr c -> IO ()
   multTransposeMatrix :: Ptr c -> IO ()
   getUniformv :: GLuint -> GLint -> Ptr c -> IO ()
   uniformMatrix4v :: GLint -> GLsizei -> GLboolean -> Ptr c -> IO ()
   rotate :: c -> Vector3 c -> IO ()
   translate :: Vector3 c -> IO ()
   scale :: c -> c -> c -> IO ()

instance MatrixComponent GLfloat where
   getMatrix = getMatrixf
   loadMatrix = glLoadMatrixf
   loadTransposeMatrix = glLoadTransposeMatrixf
   multMatrix_ = glMultMatrixf
   multTransposeMatrix = glMultTransposeMatrixf
   getUniformv = glGetUniformfv
   uniformMatrix4v = glUniformMatrix4fv
   rotate a (Vector3 x y z) = glRotatef a x y z
   translate (Vector3 x y z) = glTranslatef x y z
   scale = glScalef

instance MatrixComponent GLdouble where
   getMatrix = getMatrixd
   loadMatrix = glLoadMatrixd
   loadTransposeMatrix = glLoadTransposeMatrixd
   multMatrix_ = glMultMatrixd
   multTransposeMatrix = glMultTransposeMatrixd
   getUniformv = glGetUniformdv
   uniformMatrix4v = glUniformMatrix4dv
   rotate a (Vector3 x y z) = glRotated a x y z
   translate (Vector3 x y z) = glTranslated x y z
   scale = glScaled
