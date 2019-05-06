--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.Matrix
-- Copyright   :  (c) Sven Panne 2002-2019
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to chapter 4 (Matrix Manipulation) of the GLU specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GLU.Matrix (
   ortho2D, perspective, lookAt, pickMatrix,
   project, unProject, unProject4
) where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.GLU
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.Tensor
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.GL

--------------------------------------------------------------------------------
-- matrix setup

ortho2D :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()
ortho2D = gluOrtho2D


perspective :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()
perspective = gluPerspective

lookAt :: Vertex3 GLdouble -> Vertex3 GLdouble -> Vector3 GLdouble -> IO ()
lookAt (Vertex3 eyeX    eyeY    eyeZ)
       (Vertex3 centerX centerY centerZ)
       (Vector3 upX     upY     upZ) =
   gluLookAt eyeX eyeY eyeZ centerX centerY centerZ upX upY upZ

pickMatrix ::
   (GLdouble, GLdouble) -> (GLdouble, GLdouble) -> (Position, Size) -> IO ()
pickMatrix (x, y) (w, h) viewPort =
   withViewport viewPort $ gluPickMatrix x y w h

--------------------------------------------------------------------------------
-- coordinate projection

project ::
      Matrix m
   => Vertex3 GLdouble -> m GLdouble -> m GLdouble -> (Position, Size)
   -> IO (Vertex3 GLdouble)
project (Vertex3 objX objY objZ) model proj viewPort =
   withColumnMajor model $ \modelBuf ->
   withColumnMajor proj $ \projBuf ->
   withViewport viewPort $ \viewBuf ->
   getVertex3 $ gluProject objX objY objZ modelBuf projBuf viewBuf

unProject ::
      Matrix m
   => Vertex3 GLdouble -> m GLdouble -> m GLdouble -> (Position, Size)
   -> IO (Vertex3 GLdouble)
unProject (Vertex3 objX objY objZ) model proj viewPort =
   withColumnMajor model $ \modelBuf ->
   withColumnMajor proj $ \projBuf ->
   withViewport viewPort $ \viewBuf ->
   getVertex3 $ gluUnProject objX objY objZ modelBuf projBuf viewBuf

unProject4 ::
      Matrix m
   => Vertex4 GLdouble -> m GLdouble -> m GLdouble -> (Position, Size)
   -> GLclampd -> GLclampd
   -> IO (Vertex4 GLdouble)
unProject4 (Vertex4 objX objY objZ clipW) model proj viewPort near far =
   withColumnMajor model $ \modelBuf ->
   withColumnMajor proj $ \projBuf ->
   withViewport viewPort $ \viewBuf ->
   getVertex4 $
      gluUnProject4 objX objY objZ clipW modelBuf projBuf viewBuf near far

--------------------------------------------------------------------------------

withViewport :: (Position, Size) -> (Ptr GLint -> IO a ) -> IO a
withViewport (Position x y, Size w h) =
   withArray [ x, y, fromIntegral w, fromIntegral h ]

withColumnMajor :: (Matrix m, MatrixComponent c) => m c -> (Ptr c -> IO b) -> IO b
withColumnMajor mat act = withMatrix mat juggle
   where juggle ColumnMajor p = act p
         juggle RowMajor    p = do
            transposedElems <- mapM (peekElemOff p) [ 0, 4,  8, 12,
                                                      1, 5,  9, 13,
                                                      2, 6, 10, 14,
                                                      3, 7, 11, 15 ]
            withArray transposedElems act

getVertex3 ::
      (Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> IO GLint)
   -> IO (Vertex3 GLdouble)
getVertex3 act =
   alloca $ \xBuf ->
   alloca $ \yBuf ->
   alloca $ \zBuf -> do
   ok <- act xBuf yBuf zBuf
   if unmarshalGLboolean ok
      then do x <- peek xBuf
              y <- peek yBuf
              z <- peek zBuf
              return $ Vertex3 x y z
      else do recordInvalidValue
              return $ Vertex3 0 0 0

getVertex4 ::
      (Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> IO GLint)
   -> IO (Vertex4 GLdouble)
getVertex4 act =
   alloca $ \xBuf ->
   alloca $ \yBuf ->
   alloca $ \zBuf ->
   alloca $ \wBuf -> do
   ok <- act xBuf yBuf zBuf wBuf
   if unmarshalGLboolean ok
      then do x <- peek xBuf
              y <- peek yBuf
              z <- peek zBuf
              w <- peek wBuf
              return $ Vertex4 x y z w
      else do recordInvalidValue
              return $ Vertex4 0 0 0 0
