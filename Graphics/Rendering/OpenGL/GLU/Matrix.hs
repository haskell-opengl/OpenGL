--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.Matrix
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to chapter 4 (Matrix Manipulation) of the GLU specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GLU.Matrix (
   ortho2D, perspective, lookAt, pickMatrix,
   project, unProject, unProject4
) where

import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( withArray )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable(peek,peekElemOff) )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLint, GLdouble, GLclampd )
import Graphics.Rendering.OpenGL.GL.CoordTrans (
   MatrixOrder(..), Matrix(..), MatrixComponent,
   Vector3(..), Position(..), Size(..) )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.GLboolean ( unmarshalGLboolean )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Vertex3(..), Vertex4(..) )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"

--------------------------------------------------------------------------------
-- matrix setup

foreign import CALLCONV unsafe "gluOrtho2D" ortho2D ::
   GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "gluPerspective" perspective ::
   GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

lookAt :: Vertex3 GLdouble -> Vertex3 GLdouble -> Vector3 GLdouble -> IO ()
lookAt (Vertex3 eyeX    eyeY    eyeZ)
       (Vertex3 centerX centerY centerZ)
       (Vector3 upX     upY     upZ) =
   gluLookAt eyeX eyeY eyeZ centerX centerY centerZ upX upY upZ

foreign import CALLCONV unsafe "gluLookAt" gluLookAt ::
      GLdouble -> GLdouble -> GLdouble
   -> GLdouble -> GLdouble -> GLdouble
   -> GLdouble -> GLdouble -> GLdouble -> IO ()

pickMatrix ::
   (GLdouble, GLdouble) -> (GLdouble, GLdouble) -> (Position, Size) -> IO ()
pickMatrix (x, y) (w, h) viewport =
   withViewport viewport $ gluPickMatrix x y w h

foreign import CALLCONV unsafe "gluPickMatrix" gluPickMatrix ::
   GLdouble -> GLdouble -> GLdouble -> GLdouble -> Ptr GLint -> IO ()

--------------------------------------------------------------------------------
-- coordinate projection

project ::
      Matrix m
   => Vertex3 GLdouble -> m GLdouble -> m GLdouble -> (Position, Size)
   -> IO (Maybe (Vertex3 GLdouble))
project (Vertex3 objX objY objZ) model proj viewport =
   withColumnMajor model $ \modelBuf ->
   withColumnMajor proj $ \projBuf ->
   withViewport viewport $ \viewBuf ->
   getVertex3 $ gluProject objX objY objZ modelBuf projBuf viewBuf

foreign import CALLCONV unsafe "gluProject" gluProject ::
      GLdouble -> GLdouble -> GLdouble
   -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLint
   -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> IO GLint

unProject ::
      Matrix m
   => Vertex3 GLdouble -> m GLdouble -> m GLdouble -> (Position, Size)
   -> IO (Maybe (Vertex3 GLdouble))
unProject (Vertex3 objX objY objZ) model proj viewport =
   withColumnMajor model $ \modelBuf ->
   withColumnMajor proj $ \projBuf ->
   withViewport viewport $ \viewBuf ->
   getVertex3 $ gluUnProject objX objY objZ modelBuf projBuf viewBuf

foreign import CALLCONV unsafe "gluUnProject" gluUnProject ::
      GLdouble -> GLdouble -> GLdouble
   -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLint
   -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> IO GLint

unProject4 ::
      Matrix m
   => Vertex4 GLdouble -> m GLdouble -> m GLdouble -> (Position, Size)
   -> GLclampd -> GLclampd
   -> IO (Maybe (Vertex4 GLdouble))
unProject4 (Vertex4 objX objY objZ clipW) model proj viewport near far =
   withColumnMajor model $ \modelBuf ->
   withColumnMajor proj $ \projBuf ->
   withViewport viewport $ \viewBuf ->
   getVertex4 $
      gluUnProject4 objX objY objZ clipW modelBuf projBuf viewBuf near far

EXTENSION_ENTRY("GLU 1.3",gluUnProject4,GLdouble -> GLdouble -> GLdouble -> GLdouble -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLint -> GLclampd -> GLclampd -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> IO GLint)

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
   Storable a => (Ptr a -> Ptr a -> Ptr a -> IO GLint) -> IO (Maybe (Vertex3 a))
getVertex3 act =
   alloca $ \xBuf ->
   alloca $ \yBuf ->
   alloca $ \zBuf -> do
   ok <- act xBuf yBuf zBuf
   if unmarshalGLboolean (fromIntegral ok)
      then do x <- peek xBuf
              y <- peek yBuf
              z <- peek zBuf
              return $ Just (Vertex3 x y z)
      else return Nothing

getVertex4 ::
   Storable a => (Ptr a -> Ptr a -> Ptr a -> Ptr a -> IO GLint) -> IO (Maybe (Vertex4 a))
getVertex4 act =
   alloca $ \xBuf ->
   alloca $ \yBuf ->
   alloca $ \zBuf ->
   alloca $ \wBuf -> do
   ok <- act xBuf yBuf zBuf wBuf
   if unmarshalGLboolean (fromIntegral ok)
      then do x <- peek xBuf
              y <- peek yBuf
              z <- peek zBuf
              w <- peek wBuf
              return $ Just (Vertex4 x y z w)
      else return Nothing
