--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.Matrix
-- Copyright   :  (c) Sven Panne 2002
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module corresponds to chapter 4 (Matrix Manipulation) of the GLU specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GLU.Matrix (
   ortho2D, perspective, lookAt, pickMatrix,
   UnProjFunc, project, unProject
) where

import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( withArray )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable(peek) )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   unmarshalGLboolean, GLint, GLsizei, GLdouble )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Vertex4(..) )
import Graphics.Rendering.OpenGL.GL.CoordTrans (
   MatrixOrder(ColumnMajor), Matrix, MatrixElement(getMatrixElements) )

---------------------------------------------------------------------------
-- matrix setup

foreign import CALLCONV unsafe "gluOrtho2D" ortho2D ::
   GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "gluPerspective" perspective ::
   GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

lookAt :: (GLdouble, GLdouble, GLdouble)
       -> (GLdouble, GLdouble, GLdouble)
       -> (GLdouble, GLdouble, GLdouble) -> IO ()
lookAt (eyeX, eyeY, eyeZ) (centerX, centerY, centerZ) (upX, upY, upZ) =
   gluLookAt eyeX eyeY eyeZ centerX centerY centerZ upX upY upZ

foreign import CALLCONV unsafe "gluLookAt" gluLookAt ::
      GLdouble -> GLdouble -> GLdouble
   -> GLdouble -> GLdouble -> GLdouble
   -> GLdouble -> GLdouble -> GLdouble -> IO ()
pickMatrix :: (GLdouble, GLdouble) -> (GLdouble, GLdouble)
           -> (GLint, GLint) -> (GLsizei, GLsizei) -> IO ()
pickMatrix (x, y) (w, h) (vx, vy) (vw, vh) =
   with (Vertex4 vx vy (fromIntegral vw) (fromIntegral vh))
        (pickMatrixAux x y w h)

foreign import CALLCONV unsafe "gluPickMatrix" pickMatrixAux ::
   GLdouble -> GLdouble -> GLdouble -> GLdouble -> Ptr (Vertex4 GLint) -> IO ()

---------------------------------------------------------------------------
-- coordinate projection
-- TODO: Missing for GLU 1.3: gluUnProject4

type UnProjFunc =
      (GLdouble, GLdouble, GLdouble)
   -> Matrix GLdouble -> Matrix GLdouble -> (GLint, GLint) -> (GLsizei, GLsizei)
   -> IO (Maybe (GLdouble, GLdouble, GLdouble))

type UnProjFuncInternal =
      GLdouble -> GLdouble -> GLdouble
   -> Ptr GLdouble -> Ptr GLdouble -> Ptr (Vertex4 GLint)
   -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> IO GLint

project :: UnProjFunc
project = projHelper gluProject

foreign import CALLCONV unsafe "gluProject" gluProject :: UnProjFuncInternal

unProject :: UnProjFunc
unProject = projHelper gluUnProject

foreign import CALLCONV unsafe "gluUnProject" gluUnProject :: UnProjFuncInternal

projHelper :: UnProjFuncInternal -> UnProjFunc
projHelper upf (objX, objY, objZ) model proj (x, y) (w, h) = do
   modelElems <- getMatrixElements ColumnMajor model -- TODO: Improve this sometimes needless (un-)marshaling
   withArray modelElems $ \modelBuf -> do
      projElems <- getMatrixElements ColumnMajor proj
      withArray projElems $ \projBuf ->
         with (Vertex4 (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)) $ \viewBuf  ->
            alloca $ \winXBuf  ->
            alloca $ \winYBuf  ->
            alloca $ \winZBuf  -> do
            success  <- upf objX objY objZ modelBuf projBuf viewBuf winXBuf winYBuf winZBuf
            if unmarshalGLboolean (fromIntegral success)
               then do winX <- peek winXBuf
                       winY <- peek winYBuf
                       winZ <- peek winZBuf
                       return $ Just (winX, winY, winZ)
               else return Nothing
