--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.Matrix
-- Copyright   :  (c) Sven Panne 2003
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
import Graphics.Rendering.OpenGL.GL.CoordTrans (
   MatrixOrder(ColumnMajor), Matrix, MatrixElement(getMatrixElements),
   Vector3(..), Position(..), Size(..) )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Vertex3(..), Vertex4(..) )

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
pickMatrix (x, y) (w, h) (Position vx vy, Size vw vh) =
   withArray [ vx, vy, fromIntegral vw, fromIntegral vh ] $
        gluPickMatrix x y w h

foreign import CALLCONV unsafe "gluPickMatrix" gluPickMatrix ::
   GLdouble -> GLdouble -> GLdouble -> GLdouble -> Ptr GLint -> IO ()

--------------------------------------------------------------------------------
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
