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
import Foreign.Storable ( Storable(peek) )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLint, GLdouble, GLclampd )
import Graphics.Rendering.OpenGL.GL.GLboolean ( unmarshalGLboolean )
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
pickMatrix (x, y) (w, h) viewport =
   withViewport viewport $ gluPickMatrix x y w h

foreign import CALLCONV unsafe "gluPickMatrix" gluPickMatrix ::
   GLdouble -> GLdouble -> GLdouble -> GLdouble -> Ptr GLint -> IO ()

--------------------------------------------------------------------------------
-- coordinate projection
-- TODO: Missing for GLU 1.3: gluUnProject4

project ::
      Vertex3 GLdouble
   -> Matrix GLdouble -> Matrix GLdouble -> (Position, Size)
   -> IO (Maybe (Vertex3 GLdouble))
project (Vertex3 objX objY objZ) model proj viewport =
   withMatrix ColumnMajor model $ \modelBuf ->
   withMatrix ColumnMajor proj $ \projBuf ->
   withViewport viewport $ \viewBuf ->
   getVertex3 $ gluProject objX objY objZ modelBuf projBuf viewBuf

foreign import CALLCONV unsafe "gluProject" gluProject ::
      GLdouble -> GLdouble -> GLdouble
   -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLint
   -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> IO GLint

unProject ::
      Vertex3 GLdouble
   -> Matrix GLdouble -> Matrix GLdouble -> (Position, Size)
   -> IO (Maybe (Vertex3 GLdouble))
unProject (Vertex3 objX objY objZ) model proj viewport =
   withMatrix ColumnMajor model $ \modelBuf ->
   withMatrix ColumnMajor proj $ \projBuf ->
   withViewport viewport $ \viewBuf ->
   getVertex3 $ gluUnProject objX objY objZ modelBuf projBuf viewBuf

foreign import CALLCONV unsafe "gluUnProject" gluUnProject ::
      GLdouble -> GLdouble -> GLdouble
   -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLint
   -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> IO GLint

unProject4 ::
      Vertex4 GLdouble
   -> Matrix GLdouble -> Matrix GLdouble -> (Position, Size)
   -> GLclampd -> GLclampd
   -> IO (Maybe (Vertex4 GLdouble))
unProject4 (Vertex4 objX objY objZ clipW) model proj viewport near far =
   withMatrix ColumnMajor model $ \modelBuf ->
   withMatrix ColumnMajor proj $ \projBuf ->
   withViewport viewport $ \viewBuf ->
   getVertex4 $
      gluUnProject4 objX objY objZ clipW modelBuf projBuf viewBuf near far

foreign import CALLCONV unsafe "gluUnProject4" gluUnProject4 ::
      GLdouble -> GLdouble -> GLdouble -> GLdouble
   -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLint
   -> GLclampd -> GLclampd
   -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> Ptr GLdouble -> IO GLint

--------------------------------------------------------------------------------

withViewport :: (Position, Size) -> (Ptr GLint -> IO a ) -> IO a
withViewport (Position x y, Size w h) =
   withArray [ x, y, fromIntegral w, fromIntegral h ]

-- TODO: Improve this sometimes needless (un-)marshaling
withMatrix ::
   MatrixElement a => MatrixOrder -> Matrix a -> (Ptr a -> IO b) -> IO b
withMatrix order matrix act = do
   elements <- getMatrixElements order matrix
   withArray elements act

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
