--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.RasterPos
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 2.12 (Current Raster Position) of the
-- OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.RasterPos (
   RasterPosComponent, RasterPos(..),
   WindowPosComponent, WindowPos(..)
) where

import Foreign.Ptr ( Ptr, castPtr )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLshort, GLint, GLfloat,, GLdouble )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.VertexSpec (
   Vertex2(..), Vertex3(..), Vertex4(..) )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"

--------------------------------------------------------------------------------

class RasterPosComponent a where
   rasterPos2 :: a -> a -> IO ()
   rasterPos3 :: a -> a -> a -> IO ()
   rasterPos4 :: a -> a -> a -> a -> IO ()

   rasterPos2v :: Ptr a -> IO ()
   rasterPos3v :: Ptr a -> IO ()
   rasterPos4v :: Ptr a -> IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glRasterPos2s" glRasterPos2s ::
   GLshort -> GLshort -> IO ()

foreign import CALLCONV unsafe "glRasterPos3s" glRasterPos3s ::
   GLshort -> GLshort -> GLshort -> IO ()

foreign import CALLCONV unsafe "glRasterPos4s" glRasterPos4s ::
   GLshort -> GLshort -> GLshort -> GLshort -> IO ()

foreign import CALLCONV unsafe "glRasterPos2sv" glRasterPos2sv ::
   Ptr GLshort -> IO ()

foreign import CALLCONV unsafe "glRasterPos3sv" glRasterPos3sv ::
   Ptr GLshort -> IO ()

foreign import CALLCONV unsafe "glRasterPos4sv" glRasterPos4sv ::
   Ptr GLshort -> IO ()

instance RasterPosComponent GLshort where
   rasterPos2 = glRasterPos2s
   rasterPos3 = glRasterPos3s
   rasterPos4 = glRasterPos4s

   rasterPos2v = glRasterPos2sv
   rasterPos3v = glRasterPos3sv
   rasterPos4v = glRasterPos4sv

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glRasterPos2i" glRasterPos2i ::
   GLint -> GLint -> IO ()

foreign import CALLCONV unsafe "glRasterPos3i" glRasterPos3i ::
   GLint -> GLint -> GLint -> IO ()

foreign import CALLCONV unsafe "glRasterPos4i" glRasterPos4i ::
   GLint -> GLint -> GLint -> GLint -> IO ()

foreign import CALLCONV unsafe "glRasterPos2iv" glRasterPos2iv ::
   Ptr GLint -> IO ()

foreign import CALLCONV unsafe "glRasterPos3iv" glRasterPos3iv ::
   Ptr GLint -> IO ()

foreign import CALLCONV unsafe "glRasterPos4iv" glRasterPos4iv ::
   Ptr GLint -> IO ()

instance RasterPosComponent GLint where
   rasterPos2 = glRasterPos2i
   rasterPos3 = glRasterPos3i
   rasterPos4 = glRasterPos4i

   rasterPos2v = glRasterPos2iv
   rasterPos3v = glRasterPos3iv
   rasterPos4v = glRasterPos4iv

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glRasterPos2f" glRasterPos2f ::
   GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glRasterPos3f" glRasterPos3f ::
   GLfloat -> GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glRasterPos4f" glRasterPos4f ::
   GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glRasterPos2fv" glRasterPos2fv ::
   Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glRasterPos3fv" glRasterPos3fv ::
   Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glRasterPos4fv" glRasterPos4fv ::
   Ptr GLfloat -> IO ()

instance RasterPosComponent GLfloat where
   rasterPos2 = glRasterPos2f
   rasterPos3 = glRasterPos3f
   rasterPos4 = glRasterPos4f

   rasterPos2v = glRasterPos2fv
   rasterPos3v = glRasterPos3fv
   rasterPos4v = glRasterPos4fv

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glRasterPos2d" glRasterPos2d ::
   GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glRasterPos3d" glRasterPos3d ::
   GLdouble -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glRasterPos4d" glRasterPos4d ::
   GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glRasterPos2dv" glRasterPos2dv ::
   Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glRasterPos3dv" glRasterPos3dv ::
   Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glRasterPos4dv" glRasterPos4dv ::
   Ptr GLdouble -> IO ()

instance RasterPosComponent GLdouble where
   rasterPos2 = glRasterPos2d
   rasterPos3 = glRasterPos3d
   rasterPos4 = glRasterPos4d

   rasterPos2v = glRasterPos2dv
   rasterPos3v = glRasterPos3dv
   rasterPos4v = glRasterPos4dv

--------------------------------------------------------------------------------

class RasterPos a where
   rasterPos  ::     a -> IO ()
   rasterPosv :: Ptr a -> IO ()

instance RasterPosComponent a => RasterPos (Vertex2 a) where
   rasterPos (Vertex2 x y) = rasterPos2 x y
   rasterPosv = rasterPos2v . (castPtr :: Ptr (Vertex2 b) -> Ptr b)

instance RasterPosComponent a => RasterPos (Vertex3 a) where
   rasterPos (Vertex3 x y z) = rasterPos3 x y z
   rasterPosv = rasterPos3v . (castPtr :: Ptr (Vertex3 b) -> Ptr b)

instance RasterPosComponent a => RasterPos (Vertex4 a) where
   rasterPos (Vertex4 x y z w) = rasterPos4 x y z w
   rasterPosv = rasterPos4v . (castPtr :: Ptr (Vertex4 b) -> Ptr b)

--------------------------------------------------------------------------------

class WindowPosComponent a where
   windowPos2 :: a -> a -> IO ()
   windowPos3 :: a -> a -> a -> IO ()

   windowPos2v :: Ptr a -> IO ()
   windowPos3v :: Ptr a -> IO ()

--------------------------------------------------------------------------------

windowPos2s :: GLshort -> GLshort -> IO ()
windowPos2s = dynWindowPos2s ptrWindowPos2s

EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4","glWindowPos2sARB",dynWindowPos2s,ptrWindowPos2s,GLshort -> GLshort -> IO ())

windowPos3s :: GLshort -> GLshort -> GLshort -> IO ()
windowPos3s = dynWindowPos3s ptrWindowPos3s

EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4","glWindowPos3sARB",dynWindowPos3s,ptrWindowPos3s,GLshort -> GLshort -> GLshort -> IO ())

windowPos2sv :: Ptr GLshort -> IO ()
windowPos2sv = dynWindowPos2sv ptrWindowPos2sv

EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4","glWindowPos2svARB",dynWindowPos2sv,ptrWindowPos2sv,Ptr GLshort -> IO ())

windowPos3sv :: Ptr GLshort -> IO ()
windowPos3sv = dynWindowPos3sv ptrWindowPos3sv

EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4","glWindowPos3svARB",dynWindowPos3sv,ptrWindowPos3sv,Ptr GLshort -> IO ())

instance WindowPosComponent GLshort where
   windowPos2 = windowPos2s
   windowPos3 = windowPos3s

   windowPos2v = windowPos2sv
   windowPos3v = windowPos3sv

--------------------------------------------------------------------------------

windowPos2i :: GLint -> GLint -> IO ()
windowPos2i = dynWindowPos2i ptrWindowPos2i

EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4","glWindowPos2iARB",dynWindowPos2i,ptrWindowPos2i,GLint -> GLint -> IO ())

windowPos3i :: GLint -> GLint -> GLint -> IO ()
windowPos3i = dynWindowPos3i ptrWindowPos3i

EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4","glWindowPos3iARB",dynWindowPos3i,ptrWindowPos3i,GLint -> GLint -> GLint -> IO ())

windowPos2iv :: Ptr GLint -> IO ()
windowPos2iv = dynWindowPos2iv ptrWindowPos2iv

EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4","glWindowPos2ivARB",dynWindowPos2iv,ptrWindowPos2iv,Ptr GLint -> IO ())

windowPos3iv :: Ptr GLint -> IO ()
windowPos3iv = dynWindowPos3iv ptrWindowPos3iv

EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4","glWindowPos3ivARB",dynWindowPos3iv,ptrWindowPos3iv,Ptr GLint -> IO ())

instance WindowPosComponent GLint where
   windowPos2 = windowPos2i
   windowPos3 = windowPos3i

   windowPos2v = windowPos2iv
   windowPos3v = windowPos3iv

--------------------------------------------------------------------------------

windowPos2f :: GLfloat -> GLfloat -> IO ()
windowPos2f = dynWindowPos2f ptrWindowPos2f

EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4","glWindowPos2fARB",dynWindowPos2f,ptrWindowPos2f,GLfloat -> GLfloat -> IO ())

windowPos3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
windowPos3f = dynWindowPos3f ptrWindowPos3f

EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4","glWindowPos3fARB",dynWindowPos3f,ptrWindowPos3f,GLfloat -> GLfloat -> GLfloat -> IO ())

windowPos2fv :: Ptr GLfloat -> IO ()
windowPos2fv = dynWindowPos2fv ptrWindowPos2fv

EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4","glWindowPos2fvARB",dynWindowPos2fv,ptrWindowPos2fv,Ptr GLfloat -> IO ())

windowPos3fv :: Ptr GLfloat -> IO ()
windowPos3fv = dynWindowPos3fv ptrWindowPos3fv

EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4","glWindowPos3fvARB",dynWindowPos3fv,ptrWindowPos3fv,Ptr GLfloat -> IO ())

instance WindowPosComponent GLfloat where
   windowPos2 = windowPos2f
   windowPos3 = windowPos3f

   windowPos2v = windowPos2fv
   windowPos3v = windowPos3fv

--------------------------------------------------------------------------------

windowPos2d :: GLdouble -> GLdouble -> IO ()
windowPos2d = dynWindowPos2d ptrWindowPos2d

EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4","glWindowPos2dARB",dynWindowPos2d,ptrWindowPos2d,GLdouble -> GLdouble -> IO ())

windowPos3d :: GLdouble -> GLdouble -> GLdouble -> IO ()
windowPos3d = dynWindowPos3d ptrWindowPos3d

EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4","glWindowPos3dARB",dynWindowPos3d,ptrWindowPos3d,GLdouble -> GLdouble -> GLdouble -> IO ())

windowPos2dv :: Ptr GLdouble -> IO ()
windowPos2dv = dynWindowPos2dv ptrWindowPos2dv

EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4","glWindowPos2dvARB",dynWindowPos2dv,ptrWindowPos2dv,Ptr GLdouble -> IO ())

windowPos3dv :: Ptr GLdouble -> IO ()
windowPos3dv = dynWindowPos3dv ptrWindowPos3dv

EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4","glWindowPos3dvARB",dynWindowPos3dv,ptrWindowPos3dv,Ptr GLdouble -> IO ())

instance WindowPosComponent GLdouble where
   windowPos2 = windowPos2d
   windowPos3 = windowPos3d

   windowPos2v = windowPos2dv
   windowPos3v = windowPos3dv

--------------------------------------------------------------------------------

class WindowPos a where
   windowPos  ::     a -> IO ()
   windowPosv :: Ptr a -> IO ()

instance WindowPosComponent a => WindowPos (Vertex2 a) where
   windowPos (Vertex2 x y) = windowPos2 x y
   windowPosv = windowPos2v . (castPtr :: Ptr (Vertex2 b) -> Ptr b)

instance WindowPosComponent a => WindowPos (Vertex3 a) where
   windowPos (Vertex3 x y z) = windowPos3 x y z
   windowPosv = windowPos3v . (castPtr :: Ptr (Vertex3 b) -> Ptr b)
