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
   currentRasterPosition, RasterPosComponent, RasterPos(..),
   WindowPosComponent, WindowPos(..),
   currentRasterDistance, currentRasterColor, currentRasterIndex,
   currentRasterTexCoords, currentRasterPositionValid
) where

import Data.Int
import Foreign.Ptr ( Ptr, castPtr )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLshort, GLint, GLfloat, GLdouble )
import Graphics.Rendering.OpenGL.GL.GLboolean ( unmarshalGLboolean )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetCurrentRasterPosition,GetCurrentRasterDistance,
            GetCurrentRasterColor,GetCurrentRasterIndex,
            GetCurrentRasterTextureCoords,GetCurrentRasterPositionValid),
   getBoolean1, getInteger1, getFloat1, getFloat4 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.VertexSpec (
   Vertex2(..), Vertex3(..), Vertex4(..), TexCoord4(..),
   Color4(..), Index1(..) )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"
#include "HsOpenGLTypes.h"

--------------------------------------------------------------------------------

currentRasterPosition :: StateVar (Vertex4 GLfloat)
currentRasterPosition =
   makeStateVar (getFloat4 Vertex4 GetCurrentRasterPosition) rasterPos

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

instance RasterPosComponent GLshort_ where
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

instance RasterPosComponent GLint_ where
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

instance RasterPosComponent GLfloat_ where
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

instance RasterPosComponent GLdouble_ where
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

EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4",glWindowPos2sARB,GLshort -> GLshort -> IO ())
EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4",glWindowPos3sARB,GLshort -> GLshort -> GLshort -> IO ())
EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4",glWindowPos2svARB,Ptr GLshort -> IO ())
EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4",glWindowPos3svARB,Ptr GLshort -> IO ())

instance WindowPosComponent GLshort_ where
   windowPos2 = glWindowPos2sARB
   windowPos3 = glWindowPos3sARB

   windowPos2v = glWindowPos2svARB
   windowPos3v = glWindowPos3svARB

--------------------------------------------------------------------------------

EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4",glWindowPos2iARB,GLint -> GLint -> IO ())
EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4",glWindowPos3iARB,GLint -> GLint -> GLint -> IO ())
EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4",glWindowPos2ivARB,Ptr GLint -> IO ())
EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4",glWindowPos3ivARB,Ptr GLint -> IO ())

instance WindowPosComponent GLint_ where
   windowPos2 = glWindowPos2iARB
   windowPos3 = glWindowPos3iARB

   windowPos2v = glWindowPos2ivARB
   windowPos3v = glWindowPos3ivARB

--------------------------------------------------------------------------------

EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4",glWindowPos2fARB,GLfloat -> GLfloat -> IO ())
EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4",glWindowPos3fARB,GLfloat -> GLfloat -> GLfloat -> IO ())
EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4",glWindowPos2fvARB,Ptr GLfloat -> IO ())
EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4",glWindowPos3fvARB,Ptr GLfloat -> IO ())

instance WindowPosComponent GLfloat_ where
   windowPos2 = glWindowPos2fARB
   windowPos3 = glWindowPos3fARB

   windowPos2v = glWindowPos2fvARB
   windowPos3v = glWindowPos3fvARB

--------------------------------------------------------------------------------

EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4",glWindowPos2dARB,GLdouble -> GLdouble -> IO ())
EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4",glWindowPos3dARB,GLdouble -> GLdouble -> GLdouble -> IO ())
EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4",glWindowPos2dvARB,Ptr GLdouble -> IO ())
EXTENSION_ENTRY("GL_ARB_window_pos or OpenGL 1.4",glWindowPos3dvARB,Ptr GLdouble -> IO ())

instance WindowPosComponent GLdouble_ where
   windowPos2 = glWindowPos2dARB
   windowPos3 = glWindowPos3dARB

   windowPos2v = glWindowPos2dvARB
   windowPos3v = glWindowPos3dvARB

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

--------------------------------------------------------------------------------

currentRasterDistance :: GettableStateVar GLfloat
currentRasterDistance =
   makeGettableStateVar (getFloat1 id GetCurrentRasterDistance)

currentRasterColor :: GettableStateVar (Color4 GLfloat)
currentRasterColor =
   makeGettableStateVar (getFloat4 Color4 GetCurrentRasterColor)

currentRasterIndex :: GettableStateVar (Index1 GLint)
currentRasterIndex =
   makeGettableStateVar (getInteger1 Index1 GetCurrentRasterIndex)

currentRasterTexCoords :: GettableStateVar (TexCoord4 GLfloat)
currentRasterTexCoords =
   makeGettableStateVar (getFloat4 TexCoord4 GetCurrentRasterTextureCoords)

currentRasterPositionValid :: GettableStateVar Bool
currentRasterPositionValid =
   makeGettableStateVar
      (getBoolean1 unmarshalGLboolean GetCurrentRasterPositionValid)
