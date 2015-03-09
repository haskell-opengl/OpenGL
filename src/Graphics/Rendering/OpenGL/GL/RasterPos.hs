--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.RasterPos
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 2.13 (Current Raster Position) of the
-- OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances #-}

module Graphics.Rendering.OpenGL.GL.RasterPos (
   currentRasterPosition, RasterPosComponent, RasterPos(..),
   WindowPosComponent, WindowPos(..),
   currentRasterDistance, currentRasterColor, currentRasterSecondaryColor,
   currentRasterIndex, currentRasterTexCoords, currentRasterPositionValid,
   rasterPositionUnclipped
) where

import Data.StateVar
import Foreign.Ptr
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.Tensor
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.Raw

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

instance RasterPosComponent GLshort where
   rasterPos2 = glRasterPos2s
   rasterPos3 = glRasterPos3s
   rasterPos4 = glRasterPos4s

   rasterPos2v = glRasterPos2sv
   rasterPos3v = glRasterPos3sv
   rasterPos4v = glRasterPos4sv

instance RasterPosComponent GLint where
   rasterPos2 = glRasterPos2i
   rasterPos3 = glRasterPos3i
   rasterPos4 = glRasterPos4i

   rasterPos2v = glRasterPos2iv
   rasterPos3v = glRasterPos3iv
   rasterPos4v = glRasterPos4iv

instance RasterPosComponent GLfloat where
   rasterPos2 = glRasterPos2f
   rasterPos3 = glRasterPos3f
   rasterPos4 = glRasterPos4f

   rasterPos2v = glRasterPos2fv
   rasterPos3v = glRasterPos3fv
   rasterPos4v = glRasterPos4fv

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

instance WindowPosComponent GLshort where
   windowPos2 = glWindowPos2s
   windowPos3 = glWindowPos3s

   windowPos2v = glWindowPos2sv
   windowPos3v = glWindowPos3sv

instance WindowPosComponent GLint where
   windowPos2 = glWindowPos2i
   windowPos3 = glWindowPos3i

   windowPos2v = glWindowPos2iv
   windowPos3v = glWindowPos3iv

instance WindowPosComponent GLfloat where
   windowPos2 = glWindowPos2f
   windowPos3 = glWindowPos3f

   windowPos2v = glWindowPos2fv
   windowPos3v = glWindowPos3fv

instance WindowPosComponent GLdouble where
   windowPos2 = glWindowPos2d
   windowPos3 = glWindowPos3d

   windowPos2v = glWindowPos2dv
   windowPos3v = glWindowPos3dv

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

currentRasterSecondaryColor :: GettableStateVar (Color4 GLfloat)
currentRasterSecondaryColor =
   makeGettableStateVar (getFloat4 Color4 GetCurrentRasterSecondaryColor)

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

--------------------------------------------------------------------------------

rasterPositionUnclipped :: StateVar Capability
rasterPositionUnclipped = makeCapability CapRasterPositionUnclipped
