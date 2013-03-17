-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- A Haskell binding for OpenGL, the industry\'s most widely used and
-- supported 2D and 3D graphics API.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL (
   -- * OpenGL Operation
   module Graphics.Rendering.OpenGL.Raw.Core31,
   module Graphics.Rendering.OpenGL.GL.BeginEnd,
   module Graphics.Rendering.OpenGL.GL.VertexSpec,
   module Graphics.Rendering.OpenGL.GL.VertexArrays,
   module Graphics.Rendering.OpenGL.GL.VertexArrayObjects,
   module Graphics.Rendering.OpenGL.GL.BufferObjects,
   module Graphics.Rendering.OpenGL.GL.Rectangles,
   module Graphics.Rendering.OpenGL.GL.CoordTrans,
   module Graphics.Rendering.OpenGL.GL.Clipping,
   module Graphics.Rendering.OpenGL.GL.RasterPos,
   module Graphics.Rendering.OpenGL.GL.Colors,
   module Graphics.Rendering.OpenGL.GL.Shaders,

   -- * Rasterization
   module Graphics.Rendering.OpenGL.GL.Antialiasing,
   module Graphics.Rendering.OpenGL.GL.FramebufferObjects,
   module Graphics.Rendering.OpenGL.GL.Points,
   module Graphics.Rendering.OpenGL.GL.LineSegments,
   module Graphics.Rendering.OpenGL.GL.Polygons,
   module Graphics.Rendering.OpenGL.GL.PixelRectangles,
   module Graphics.Rendering.OpenGL.GL.Bitmaps,
   module Graphics.Rendering.OpenGL.GL.Texturing,
   module Graphics.Rendering.OpenGL.GL.ColorSum,
   module Graphics.Rendering.OpenGL.GL.Fog,

   -- * Per-Fragment Operations and the Framebuffer
   module Graphics.Rendering.OpenGL.GL.PerFragment,
   module Graphics.Rendering.OpenGL.GL.Framebuffer,
   module Graphics.Rendering.OpenGL.GL.ReadCopyPixels,

   -- * Special Functions
   module Graphics.Rendering.OpenGL.GL.Evaluators,
   module Graphics.Rendering.OpenGL.GL.Selection,
   module Graphics.Rendering.OpenGL.GL.Feedback,
   module Graphics.Rendering.OpenGL.GL.DisplayLists,
   module Graphics.Rendering.OpenGL.GL.FlushFinish,
   module Graphics.Rendering.OpenGL.GL.Hints,
   module Graphics.Rendering.OpenGL.GL.PixellikeObject,
   module Graphics.Rendering.OpenGL.GL.TransformFeedback,
-- is exported through PerFragment for backwards compatibility reasons
--   module Graphics.Rendering.OpenGL.GL.QueryObjects,

   -- * State and State Requests
   module Graphics.Rendering.OpenGL.GL.StateVar,
   module Graphics.Rendering.OpenGL.GL.Tensor,
   module Graphics.Rendering.OpenGL.GL.StringQueries,
   module Graphics.Rendering.OpenGL.GL.SavingState
) where

import Graphics.Rendering.OpenGL.Raw.Core31 (
   GLbitfield,  GLboolean,  GLbyte,  GLchar,  GLclampd,  GLclampf,  GLdouble,
   GLenum,  GLfloat,  GLhalf,  GLint,  GLintptr,  GLshort,  GLsizei,
   GLsizeiptr,  GLubyte,  GLuint,  GLushort )
import Graphics.Rendering.OpenGL.GL.BeginEnd
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GL.VertexArrays
import Graphics.Rendering.OpenGL.GL.VertexArrayObjects
import Graphics.Rendering.OpenGL.GL.BufferObjects
import Graphics.Rendering.OpenGL.GL.Rectangles
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.Clipping
import Graphics.Rendering.OpenGL.GL.RasterPos
import Graphics.Rendering.OpenGL.GL.Colors
import Graphics.Rendering.OpenGL.GL.Shaders

import Graphics.Rendering.OpenGL.GL.Antialiasing
import Graphics.Rendering.OpenGL.GL.FramebufferObjects
import Graphics.Rendering.OpenGL.GL.Points
import Graphics.Rendering.OpenGL.GL.LineSegments
import Graphics.Rendering.OpenGL.GL.Polygons
import Graphics.Rendering.OpenGL.GL.PixelRectangles
import Graphics.Rendering.OpenGL.GL.Bitmaps
import Graphics.Rendering.OpenGL.GL.Texturing
import Graphics.Rendering.OpenGL.GL.ColorSum
import Graphics.Rendering.OpenGL.GL.Fog

import Graphics.Rendering.OpenGL.GL.PerFragment
import Graphics.Rendering.OpenGL.GL.Framebuffer
import Graphics.Rendering.OpenGL.GL.ReadCopyPixels

import Graphics.Rendering.OpenGL.GL.Evaluators
import Graphics.Rendering.OpenGL.GL.Selection
import Graphics.Rendering.OpenGL.GL.Feedback
import Graphics.Rendering.OpenGL.GL.DisplayLists
import Graphics.Rendering.OpenGL.GL.FlushFinish
import Graphics.Rendering.OpenGL.GL.Hints
import Graphics.Rendering.OpenGL.GL.PixellikeObject
import Graphics.Rendering.OpenGL.GL.TransformFeedback
-- is exported through PerFragment for backwards compatibility reasons
--import Graphics.Rendering.OpenGL.GL.QueryObjects

import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.Tensor
import Graphics.Rendering.OpenGL.GL.StringQueries
import Graphics.Rendering.OpenGL.GL.SavingState
