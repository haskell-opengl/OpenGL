--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.LineSegments
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 3.4 (Line Segments) of the OpenGL 2.1
-- specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.LineSegments (
   -- * Line Rasterization
   lineWidth,

   -- * Line Stipple
   lineStipple,

   -- * Line Antialiasing
  lineSmooth,

   -- * Implementation-Dependent Limits
   aliasedLineWidthRange, smoothLineWidthRange, smoothLineWidthGranularity
) where

import Control.Monad
import Data.StateVar
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

-- | 'lineWidth' contains the rasterized width of both aliased and antialiased
-- lines. The initial value is 1. Using a line width other than 1 has different
-- effects, depending on whether line antialiasing is enabled (see
-- 'lineSmooth'). Line antialiasing is initially disabled.
--
-- If line antialiasing is disabled, the actual width is determined by rounding
-- the supplied width to the nearest integer. (If the rounding results in the
-- value 0, it is as if the line width were 1.) If /delta x/ >= /delta y/, /i/
-- pixels are filled in each column that is rasterized, where /i/ is the
-- rounded value of 'lineWidth'. Otherwise, /i/ pixels are filled in each row
-- that is rasterized.
--
-- If antialiasing is enabled, line rasterization produces a fragment for each
-- pixel square that intersects the region lying within the rectangle having
-- width equal to the current line width, length equal to the actual length of
-- the line, and centered on the mathematical line segment. The coverage value
-- for each fragment is the window coordinate area of the intersection of the
-- rectangular region with the corresponding pixel square. This value is saved
-- and used in the final rasterization step.
--
-- Not all widths can be supported when line antialiasing is enabled. If an
-- unsupported width is requested, the nearest supported width is used. Only
-- width 1 is guaranteed to be supported; others depend on the implementation.
--  Likewise, there is a range for aliased line widths as well. To query the
-- range of supported widths of antialiased lines and the size difference
-- between supported widths within the range, query 'smoothLineWidthRange' and
-- 'smoothLineWidthGranularity', respectively. For aliased lines, query the
-- supported range with 'aliasedLineWidthRange'.
--
-- The line width specified when 'lineWidth' is set is always returned when it
-- is queried. Clamping and rounding for aliased and antialiased lines have no
-- effect on the specified value.
--
-- A non-antialiased line width may be clamped to an implementation-dependent
-- maximum.  Query 'aliasedLineWidthRange' to determine the maximum width.
--
-- An 'Graphics.Rendering.OpenGL.GLU.Errors.InvalidValue' is generated if
-- 'lineWidth' is set to a value less than or equal to zero.
--
-- An 'Graphics.Rendering.OpenGL.GLU.Errors.InvalidOperation' is generated if
-- 'lineWidth' is set during
-- 'Graphics.Rendering.OpenGL.GL.BeginEnd.renderPrimitive'.

lineWidth :: StateVar GLfloat
lineWidth = makeStateVar (getFloat1 id GetLineWidth) glLineWidth

--------------------------------------------------------------------------------

-- | Line stippling masks out certain fragments produced by rasterization; those
-- fragments will not be drawn. The masking is achieved by using three
-- parameters: the repeat count (1st element of the 'lineStipple' pair, clamped
-- to the range [ 1 .. 256 ]), the 16-bit line stipple pattern (2nd element),
-- and an integer stipple counter /s/.
--
-- The counter /s/ is reset to 0 at before the first action during
-- 'Graphics.Rendering.OpenGL.GL.BeginEnd.renderPrimitive' is called and before
-- each line segment during
-- 'Graphics.Rendering.OpenGL.GL.BeginEnd.renderPrimitive' is generated. It is
-- incremented after each fragment of a unit width aliased line segment is
-- generated or after each /i/ fragments of an /i/ width line segment are
-- generated. The /i/ fragments associated with count /s/ are masked out if
-- @'Data.Bits.testBit' /pattern/ (( /s/ \/ /factor/ ) /mod/ 16)@ is 'False',
-- otherwise these fragments are sent to the frame buffer. Bit zero of the
-- pattern is the least significant bit, i.e. it is used first.
--
-- Antialiased lines are treated as a sequence of rectangles of height 1 for
-- purposes of stippling. Whether rectangle /s/ is rasterized or not depends on
-- the fragment rule described for aliased lines, counting rectangles rather
-- than groups of fragments.
--
-- The initial value of 'lineStipple' is 'Nothing', i.e. line stippling is
-- disabled.
--
-- An 'Graphics.Rendering.OpenGL.GLU.Errors.InvalidOperation' is generated if
-- 'lineStipple' is set during
-- 'Graphics.Rendering.OpenGL.GL.BeginEnd.renderPrimitive'.

lineStipple :: StateVar (Maybe (GLint, GLushort))
lineStipple =
   makeStateVarMaybe
      (return CapLineStipple)
      (liftM2 (,) (getInteger1 id GetLineStippleRepeat)
                  (getInteger1 fromIntegral GetLineStipplePattern))
      (uncurry glLineStipple)

--------------------------------------------------------------------------------

-- | Controls whether line antialiasing is enabled. The initial state is
-- 'Graphics.Rendering.OpenGL.GL.Capability.Disabled'.

lineSmooth :: StateVar Capability
lineSmooth = makeCapability CapLineSmooth

--------------------------------------------------------------------------------

-- | The smallest and largest supported width of aliased lines.

aliasedLineWidthRange :: GettableStateVar (GLfloat, GLfloat)
aliasedLineWidthRange =
   makeGettableStateVar $ getFloat2 (,) GetAliasedLineWidthRange

-- | The smallest and largest supported width of antialiased lines.

smoothLineWidthRange :: GettableStateVar (GLfloat, GLfloat)
smoothLineWidthRange =
   makeGettableStateVar $ getFloat2 (,) GetSmoothLineWidthRange

-- | The antialiased line width granularity, i.e. the size difference between
-- supported widths.

smoothLineWidthGranularity :: GettableStateVar GLfloat
smoothLineWidthGranularity =
   makeGettableStateVar $ getFloat1 id GetSmoothLineWidthGranularity
