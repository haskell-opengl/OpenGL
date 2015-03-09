--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Points
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 3.3 (Points) of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Points (
   -- * Point Rasterization
   pointSize, vertexProgramPointSize,

   -- * Controlling the Derived Size
   pointSizeRange, pointDistanceAttenuation,

   -- * Fading Points
   pointFadeThresholdSize,

   -- * Point Antialiasing
   pointSmooth,

   -- * Point Sprites
   pointSprite,

   -- * Implementation-Dependent Limits
   aliasedPointSizeRange, smoothPointSizeRange, smoothPointSizeGranularity
) where

import Control.Monad
import Data.StateVar
import Foreign.Marshal.Array
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.PointParameter
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

-- | 'pointSize' contains the rasterized diameter of both aliased and
-- antialiased points. The initial value is 1. Using a point size other than 1
-- has different effects, depending on whether point antialiasing is enabled
-- (see 'pointSmooth') or point sprites are enabled (see 'pointSprite'). Both
-- are initially disabled.
--
-- The specified point size is multiplied with a distance attenuation factor
-- and clamped to the specified 'pointSizeRange', and further clamped to the
-- implementation-dependent point size range to produce the derived point size
-- using
--
-- @   /derivedSize/ = /clamp/ (/size/ * /sqrt/ (1 \/ (/a/ + /b/ * /d/ + /c/ * /d/^2)))@
--
-- where /d/ is the eye-coordinate distance from the eye to the vertex, and /a/,
-- /b/, and /c/ are the distance attenuation coefficients (see
-- 'pointDistanceAttenuation').
--
-- If multisampling is disabled, the computed point size is used as the point\'s
-- width.
--
-- If multisampling is enabled, the point may be faded by modifying the point
-- alpha value (see 'Graphics.Rendering.OpenGL.GL.PerFragment.sampleCoverage')
-- instead of allowing the point width to go below a given
-- 'pointFadeThresholdSize'. In this case, the width is further modified in
-- the following manner:
--
-- @   /width/ = if /derivedSize/ >= /threshold/ then /derivedSize/ else /threshold/@
--
-- The point alpha value is modified by computing:
--
-- @   /alpha/ = if /derivedSize/ >= /threshold/ then 1 else (/derivedSize/ \/ /threshold/)^2@
--
-- If point antialiasing is disabled, the actual size is determined by rounding
-- the supplied size to the nearest integer. (If the rounding results in the
-- value 0, it is as if the point size were 1.) If the rounded size is odd,
-- then the center point (/x/, /y/) of the pixel fragment that represents
-- the point is computed as
--
-- @   (/x/, /y/) = (/floor/ /xw/ + 0.5, /floor/ /yw/ + 0.5)@
--
-- where /xw/ and /yw/ indicate window coordinates. All pixels that lie within
-- the square grid of the rounded size centered at (/x/, /y/) make up the
-- fragment. If the size is even, the center point is
--
-- @   (/x/, /y/) = (/floor/ (/xw/ + 0.5), /floor/ (/yw/ + 0.5))@
--
-- and the rasterized fragment\'s centers are the half-integer window
-- coordinates within the square of the rounded size centered at (/x/, /y/). All
-- pixel fragments produced in rasterizing a nonantialiased point are assigned
-- the same associated data, that of the vertex corresponding to the point.
--
-- If antialiasing is enabled, then point rasterization produces a fragment for
-- each pixel square that intersects the region lying within the circle having
-- diameter equal to the current point size and centered at the point\'s
-- (/xw/, /yw/). The coverage value for each fragment is the window coordinate
-- area of the intersection of the circular region with the corresponding pixel
-- square. This value is saved and used in the final rasterization step. The
-- data associated with each fragment is the data associated with the point
-- being rasterized.
--
-- Not all sizes are supported when point antialiasing is enabled. If an
-- unsupported size is requested, the nearest supported size is used.  Only size
-- 1 is guaranteed to be supported; others depend on the implementation. To
-- query the range of supported sizes for antialiased points and the size
-- difference between supported sizes within the range, query
-- 'smoothPointSizeRange' and 'smoothPointSizeGranularity', respectively. For
-- aliased points, query the supported range with 'aliasedPointSizeRange'.
--
-- The point size specified when 'pointSize' is set is always returned when it
-- is queried. Clamping and rounding for aliased and antialiased points have no
-- effect on the specified value.
--
-- A non-antialiased point size may be clamped to an implementation-dependent
-- maximum. Although this maximum cannot be queried, it must be no less than the
-- maximum value for antialiased points, rounded to the nearest integer value.
--
-- An 'Graphics.Rendering.OpenGL.GLU.Errors.InvalidValue' is generated if
-- 'pointSize' is set to a value less than or equal to zero.
--
-- An 'Graphics.Rendering.OpenGL.GLU.Errors.InvalidOperation' is generated if
-- 'pointSize' is set during
-- 'Graphics.Rendering.OpenGL.GL.BeginEnd.renderPrimitive'.

pointSize :: StateVar GLfloat
pointSize = makeStateVar (getFloat1 id GetPointSize) glPointSize

--------------------------------------------------------------------------------

vertexProgramPointSize :: StateVar Capability
vertexProgramPointSize = makeCapability CapVertexProgramPointSize

--------------------------------------------------------------------------------

-- | The range to which the derived point size is clamped, see 'pointSize'. Note
-- that the size is further clamped to the implementation-dependent limits, see
-- 'aliasedPointSizeRange' and 'smoothPointSizeRange'. The initial range is
-- (0, 1).
--
-- An 'Graphics.Rendering.OpenGL.GLU.Errors.InvalidValue' is generated if the
-- lower or upper bound of the range is set to a value less than zero. If the
-- lower bound is greater than the upper bound, the point size after clamping is
-- undefined, but no error is generated.

pointSizeRange :: StateVar (GLfloat, GLfloat)
pointSizeRange =
   makeStateVar
   (liftM2 (,) (getFloat1 id GetPointSizeMin) (getFloat1 id GetPointSizeMax))
   (\(sizeMin, sizeMax) -> do pointParameterf PointSizeMin sizeMin
                              pointParameterf PointSizeMax sizeMax)

--------------------------------------------------------------------------------

-- | The  constant, linear, and quadratic distance attenuation coefficients, see
-- 'pointSize'. The initial coefficients are (1, 0, 0).

pointDistanceAttenuation :: StateVar (GLfloat, GLfloat, GLfloat)
pointDistanceAttenuation =
   makeStateVar
      (getFloat3 (,,) GetPointDistanceAttenuation)
      (\(a, b, c) -> withArray [a, b, c] $
                        pointParameterfv PointDistanceAttenuation)

--------------------------------------------------------------------------------

-- | The threshold for alpha attenuation of points when multisampling is used,
-- see 'pointSize'. The initial threshold is 1.
--
-- An 'Graphics.Rendering.OpenGL.GLU.Errors.InvalidValue' is generated if the
-- threshold is set to a value less than zero.

pointFadeThresholdSize :: StateVar GLfloat
pointFadeThresholdSize =
   makeStateVar
      (getFloat1 id GetPointFadeThresholdSize)
      (pointParameterf PointFadeThresholdSize)

--------------------------------------------------------------------------------

-- | Controls whether point antialiasing is enabled. The initial state is
-- 'Graphics.Rendering.OpenGL.GL.Capability.Disabled'.

pointSmooth :: StateVar Capability
pointSmooth = makeCapability CapPointSmooth

--------------------------------------------------------------------------------

-- | Controls whether point sprites are enabled. The initial state is
-- 'Graphics.Rendering.OpenGL.GL.Capability.Disabled'. When point sprites are
-- enabled, the state of point antialiasing (i.e. 'pointSmooth') is ignored.

pointSprite :: StateVar Capability
pointSprite = makeCapability CapPointSprite

--------------------------------------------------------------------------------

-- | The smallest and largest supported size of aliased points.

aliasedPointSizeRange :: GettableStateVar (GLfloat, GLfloat)
aliasedPointSizeRange =
   makeGettableStateVar $ getFloat2 (,) GetAliasedPointSizeRange

-- | The smallest and largest supported size of antialiased points.

smoothPointSizeRange :: GettableStateVar (GLfloat, GLfloat)
smoothPointSizeRange =
   makeGettableStateVar $ getFloat2 (,) GetSmoothPointSizeRange

-- | The antialiased point size granularity, i.e. the size difference between
-- supported sizes.

smoothPointSizeGranularity :: GettableStateVar GLfloat
smoothPointSizeGranularity =
   makeGettableStateVar $ getFloat1 id GetSmoothPointSizeGranularity
