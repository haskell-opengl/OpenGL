--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Points
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 3.3 (Points) of the OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Points (
   pointSize, aliasedPointSizeRange, smoothPointSizeRange,
   smoothPointSizeGranularity, pointSizeRange, pointDistanceAttenuation,
   pointFadeThresholdSize, pointSmooth, pointSprite
) where

import Control.Monad ( liftM2 )
import Foreign.Marshal.Array ( withArray )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapPointSmooth,CapPointSprite), makeCapability )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLfloat, Capability )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetPointSize,GetAliasedPointSizeRange,GetSmoothPointSizeRange,
            GetSmoothPointSizeGranularity,GetPointSizeMin,GetPointSizeMax,
            GetPointDistanceAttenuation,GetPointFadeThresholdSize),
   getFloat1, getFloat2, getFloat3 )
import Graphics.Rendering.OpenGL.GL.PointParameter (
   PointParameter(..), pointParameterf, pointParameterfv )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )

--------------------------------------------------------------------------------

pointSize :: StateVar GLfloat
pointSize = makeStateVar (getFloat1 id GetPointSize) glPointSize

foreign import CALLCONV unsafe "glPointSize" glPointSize :: GLfloat -> IO ()

--------------------------------------------------------------------------------

aliasedPointSizeRange :: GettableStateVar (GLfloat, GLfloat)
aliasedPointSizeRange =
   makeGettableStateVar $ getFloat2 (,) GetAliasedPointSizeRange

smoothPointSizeRange :: GettableStateVar (GLfloat, GLfloat)
smoothPointSizeRange =
   makeGettableStateVar $ getFloat2 (,) GetSmoothPointSizeRange

smoothPointSizeGranularity :: GettableStateVar (GLfloat, GLfloat)
smoothPointSizeGranularity =
   makeGettableStateVar $ getFloat2 (,) GetSmoothPointSizeGranularity

--------------------------------------------------------------------------------

pointSizeRange :: StateVar (GLfloat, GLfloat)
pointSizeRange =
   makeStateVar
   (liftM2 (,) (getFloat1 id GetPointSizeMin) (getFloat1 id GetPointSizeMax))
   (\(sizeMin, sizeMax) -> do pointParameterf PointSizeMin sizeMin
                              pointParameterf PointSizeMax sizeMax)

--------------------------------------------------------------------------------

pointDistanceAttenuation :: StateVar (GLfloat, GLfloat, GLfloat)
pointDistanceAttenuation =
   makeStateVar
      (getFloat3 (,,) GetPointDistanceAttenuation)
      (\(a, b, c) -> withArray [a, b, c] $
                        pointParameterfv PointDistanceAttenuation)

--------------------------------------------------------------------------------

pointFadeThresholdSize :: StateVar GLfloat
pointFadeThresholdSize =
   makeStateVar
      (getFloat1 id GetPointFadeThresholdSize)
      (pointParameterf PointFadeThresholdSize)

--------------------------------------------------------------------------------

pointSmooth :: StateVar Capability
pointSmooth = makeCapability CapPointSmooth

--------------------------------------------------------------------------------

pointSprite :: StateVar Capability
pointSprite = makeCapability CapPointSprite
