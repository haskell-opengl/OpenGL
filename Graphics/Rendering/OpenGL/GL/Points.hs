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
   pointFadeThresholdSize, pointSmooth
) where

import Control.Monad ( liftM2 )
import Foreign.Marshal.Array ( withArray )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLfloat )
import Graphics.Rendering.OpenGL.GL.Capability (
   Capability, EnableCap(CapPointSmooth), makeCapability )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetPointSize,GetAliasedPointSizeRange,GetSmoothPointSizeRange,
            GetSmoothPointSizeGranularity,GetPointSizeMin,GetPointSizeMax,
            GetPointDistanceAttenuation,GetPointFadeThresholdSize),
   getFloat1, getFloat2, getFloat3 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"

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

data PointParameterName =
     PointSizeMin
   | PointSizeMax
   | PointFadeThresholdSize
   | PointDistanceAttenuation

marshalPointParameterName :: PointParameterName -> GLenum
marshalPointParameterName x = case x of
   PointSizeMin -> 0x8126
   PointSizeMax -> 0x8127
   PointFadeThresholdSize -> 0x8128
   PointDistanceAttenuation -> 0x8129

--------------------------------------------------------------------------------

glPointParameterf :: PointParameterName -> GLfloat -> IO ()
glPointParameterf = glPointParameterfARB . marshalPointParameterName

EXTENSION_ENTRY("GL_ARB_point_parameters or OpenGL 1.4",glPointParameterfARB,GLenum -> GLfloat -> IO ())

glPointParameterfv :: PointParameterName -> Ptr GLfloat -> IO ()
glPointParameterfv = glPointParameterfvARB . marshalPointParameterName

EXTENSION_ENTRY("GL_ARB_point_parameters or OpenGL 1.4",glPointParameterfvARB,GLenum -> Ptr GLfloat -> IO ())

--------------------------------------------------------------------------------

pointSizeRange :: StateVar (GLfloat, GLfloat)
pointSizeRange =
   makeStateVar
   (liftM2 (,) (getFloat1 id GetPointSizeMin) (getFloat1 id GetPointSizeMax))
   (\(sizeMin, sizeMax) -> do glPointParameterf PointSizeMin sizeMin
                              glPointParameterf PointSizeMax sizeMax)

--------------------------------------------------------------------------------

pointDistanceAttenuation :: StateVar (GLfloat, GLfloat, GLfloat)
pointDistanceAttenuation =
   makeStateVar
      (getFloat3 (,,) GetPointDistanceAttenuation)
      (\(a, b, c) -> withArray [a, b, c] $
                        glPointParameterfv PointDistanceAttenuation)

--------------------------------------------------------------------------------

pointFadeThresholdSize :: StateVar GLfloat
pointFadeThresholdSize =
   makeStateVar
      (getFloat1 id GetPointFadeThresholdSize)
      (glPointParameterf PointFadeThresholdSize)

--------------------------------------------------------------------------------

pointSmooth :: StateVar Capability
pointSmooth = makeCapability CapPointSmooth
