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
   pointSize, pointSizeRange, pointDistanceAttenuation, pointFadeThresholdSize,
   pointSmooth
) where

import Foreign.Marshal.Array ( withArray )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLfloat )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapPointSmooth), makeCapability )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetPointSize,GetPointSizeMin,GetPointSizeMax,
            GetPointFadeThresholdSize),
   getFloat1 )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar, makeStateVar )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"

--------------------------------------------------------------------------------

pointSize :: StateVar GLfloat
pointSize = makeStateVar (getFloat1 id GetPointSize) glPointSize

foreign import CALLCONV unsafe "glPointSize" glPointSize :: GLfloat -> IO ()

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
pointSizeRange = makeStateVar getPointSizeRange setPointSizeRange

getPointSizeRange :: IO (GLfloat, GLfloat)
getPointSizeRange = do
   sizeMin <- getFloat1 id GetPointSizeMin
   sizeMax <- getFloat1 id GetPointSizeMax
   return (sizeMin, sizeMax)

setPointSizeRange :: (GLfloat, GLfloat) -> IO ()
setPointSizeRange (sizeMin, sizeMax) = do
  glPointParameterf PointSizeMin sizeMin
  glPointParameterf PointSizeMax sizeMax

--------------------------------------------------------------------------------

pointDistanceAttenuation :: StateVar (GLfloat, GLfloat, GLfloat)
pointDistanceAttenuation =
   makeStateVar getPointDistanceAttenuation setPointDistanceAttenuation

getPointDistanceAttenuation :: IO (GLfloat, GLfloat, GLfloat)
getPointDistanceAttenuation = error ""

setPointDistanceAttenuation :: (GLfloat, GLfloat, GLfloat) -> IO ()
setPointDistanceAttenuation (a, b, c) = do
   withArray [a, b, c] $ glPointParameterfv PointDistanceAttenuation

--------------------------------------------------------------------------------

pointFadeThresholdSize :: StateVar GLfloat
pointFadeThresholdSize =
   makeStateVar
      (getFloat1 id GetPointFadeThresholdSize)
      (glPointParameterf PointFadeThresholdSize)

--------------------------------------------------------------------------------

pointSmooth :: StateVar Bool
pointSmooth = makeCapability CapPointSmooth
