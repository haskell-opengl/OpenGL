-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PointParameter
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for setting point parameters.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PointParameter (
   PointParameter(..), pointParameterf, pointParameterfv
) where

import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLfloat )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"

--------------------------------------------------------------------------------

data PointParameter =
     PointSizeMin
   | PointSizeMax
   | PointFadeThresholdSize
   | PointDistanceAttenuation

marshalPointParameter :: PointParameter -> GLenum
marshalPointParameter x = case x of
   PointSizeMin -> 0x8126
   PointSizeMax -> 0x8127
   PointFadeThresholdSize -> 0x8128
   PointDistanceAttenuation -> 0x8129

--------------------------------------------------------------------------------

pointParameterf :: PointParameter -> GLfloat -> IO ()
pointParameterf = glPointParameterfARB . marshalPointParameter

EXTENSION_ENTRY("GL_ARB_point_parameters or OpenGL 1.4",glPointParameterfARB,GLenum -> GLfloat -> IO ())

pointParameterfv :: PointParameter -> Ptr GLfloat -> IO ()
pointParameterfv = glPointParameterfvARB . marshalPointParameter

EXTENSION_ENTRY("GL_ARB_point_parameters or OpenGL 1.4",glPointParameterfvARB,GLenum -> Ptr GLfloat -> IO ())
