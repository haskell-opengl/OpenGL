-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PointParameter
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for setting point parameters.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PointParameter (
   PointParameter(..), pointParameterf, pointParameterfv, pointParameteri
) where

import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLfloat, GLint )
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
   | PointSpriteRMode

marshalPointParameter :: PointParameter -> GLenum
marshalPointParameter x = case x of
   PointSizeMin -> 0x8126
   PointSizeMax -> 0x8127
   PointFadeThresholdSize -> 0x8128
   PointDistanceAttenuation -> 0x8129
   PointSpriteRMode -> 0x8863

--------------------------------------------------------------------------------

pointParameterf :: PointParameter -> GLfloat -> IO ()
pointParameterf = glPointParameterfARB . marshalPointParameter

EXTENSION_ENTRY("GL_ARB_point_parameters or OpenGL 1.4",glPointParameterfARB,GLenum -> GLfloat -> IO ())

pointParameterfv :: PointParameter -> Ptr GLfloat -> IO ()
pointParameterfv = glPointParameterfvARB . marshalPointParameter

EXTENSION_ENTRY("GL_ARB_point_parameters or OpenGL 1.4",glPointParameterfvARB,GLenum -> Ptr GLfloat -> IO ())

pointParameteri :: PointParameter -> GLint -> IO ()
pointParameteri = glPointParameteriNV . marshalPointParameter

EXTENSION_ENTRY("GL_NV_point_sprite",glPointParameteriNV,GLenum -> GLint -> IO ())
