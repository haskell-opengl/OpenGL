{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PointParameter
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for setting point parameters.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PointParameter (
   PointParameter(..), pointParameterf, pointParameterfv
) where

import Foreign.Ptr
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (
 gl_POINT_DISTANCE_ATTENUATION, gl_POINT_SIZE_MAX, gl_POINT_SIZE_MIN )
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

data PointParameter =
     PointSizeMin
   | PointSizeMax
   | PointFadeThresholdSize
   | PointDistanceAttenuation

marshalPointParameter :: PointParameter -> GLenum
marshalPointParameter x = case x of
   PointSizeMin -> gl_POINT_SIZE_MIN
   PointSizeMax -> gl_POINT_SIZE_MAX
   PointFadeThresholdSize -> gl_POINT_FADE_THRESHOLD_SIZE
   PointDistanceAttenuation -> gl_POINT_DISTANCE_ATTENUATION

--------------------------------------------------------------------------------

pointParameterf :: PointParameter -> GLfloat -> IO ()
pointParameterf = glPointParameterf . marshalPointParameter

pointParameterfv :: PointParameter -> Ptr GLfloat -> IO ()
pointParameterfv = glPointParameterfv . marshalPointParameter
