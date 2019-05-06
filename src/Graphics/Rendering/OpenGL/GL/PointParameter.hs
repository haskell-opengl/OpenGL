{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PointParameter
-- Copyright   :  (c) Sven Panne 2002-2019
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
import Graphics.GL

--------------------------------------------------------------------------------

data PointParameter =
     PointSizeMin
   | PointSizeMax
   | PointFadeThresholdSize
   | PointDistanceAttenuation

marshalPointParameter :: PointParameter -> GLenum
marshalPointParameter x = case x of
   PointSizeMin -> GL_POINT_SIZE_MIN
   PointSizeMax -> GL_POINT_SIZE_MAX
   PointFadeThresholdSize -> GL_POINT_FADE_THRESHOLD_SIZE
   PointDistanceAttenuation -> GL_POINT_DISTANCE_ATTENUATION

--------------------------------------------------------------------------------

pointParameterf :: PointParameter -> GLfloat -> IO ()
pointParameterf = glPointParameterf . marshalPointParameter

pointParameterfv :: PointParameter -> Ptr GLfloat -> IO ()
pointParameterfv = glPointParameterfv . marshalPointParameter
