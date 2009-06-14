--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to a part of section 3.6.4 (Rasterization of Pixel
-- Rectangles) of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization (
   PixelData(..), PixelFormat(..), drawPixels, pixelZoom
) where

import Control.Monad ( liftM2 )
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Size(..) )
import Graphics.Rendering.OpenGL.GL.PixelData ( PixelData(..), withPixelData )
import Graphics.Rendering.OpenGL.GL.PixelFormat ( PixelFormat(..) )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetZoomX,GetZoomY), getFloat1 )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar, makeStateVar )

--------------------------------------------------------------------------------

drawPixels :: Size -> PixelData a -> IO ()
drawPixels (Size w h) pd = withPixelData pd $ glDrawPixels w h

--------------------------------------------------------------------------------

pixelZoom :: StateVar (GLfloat, GLfloat)
pixelZoom =
   makeStateVar
      (liftM2 (,) (getFloat1 id GetZoomX) (getFloat1 id GetZoomY))
      (uncurry glPixelZoom)
