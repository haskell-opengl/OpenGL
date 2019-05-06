--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.Mipmapping
-- Copyright   :  (c) Sven Panne 2002-2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to chapter 3 (Mipmapping) of the GLU specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GLU.Mipmapping (
   scaleImage, build1DMipmaps, build2DMipmaps
) where

import Graphics.GLU
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Size(..) )
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat
import Graphics.Rendering.OpenGL.GL.PixelData ( PixelData, withPixelData )
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal ( recordInvalidValue )
import Graphics.GL

--------------------------------------------------------------------------------
-- Section 3.1 (Image Scaling)

scaleImage :: Size -> PixelData a -> Size -> PixelData b -> IO ()
scaleImage (Size widthIn  heightIn)  pdIn (Size widthOut heightOut) pdOut =
   withPixelData pdIn $ \fIn dIn pIn ->
      withPixelData pdOut $ \fOut dOut pOut ->
         if fIn == fOut
            then do _ <- gluScaleImage
                      fIn widthIn heightIn dIn pIn widthOut heightOut dOut pOut
                    return ()   -- TODO: Should we use the return value?
            else recordInvalidValue

--------------------------------------------------------------------------------
-- Section 3.2 (Automatic Mipmapping)
-- Missing for GLU 1.3: gluBuild3DMipmaps, gluBuild{1,2,3}DMipmapLevels

build1DMipmaps ::
   TextureTarget1D -> PixelInternalFormat -> GLsizei -> PixelData a -> IO ()
build1DMipmaps target internalFormat height pd = do
   _ <- withPixelData pd $
      gluBuild1DMipmaps
         (marshalGettableTextureTarget target)
         (marshalPixelInternalFormat internalFormat)
         height
   return ()   -- TODO: Should we use the return value?

--------------------------------------------------------------------------------

build2DMipmaps :: TextureTarget2D -> PixelInternalFormat -> GLsizei -> GLsizei
               -> PixelData a -> IO ()
build2DMipmaps target internalFormat width height pd = do
   _ <- withPixelData pd $
      gluBuild2DMipmaps
         (marshalGettableTextureTarget target)
         (marshalPixelInternalFormat internalFormat)
         width height
   return ()   -- TODO: Should we use the return value?
