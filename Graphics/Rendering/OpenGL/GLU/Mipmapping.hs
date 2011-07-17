--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.Mipmapping
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to chapter 3 (Mipmapping) of the GLU specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GLU.Mipmapping (
   scaleImage, build1DMipmaps, build2DMipmaps
) where

import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Size(..) )
import Graphics.Rendering.OpenGL.GL.PixelData ( PixelData, withPixelData )
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat (
   PixelInternalFormat, marshalPixelInternalFormat )
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget (
   TextureTarget, marshalTextureTarget )
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal ( recordInvalidValue )

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
   TextureTarget -> PixelInternalFormat -> GLsizei -> PixelData a -> IO ()
build1DMipmaps target internalFormat height pd = do
   _ <- withPixelData pd $
      gluBuild1DMipmaps
         (marshalTextureTarget target)
         (marshalPixelInternalFormat internalFormat)
         height
   return ()   -- TODO: Should we use the return value?

--------------------------------------------------------------------------------

build2DMipmaps :: TextureTarget -> PixelInternalFormat -> GLsizei -> GLsizei
               -> PixelData a -> IO ()
build2DMipmaps target internalFormat width height pd = do
   _ <- withPixelData pd $
      gluBuild2DMipmaps
         (marshalTextureTarget target)
         (marshalPixelInternalFormat internalFormat)
         width height
   return ()   -- TODO: Should we use the return value?
