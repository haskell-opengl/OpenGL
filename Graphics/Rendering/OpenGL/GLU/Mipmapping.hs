--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.Mipmapping
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to chapter 3 (Mipmapping) of the GLU specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GLU.Mipmapping (
   scaleImage, build1DMipmaps, build2DMipmaps
) where

import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLsizei, GLint, GLenum )
import Graphics.Rendering.OpenGL.GL.PixelRect (
   PixelDescriptor(..), marshalPixelFormat, marshalPixelType )
import Graphics.Rendering.OpenGL.GL.Texturing (
   TextureTarget, marshalTextureTarget,
   PixelInternalFormat, marshalPixelInternalFormat )

---------------------------------------------------------------------------
-- Section 3.1 (Image Scaling)

scaleImage :: GLsizei -> GLsizei -> PixelDescriptor
           -> GLsizei -> GLsizei -> PixelDescriptor -> IO ()
scaleImage widthIn  heightIn  (PixelDescriptor formatIn  typeIn  addrIn )
           widthOut heightOut (PixelDescriptor formatOut typeOut addrOut)
   | formatIn == formatOut = scaleImageAux (marshalPixelFormat formatIn) widthIn  heightIn  (marshalPixelType typeIn ) addrIn
                                                                         widthOut heightOut (marshalPixelType typeOut) addrOut
   | otherwise = error "scaleImage: pixel formats differ"

foreign import CALLCONV unsafe "gluScaleImage" scaleImageAux ::
   GLenum -> GLsizei -> GLsizei -> GLenum -> Ptr ()
          -> GLsizei -> GLsizei -> GLenum -> Ptr () -> IO ()

---------------------------------------------------------------------------
-- Section 3.2 (Automatic Mipmapping)
-- Missing for GLU 1.3: gluBuild3DMipmaps, gluBuild{1,2,3}DMipmapLevels

build1DMipmaps :: TextureTarget -> PixelInternalFormat -> GLsizei
               -> PixelDescriptor -> IO ()
build1DMipmaps target internalFormat height (PixelDescriptor f t a) = do
   build1DMipmapsAux (marshalTextureTarget target) (fromIntegral (marshalPixelInternalFormat internalFormat)) height (marshalPixelFormat f) (marshalPixelType t) a
   return ()   -- TODO: Should we use the return value?

foreign import CALLCONV unsafe "gluBuild1DMipmaps" build1DMipmapsAux ::
      GLenum -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO GLint

build2DMipmaps :: TextureTarget -> PixelInternalFormat -> GLsizei -> GLsizei
               -> PixelDescriptor -> IO ()
build2DMipmaps target internalFormat width height (PixelDescriptor f t a) = do
   build2DMipmapsAux (marshalTextureTarget target) (fromIntegral (marshalPixelInternalFormat internalFormat)) width height (marshalPixelFormat f) (marshalPixelType t) a
   return ()   -- TODO: Should we use the return value?

foreign import CALLCONV unsafe "gluBuild2DMipmaps" build2DMipmapsAux ::
      GLenum -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO GLint
