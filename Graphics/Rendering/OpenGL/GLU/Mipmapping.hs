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
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Size(..) )
import Graphics.Rendering.OpenGL.GL.DataType ( marshalDataType )
import Graphics.Rendering.OpenGL.GL.VertexArrays ( DataType )
import Graphics.Rendering.OpenGL.GL.PixelFormat ( marshalPixelFormat )
import Graphics.Rendering.OpenGL.GL.PixelRectangles ( PixelFormat )
import Graphics.Rendering.OpenGL.GL.Texturing (
   TextureTarget, marshalTextureTarget,
   PixelInternalFormat, marshalPixelInternalFormat )

---------------------------------------------------------------------------
-- Section 3.1 (Image Scaling)

scaleImage :: Size -> PixelFormat -> DataType -> Ptr a
           -> Size -> PixelFormat -> DataType -> Ptr b -> IO ()
scaleImage (Size widthIn  heightIn)  formatIn  typeIn  addrIn
           (Size widthOut heightOut) formatOut typeOut addrOut
   | formatIn == formatOut = scaleImageAux (marshalPixelFormat formatIn) widthIn  heightIn  (marshalDataType typeIn ) addrIn
                                                                         widthOut heightOut (marshalDataType typeOut) addrOut
   | otherwise = error "scaleImage: pixel formats differ"

foreign import CALLCONV unsafe "gluScaleImage" scaleImageAux ::
   GLenum -> GLsizei -> GLsizei -> GLenum -> Ptr a
          -> GLsizei -> GLsizei -> GLenum -> Ptr b -> IO ()

---------------------------------------------------------------------------
-- Section 3.2 (Automatic Mipmapping)
-- Missing for GLU 1.3: gluBuild3DMipmaps, gluBuild{1,2,3}DMipmapLevels

build1DMipmaps :: TextureTarget -> PixelInternalFormat -> GLsizei
               -> PixelFormat -> DataType -> Ptr a -> IO ()
build1DMipmaps target internalFormat height f t a = do
   build1DMipmapsAux (marshalTextureTarget target) (fromIntegral (marshalPixelInternalFormat internalFormat)) height (marshalPixelFormat f) (marshalDataType t) a
   return ()   -- TODO: Should we use the return value?

foreign import CALLCONV unsafe "gluBuild1DMipmaps" build1DMipmapsAux ::
      GLenum -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO GLint

build2DMipmaps :: TextureTarget -> PixelInternalFormat -> GLsizei -> GLsizei
               -> PixelFormat -> DataType -> Ptr a -> IO ()
build2DMipmaps target internalFormat width height f t a = do
   build2DMipmapsAux (marshalTextureTarget target) (fromIntegral (marshalPixelInternalFormat internalFormat)) width height (marshalPixelFormat f) (marshalDataType t) a
   return ()   -- TODO: Should we use the return value?

foreign import CALLCONV unsafe "gluBuild2DMipmaps" build2DMipmapsAux ::
      GLenum -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO GLint
