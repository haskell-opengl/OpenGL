--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Queries
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module offers various texture queries.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing.Queries (
   TextureQuery, textureInternalFormat, textureSize1D, textureSize2D,
   textureSize3D, textureBorder, textureRGBASizes, textureSharedSize,
   textureIntensitySize, textureLuminanceSize, textureIndexSize,
   textureDepthBits, textureCompressedImageSize, textureProxyOK
) where

import Control.Monad
import Foreign.Marshal.Alloc
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.PixelRectangles
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat
import Graphics.Rendering.OpenGL.GL.Texturing.Specification
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

data TexLevelParameter =
     TextureInternalFormat
   | TextureWidth
   | TextureHeight
   | TextureDepth
   | TextureBorder
   | TextureRedSize
   | TextureGreenSize
   | TextureBlueSize
   | TextureAlphaSize
   | TextureIntensitySize
   | TextureLuminanceSize
   | TextureIndexSize
   | DepthBits
   | TextureCompressedImageSize
   | TextureCompressed
   | TextureSharedSize

marshalTexLevelParameter :: TexLevelParameter -> GLenum
marshalTexLevelParameter x = case x of
   TextureInternalFormat -> gl_TEXTURE_INTERNAL_FORMAT
   TextureWidth -> gl_TEXTURE_WIDTH
   TextureHeight -> gl_TEXTURE_HEIGHT
   TextureDepth -> gl_TEXTURE_DEPTH
   TextureBorder -> gl_TEXTURE_BORDER
   TextureRedSize -> gl_TEXTURE_RED_SIZE
   TextureGreenSize -> gl_TEXTURE_GREEN_SIZE
   TextureBlueSize -> gl_TEXTURE_BLUE_SIZE
   TextureAlphaSize -> gl_TEXTURE_ALPHA_SIZE
   TextureIntensitySize -> gl_TEXTURE_INTENSITY_SIZE
   TextureLuminanceSize -> gl_TEXTURE_LUMINANCE_SIZE
   TextureIndexSize -> gl_TEXTURE_INDEX_SIZE
   DepthBits -> gl_DEPTH_BITS
   TextureCompressedImageSize -> gl_TEXTURE_COMPRESSED_IMAGE_SIZE
   TextureCompressed -> gl_TEXTURE_COMPRESSED
   TextureSharedSize -> gl_TEXTURE_SHARED_SIZE

--------------------------------------------------------------------------------

type TextureQuery t a = t -> Level -> GettableStateVar a

textureInternalFormat :: QueryableTextureTarget t => TextureQuery t PixelInternalFormat
textureInternalFormat t level =
   makeGettableStateVar $
      getTexLevelParameteriNoProxy unmarshalPixelInternalFormat t level TextureInternalFormat

textureSize1D :: TextureQuery TextureTarget1D TextureSize1D
textureSize1D t level =
   makeGettableStateVar $
      liftM TextureSize1D
            (getTexLevelParameteriNoProxy fromIntegral t level TextureWidth)

textureSize2D :: TextureQuery TextureTarget2D TextureSize2D
textureSize2D t level =
   makeGettableStateVar $
      liftM2 TextureSize2D
             (getTexLevelParameteriNoProxy fromIntegral t level TextureWidth )
             (getTexLevelParameteriNoProxy fromIntegral t level TextureHeight)

textureSize3D :: TextureQuery TextureTarget3D TextureSize3D
textureSize3D t level =
   makeGettableStateVar $
      liftM3 TextureSize3D
             (getTexLevelParameteriNoProxy fromIntegral t level TextureWidth )
             (getTexLevelParameteriNoProxy fromIntegral t level TextureHeight)
             (getTexLevelParameteriNoProxy fromIntegral t level TextureDepth )

textureBorder :: QueryableTextureTarget t => TextureQuery t Border
textureBorder t level =
   makeGettableStateVar $
      getTexLevelParameteriNoProxy fromIntegral t level TextureBorder

textureRGBASizes :: QueryableTextureTarget t =>  TextureQuery t (Color4 GLsizei)
textureRGBASizes t level =
   makeGettableStateVar $
      liftM4 Color4
             (getTexLevelParameteriNoProxy fromIntegral t level TextureRedSize  )
             (getTexLevelParameteriNoProxy fromIntegral t level TextureGreenSize)
             (getTexLevelParameteriNoProxy fromIntegral t level TextureBlueSize )
             (getTexLevelParameteriNoProxy fromIntegral t level TextureAlphaSize)

textureSharedSize :: QueryableTextureTarget t =>  TextureQuery t GLsizei
textureSharedSize t level =
   makeGettableStateVar $
      getTexLevelParameteriNoProxy fromIntegral t level TextureSharedSize

textureIntensitySize :: QueryableTextureTarget t => TextureQuery t GLsizei
textureIntensitySize t level =
   makeGettableStateVar $
      getTexLevelParameteriNoProxy fromIntegral t level TextureIntensitySize

textureLuminanceSize :: QueryableTextureTarget t =>  TextureQuery t GLsizei
textureLuminanceSize t level =
   makeGettableStateVar $
      getTexLevelParameteriNoProxy fromIntegral t level TextureLuminanceSize

textureIndexSize :: QueryableTextureTarget t => TextureQuery t GLsizei
textureIndexSize t level =
   makeGettableStateVar $
      getTexLevelParameteriNoProxy fromIntegral t level TextureIndexSize

textureDepthBits :: QueryableTextureTarget t => TextureQuery t GLsizei
textureDepthBits t level =
   makeGettableStateVar $
      getTexLevelParameteriNoProxy fromIntegral t level DepthBits

textureCompressedImageSize :: QueryableTextureTarget t => TextureQuery t (Maybe GLsizei)
textureCompressedImageSize t level =
   makeGettableStateVar $ do
      isCompressed <- getTexLevelParameteriNoProxy unmarshalGLboolean t level TextureCompressed
      if isCompressed
         then getTexLevelParameteriNoProxy (Just . fromIntegral) t level TextureCompressedImageSize
         else return Nothing

textureProxyOK :: ParameterizedTextureTarget t => TextureQuery t Bool
textureProxyOK t level =
   makeGettableStateVar $
      getTexLevelParameteriProxy unmarshalGLboolean t level TextureWidth

getTexLevelParameteriProxy :: ParameterizedTextureTarget t => (GLint -> a) -> t -> Level -> TexLevelParameter -> IO a
getTexLevelParameteriProxy f t level p = getTexLevelParameteri f (marshalParameterizedTextureTargetProxy t) level p

getTexLevelParameteriNoProxy :: QueryableTextureTarget t => (GLint -> a) -> t -> Level -> TexLevelParameter -> IO a
getTexLevelParameteriNoProxy f t level p = getTexLevelParameteri f (marshalQueryableTextureTarget t) level p

getTexLevelParameteri :: (GLint -> a) -> GLenum -> Level -> TexLevelParameter -> IO a
getTexLevelParameteri f t level p =
   alloca $ \buf -> do
      glGetTexLevelParameteriv t level (marshalTexLevelParameter p) buf
      peek1 f buf
