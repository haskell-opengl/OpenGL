--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Queries
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
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
import Data.StateVar
import Foreign.Marshal.Alloc
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.PixelRectangles
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat
import Graphics.Rendering.OpenGL.GL.Texturing.Specification
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (
   gl_TEXTURE_INTENSITY_SIZE, gl_TEXTURE_LUMINANCE_SIZE, gl_DEPTH_BITS )
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.EXT.PalettedTexture (
   gl_TEXTURE_INDEX_SIZE )

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

type TextureQuery a = Either TextureTarget CubeMapTarget -> Level -> GettableStateVar a

textureInternalFormat :: TextureQuery PixelInternalFormat
textureInternalFormat t level =
   makeGettableStateVar $
      getTexLevelParameteri unmarshalPixelInternalFormat NoProxy t level TextureInternalFormat

textureSize1D :: TextureQuery TextureSize1D
textureSize1D t level =
   makeGettableStateVar $
      getTexLevelParameteri (TextureSize1D . fromIntegral) NoProxy t level TextureWidth

textureSize2D :: TextureQuery TextureSize2D
textureSize2D t level =
   makeGettableStateVar $
      liftM2 TextureSize2D
             (getTexLevelParameteri fromIntegral NoProxy t level TextureWidth )
             (getTexLevelParameteri fromIntegral NoProxy t level TextureHeight)

textureSize3D :: TextureQuery TextureSize3D
textureSize3D t level =
   makeGettableStateVar $
      liftM3 TextureSize3D
             (getTexLevelParameteri fromIntegral NoProxy t level TextureWidth )
             (getTexLevelParameteri fromIntegral NoProxy t level TextureHeight)
             (getTexLevelParameteri fromIntegral NoProxy t level TextureDepth )

textureBorder :: TextureQuery Border
textureBorder t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level TextureBorder

textureRGBASizes :: TextureQuery (Color4 GLsizei)
textureRGBASizes t level =
   makeGettableStateVar $
      liftM4 Color4
             (getTexLevelParameteri fromIntegral NoProxy t level TextureRedSize  )
             (getTexLevelParameteri fromIntegral NoProxy t level TextureGreenSize)
             (getTexLevelParameteri fromIntegral NoProxy t level TextureBlueSize )
             (getTexLevelParameteri fromIntegral NoProxy t level TextureAlphaSize)

textureSharedSize :: TextureQuery GLsizei
textureSharedSize t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level TextureSharedSize

textureIntensitySize :: TextureQuery GLsizei
textureIntensitySize t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level TextureIntensitySize

textureLuminanceSize :: TextureQuery GLsizei
textureLuminanceSize t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level TextureLuminanceSize

textureIndexSize :: TextureQuery GLsizei
textureIndexSize t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level TextureIndexSize

textureDepthBits :: TextureQuery GLsizei
textureDepthBits t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level DepthBits

textureCompressedImageSize :: TextureQuery (Maybe GLsizei)
textureCompressedImageSize t level =
   makeGettableStateVar $ do
      isCompressed <- getTexLevelParameteri unmarshalGLboolean NoProxy t level TextureCompressed
      if isCompressed
         then getTexLevelParameteri (Just . fromIntegral) NoProxy t level TextureCompressedImageSize
         else return Nothing

textureProxyOK :: TextureQuery Bool
textureProxyOK t level =
   makeGettableStateVar $
      getTexLevelParameteri unmarshalGLboolean Proxy t level TextureWidth

getTexLevelParameteri :: (GLint -> a) -> Proxy -> Either TextureTarget CubeMapTarget -> Level -> TexLevelParameter -> IO a
getTexLevelParameteri f proxy t level p =
   alloca $ \buf -> do
      glGetTexLevelParameteriv (either (marshalProxyTextureTarget proxy) (\c -> if proxy == Proxy then marshalProxyTextureTarget Proxy TextureCubeMap else marshalCubeMapTarget c) t) level (marshalTexLevelParameter p) buf
      peek1 f buf
