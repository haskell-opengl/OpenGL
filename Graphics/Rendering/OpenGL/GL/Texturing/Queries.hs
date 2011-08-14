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
import Graphics.Rendering.OpenGL.GL.PixellikeObject
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

type TextureQuery t a = t -> Level -> GettableStateVar a

textureInternalFormat :: TextureTarget t => TextureQuery t PixelInternalFormat
textureInternalFormat t level =
   makeGettableStateVar $
      getTexLevelParameteri unmarshalPixelInternalFormat NoProxy t level TextureInternalFormat

textureSize1D :: TextureTarget t => TextureQuery t  TextureSize1D
textureSize1D t level =
   makeGettableStateVar $
      getTexLevelParameteri (TextureSize1D . fromIntegral) NoProxy t level TextureWidth

textureSize2D :: TextureTarget t => TextureQuery t  TextureSize2D
textureSize2D t level =
   makeGettableStateVar $
      liftM2 TextureSize2D
             (getTexLevelParameteri fromIntegral NoProxy t level TextureWidth )
             (getTexLevelParameteri fromIntegral NoProxy t level TextureHeight)

textureSize3D :: TextureTarget t => TextureQuery t  TextureSize3D
textureSize3D t level =
   makeGettableStateVar $
      liftM3 TextureSize3D
             (getTexLevelParameteri fromIntegral NoProxy t level TextureWidth )
             (getTexLevelParameteri fromIntegral NoProxy t level TextureHeight)
             (getTexLevelParameteri fromIntegral NoProxy t level TextureDepth )

textureBorder :: TextureTarget t => TextureQuery t  Border
textureBorder t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level TextureBorder

textureRGBASizes :: TextureTarget t => TextureQuery t  (Color4 GLsizei)
textureRGBASizes t level =
   makeGettableStateVar $
      liftM4 Color4
             (getTexLevelParameteri fromIntegral NoProxy t level TextureRedSize  )
             (getTexLevelParameteri fromIntegral NoProxy t level TextureGreenSize)
             (getTexLevelParameteri fromIntegral NoProxy t level TextureBlueSize )
             (getTexLevelParameteri fromIntegral NoProxy t level TextureAlphaSize)

textureSharedSize :: TextureTarget t => TextureQuery t  GLsizei
textureSharedSize t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level TextureSharedSize

textureIntensitySize :: TextureTarget t => TextureQuery t  GLsizei
textureIntensitySize t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level TextureIntensitySize

textureLuminanceSize :: TextureTarget t => TextureQuery t  GLsizei
textureLuminanceSize t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level TextureLuminanceSize

textureIndexSize :: TextureTarget t => TextureQuery t  GLsizei
textureIndexSize t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level TextureIndexSize

textureDepthBits :: TextureTarget t => TextureQuery t  GLsizei
textureDepthBits t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level DepthBits

textureCompressedImageSize :: TextureTarget t => TextureQuery t  (Maybe GLsizei)
textureCompressedImageSize t level =
   makeGettableStateVar $ do
      isCompressed <- getTexLevelParameteri unmarshalGLboolean NoProxy t level TextureCompressed
      if isCompressed
         then getTexLevelParameteri (Just . fromIntegral) NoProxy t level TextureCompressedImageSize
         else return Nothing

textureProxyOK :: TextureTarget t => TextureQuery t  Bool
textureProxyOK t level =
   makeGettableStateVar $
      getTexLevelParameteri unmarshalGLboolean Proxy t level TextureWidth

getTexLevelParameteri :: TextureTarget t => (GLint -> a) -> Proxy -> t -> Level -> TexLevelParameter -> IO a
getTexLevelParameteri f proxy t level p =
   alloca $ \buf -> do
      glGetTexLevelParameteriv (marshalProxyTextureTarget proxy t) level (marshalTexLevelParameter p) buf
      peek1 f buf

--------------------------------------------------------------------------------

data TextureTargetFull t = TextureTargetFull t Level

instance TextureTarget t => PixellikeObjectTarget (TextureTargetFull t) where
   marshalPixellikeOT _ x = case x of
      RedSize -> gl_TEXTURE_RED_SIZE
      BlueSize -> gl_TEXTURE_BLUE_SIZE
      GreenSize -> gl_TEXTURE_GREEN_SIZE
      AlphaSize -> gl_TEXTURE_ALPHA_SIZE
      DepthSize -> gl_TEXTURE_DEPTH_SIZE
      StencilSize -> gl_TEXTURE_STENCIL_SIZE
   getterFuncPOT (TextureTargetFull t level) p =
      alloca $ \buf -> do
      glGetTexLevelParameteriv (marshalTextureTarget t) level p buf
      peek1 id buf
