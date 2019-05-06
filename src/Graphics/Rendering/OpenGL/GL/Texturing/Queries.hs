--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Queries
-- Copyright   :  (c) Sven Panne 2002-2019
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
   textureDepthBits, textureCompressedImageSize, textureProxyOK,
   DataRepresentation(..), textureRGBATypes, textureIntensityType,
   textureLuminanceType, textureDepthType
) where

import Control.Monad
import Data.StateVar
import Foreign.Marshal.Utils
import Graphics.Rendering.OpenGL.GL.DataType
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.PixelRectangles
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat
import Graphics.Rendering.OpenGL.GL.Texturing.Specification
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.GL

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
   | TextureRedType
   | TextureGreenType
   | TextureBlueType
   | TextureAlphaType
   | TextureLuminanceType
   | TextureIntensityType
   | TextureDepthType

marshalTexLevelParameter :: TexLevelParameter -> GLenum
marshalTexLevelParameter x = case x of
   TextureInternalFormat -> GL_TEXTURE_INTERNAL_FORMAT
   TextureWidth -> GL_TEXTURE_WIDTH
   TextureHeight -> GL_TEXTURE_HEIGHT
   TextureDepth -> GL_TEXTURE_DEPTH
   TextureBorder -> GL_TEXTURE_BORDER
   TextureRedSize -> GL_TEXTURE_RED_SIZE
   TextureGreenSize -> GL_TEXTURE_GREEN_SIZE
   TextureBlueSize -> GL_TEXTURE_BLUE_SIZE
   TextureAlphaSize -> GL_TEXTURE_ALPHA_SIZE
   TextureIntensitySize -> GL_TEXTURE_INTENSITY_SIZE
   TextureLuminanceSize -> GL_TEXTURE_LUMINANCE_SIZE
   TextureIndexSize -> GL_TEXTURE_INDEX_SIZE_EXT
   DepthBits -> GL_DEPTH_BITS
   TextureCompressedImageSize -> GL_TEXTURE_COMPRESSED_IMAGE_SIZE
   TextureCompressed -> GL_TEXTURE_COMPRESSED
   TextureSharedSize -> GL_TEXTURE_SHARED_SIZE
   TextureRedType -> GL_TEXTURE_RED_TYPE_ARB
   TextureGreenType -> GL_TEXTURE_GREEN_TYPE_ARB
   TextureBlueType -> GL_TEXTURE_BLUE_TYPE_ARB
   TextureAlphaType -> GL_TEXTURE_ALPHA_TYPE_ARB
   TextureLuminanceType -> GL_TEXTURE_LUMINANCE_TYPE_ARB
   TextureIntensityType -> GL_TEXTURE_INTENSITY_TYPE_ARB
   TextureDepthType -> GL_TEXTURE_DEPTH_TYPE_ARB

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
      getTexLevelParameteri unmarshalGLboolean (marshalParameterizedTextureTargetProxy t) level TextureWidth

textureRGBATypes :: QueryableTextureTarget t =>  TextureQuery t (Color4 (Maybe DataRepresentation))
textureRGBATypes t level =
   makeGettableStateVar $
      liftM4 Color4
             (getDataRepr t level TextureRedType  )
             (getDataRepr t level TextureGreenType)
             (getDataRepr t level TextureBlueType )
             (getDataRepr t level TextureAlphaType)

getDataRepr :: QueryableTextureTarget t => t -> Level -> TexLevelParameter -> IO (Maybe DataRepresentation)
getDataRepr = getTexLevelParameteriNoProxy (unmarshalDataRepresentation . fromIntegral)

textureIntensityType :: QueryableTextureTarget t => TextureQuery t (Maybe DataRepresentation)
textureIntensityType t level = makeGettableStateVar $ getDataRepr t level TextureIntensityType

textureLuminanceType :: QueryableTextureTarget t =>  TextureQuery t (Maybe DataRepresentation)
textureLuminanceType t level = makeGettableStateVar $ getDataRepr t level TextureLuminanceType

textureDepthType :: QueryableTextureTarget t =>  TextureQuery t (Maybe DataRepresentation)
textureDepthType t level = makeGettableStateVar $ getDataRepr t level TextureDepthType

getTexLevelParameteriNoProxy :: QueryableTextureTarget t => (GLint -> a) -> t -> Level -> TexLevelParameter -> IO a
getTexLevelParameteriNoProxy f = getTexLevelParameteri f . marshalQueryableTextureTarget

getTexLevelParameteri :: (GLint -> a) -> GLenum -> Level -> TexLevelParameter -> IO a
getTexLevelParameteri f t level p =
   with 0 $ \buf -> do
      glGetTexLevelParameteriv t level (marshalTexLevelParameter p) buf
      peek1 f buf
