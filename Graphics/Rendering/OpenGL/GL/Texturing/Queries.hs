--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Queries
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module offers various texture queries.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing.Queries (
   TextureQuery, textureInternalFormat, textureSize1D, textureSize2D,
   textureSize3D, textureBorder, textureRGBASizes, textureIntensitySize,
   textureLuminanceSize, textureIndexSize, textureDepthBits,
   textureCompressedImageSize, textureProxyOK
) where

import Control.Monad ( liftM2, liftM3, liftM4 )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLint, GLsizei, GLenum )
import Graphics.Rendering.OpenGL.GL.GLboolean ( unmarshalGLboolean )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek1 )
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat (
   unmarshalPixelInternalFormat )
import Graphics.Rendering.OpenGL.GL.PixelRectangles (
   Proxy(..), PixelInternalFormat(..) )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar )
import Graphics.Rendering.OpenGL.GL.Texturing.Specification (
   TextureTarget(..), CubeMapTarget, Level, Border,
   TextureSize1D(..), TextureSize2D(..), TextureSize3D(..) )
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget (
   marshalProxyTextureTarget, marshalCubeMapTarget )
import Graphics.Rendering.OpenGL.GL.VertexSpec( Color4(..) )

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

marshalTexLevelParameter :: TexLevelParameter -> GLenum
marshalTexLevelParameter x = case x of
   TextureInternalFormat -> 0x1003
   TextureWidth -> 0x1000
   TextureHeight -> 0x1001
   TextureDepth -> 0x8071
   TextureBorder -> 0x1005
   TextureRedSize -> 0x805C
   TextureGreenSize -> 0x805D
   TextureBlueSize -> 0x805E
   TextureAlphaSize -> 0x805F
   TextureIntensitySize -> 0x8061
   TextureLuminanceSize -> 0x8060
   TextureIndexSize -> 0x80ED
   DepthBits -> 0x0D56
   TextureCompressedImageSize -> 0x86A0
   TextureCompressed -> 0x86A1

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

foreign import CALLCONV unsafe "glGetTexLevelParameteriv"
   glGetTexLevelParameteriv :: GLenum -> GLint -> GLenum -> Ptr GLint -> IO ()
