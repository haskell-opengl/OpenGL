--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Queries
-- Copyright   :  (c) Sven Panne 2002-2004
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
   PixelInternalFormat(..), unmarshalPixelInternalFormat )
import Graphics.Rendering.OpenGL.GL.PixelRectangles ( Proxy(..) )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar )
import Graphics.Rendering.OpenGL.GL.Texturing.Specification (
   Level, Border, TextureSize1D(..), TextureSize2D(..), TextureSize3D(..) )
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget (
   TextureTarget(..), marshalProxyTextureTarget )
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

type TextureQuery a = TextureTarget -> Level -> GettableStateVar a

-- ToDo: cube maps
textureInternalFormat :: TextureQuery PixelInternalFormat
textureInternalFormat t level =
   makeGettableStateVar $
      getTexLevelParameteri (unmarshalPixelInternalFormat . fromIntegral) NoProxy t level TextureInternalFormat

-- ToDo: cube maps
textureSize1D :: TextureQuery TextureSize1D
textureSize1D t level =
   makeGettableStateVar $
      getTexLevelParameteri (TextureSize1D . fromIntegral) NoProxy t level TextureWidth

-- ToDo: cube maps
textureSize2D :: TextureQuery TextureSize2D
textureSize2D t level =
   makeGettableStateVar $
      liftM2 TextureSize2D
             (getTexLevelParameteri fromIntegral NoProxy t level TextureWidth )
             (getTexLevelParameteri fromIntegral NoProxy t level TextureHeight)

-- ToDo: cube maps
textureSize3D :: TextureQuery TextureSize3D
textureSize3D t level =
   makeGettableStateVar $
      liftM3 TextureSize3D
             (getTexLevelParameteri fromIntegral NoProxy t level TextureWidth )
             (getTexLevelParameteri fromIntegral NoProxy t level TextureHeight)
             (getTexLevelParameteri fromIntegral NoProxy t level TextureDepth )

-- ToDo: cube maps
textureBorder :: TextureQuery Border
textureBorder t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level TextureBorder

-- ToDo: cube maps
textureRGBASizes :: TextureQuery (Color4 GLsizei)
textureRGBASizes t level =
   makeGettableStateVar $
      liftM4 Color4
             (getTexLevelParameteri fromIntegral NoProxy t level TextureRedSize  )
             (getTexLevelParameteri fromIntegral NoProxy t level TextureGreenSize)
             (getTexLevelParameteri fromIntegral NoProxy t level TextureBlueSize )
             (getTexLevelParameteri fromIntegral NoProxy t level TextureAlphaSize)

-- ToDo: cube maps
textureIntensitySize :: TextureQuery GLsizei
textureIntensitySize t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level TextureIntensitySize

-- ToDo: cube maps
textureLuminanceSize :: TextureQuery GLsizei
textureLuminanceSize t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level TextureLuminanceSize

-- ToDo: cube maps
textureIndexSize :: TextureQuery GLsizei
textureIndexSize t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level TextureIndexSize

-- ToDo: cube maps
textureDepthBits :: TextureQuery GLsizei
textureDepthBits t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level DepthBits

-- ToDo: cube maps
textureCompressedImageSize :: TextureQuery (Maybe GLsizei)
textureCompressedImageSize t level =
   makeGettableStateVar $ do
      isCompressed <- getTexLevelParameteri (unmarshalGLboolean . fromIntegral) NoProxy t level TextureCompressed
      if isCompressed
         then getTexLevelParameteri (Just . fromIntegral) NoProxy t level TextureCompressedImageSize
         else return Nothing

-- ToDo: cube maps
textureProxyOK :: TextureQuery Bool
textureProxyOK t level =
   makeGettableStateVar $
      getTexLevelParameteri (unmarshalGLboolean . fromIntegral) Proxy t level TextureWidth

getTexLevelParameteri :: (GLint -> a) -> Proxy -> TextureTarget -> Level -> TexLevelParameter -> IO a
getTexLevelParameteri f proxy t level p =
   alloca $ \buf -> do
      glGetTexLevelParameteriv (marshalProxyTextureTarget proxy t) level (marshalTexLevelParameter p) buf
      peek1 f buf

-- all targets incl. proxies and all cube maps
foreign import CALLCONV unsafe "glGetTexLevelParameteriv"
   glGetTexLevelParameteriv :: GLenum -> GLint -> GLenum -> Ptr GLint -> IO ()
