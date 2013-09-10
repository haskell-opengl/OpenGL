{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for marshaling texture targets.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget (
   TextureTarget(..), marshalProxyTextureTarget,
   TextureTarget1D(..), TextureTarget2D(..), TextureTarget3D(..),
   CubeMapTarget(..), unmarshalCubeMapTarget
) where

import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.PixelRectangles
import Graphics.Rendering.OpenGL.GL.QueryUtils.PName
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

class TextureTarget t where
   -- | The marshaling function for texture targets when updating, querying etc.
   marshalTextureTarget :: t -> GLenum

   -- | The marshaling function for texture targets to there proxy targets.
   marshalTextureTargetProxy :: t -> GLenum

   -- | The GetPName to query it's maximum size.
   textureTargetToMaxQuery :: t -> PName1I

   textureTargetToEnableCap :: t -> EnableCap

   textureTargetToBinding :: t -> PName1I


-- | Marshal a Proxy and Texture Target to their normal enum.
marshalProxyTextureTarget :: TextureTarget t => Proxy -> t -> GLenum
marshalProxyTextureTarget NoProxy = marshalTextureTarget
marshalProxyTextureTarget Proxy = marshalTextureTargetProxy

--------------------------------------------------------------------------------

data TextureTarget1D = Texture1D
   deriving ( Eq, Ord, Show )

instance TextureTarget TextureTarget1D where
   marshalTextureTarget t = case t of
      Texture1D -> gl_TEXTURE_1D
   marshalTextureTargetProxy t = case t of
      Texture1D -> gl_PROXY_TEXTURE_1D
   textureTargetToMaxQuery t = case t of
      Texture1D -> GetMaxTextureSize
   textureTargetToEnableCap t = case t of
      Texture1D -> CapTexture1D
   textureTargetToBinding t = case t of
      Texture1D -> GetTextureBinding1D

--------------------------------------------------------------------------------

{- TODO: TextureTarget2D in its current state is broken. Depending on the GL
   function, different things are accepted regarding cube maps:

      * functions expecting TextureCubeMap:
           CopyImageSubData
           TexParameter{if}(etc.)
           GetTexParameter{if}(etc.)
           GenerateMipmap
           TextureView
           GetInternalFormat

      * functions expecting TextureCubeMap, but only as a proxy:
           TexImage2D
           GetTexLevelParameter
           TexStorage2D

      * functions expecting separate cube map faces:
           TexImage2D
           CopyTexImage2D
           TexSubImage2D
           CopyTexSubImage2d
           GetTexLevelParameter{if}v
           FramebufferTexture2D
           GetFramebufferAttachmentParameteriv

      * functions expecting both TextureCubeMap *and* separate cube map faces:
           GetTexImage
-}

data TextureTarget2D =
     Texture2D
   | Texture1DArray
   | TextureRectangle
   | TextureCubeMap CubeMapTarget
   deriving ( Eq, Ord, Show )

-- TODO: The _ in "TextureCubeMap _" looks strange. Is this the right API?
instance TextureTarget TextureTarget2D where
   marshalTextureTarget t = case t of
      Texture2D -> gl_TEXTURE_2D
      Texture1DArray -> gl_TEXTURE_1D_ARRAY
      TextureRectangle -> gl_TEXTURE_RECTANGLE
      TextureCubeMap c -> marshalCubeMapTarget c
   marshalTextureTargetProxy t = case t of
      Texture2D -> gl_PROXY_TEXTURE_2D
      Texture1DArray -> gl_PROXY_TEXTURE_1D_ARRAY
      TextureRectangle -> gl_PROXY_TEXTURE_RECTANGLE
      TextureCubeMap _ -> gl_PROXY_TEXTURE_CUBE_MAP
   textureTargetToMaxQuery t = case t of
      Texture2D -> GetMaxTextureSize
      Texture1DArray -> GetMaxArrayTextureLayers
      TextureRectangle -> GetMaxRectangleTextureSize
      TextureCubeMap _ -> GetMaxCubeMapTextureSize
   textureTargetToEnableCap t = case t of
      Texture2D -> CapTexture2D
      Texture1DArray -> noFixed t
      TextureRectangle -> CapTextureRectangle
      TextureCubeMap _ -> CapTextureCubeMap
   textureTargetToBinding t = case t of
      Texture2D -> GetTextureBinding2D
      Texture1DArray -> GetTextureBinding1DArray
      TextureRectangle -> GetTextureBindingRectangle
      TextureCubeMap _ -> GetTextureBindingCubeMap

noFixed :: Show a => a -> b
noFixed x = 
   error (show x ++ " can't be used the fixed-function fragment processing")

--------------------------------------------------------------------------------

data TextureTarget3D =
     Texture3D
   | Texture2DArray
   | TextureCubeMapArray CubeMapTarget
   deriving ( Eq, Ord, Show )

-- TODO: The _ in "TextureCubeMap _" looks strange. Is this the right API?
instance TextureTarget TextureTarget3D where
   marshalTextureTarget t = case t of
      Texture3D -> gl_TEXTURE_3D
      Texture2DArray -> gl_TEXTURE_2D_ARRAY
      TextureCubeMapArray c -> marshalCubeMapTarget c
   marshalTextureTargetProxy t = case t of
      Texture3D -> gl_PROXY_TEXTURE_3D
      Texture2DArray -> gl_PROXY_TEXTURE_2D_ARRAY
      TextureCubeMapArray _ -> gl_PROXY_TEXTURE_CUBE_MAP_ARRAY
   textureTargetToMaxQuery t = case t of
      Texture3D -> GetMax3DTextureSize
      Texture2DArray -> GetMaxArrayTextureLayers
      TextureCubeMapArray _ ->  GetMaxArrayTextureLayers
   textureTargetToEnableCap t = case t of
      Texture3D -> CapTexture3D
      Texture2DArray -> noFixed t
      TextureCubeMapArray _ -> CapTextureCubeMap
   textureTargetToBinding t = case t of
      Texture3D -> GetTextureBinding3D
      Texture2DArray -> GetTextureBinding2DArray
      TextureCubeMapArray _ -> GetTextureBindingCubeMap

--------------------------------------------------------------------------------

data CubeMapTarget =
     TextureCubeMapPositiveX
   | TextureCubeMapNegativeX
   | TextureCubeMapPositiveY
   | TextureCubeMapNegativeY
   | TextureCubeMapPositiveZ
   | TextureCubeMapNegativeZ
   deriving ( Eq, Ord, Show )

marshalCubeMapTarget :: CubeMapTarget -> GLenum
marshalCubeMapTarget x = case x of
   TextureCubeMapPositiveX -> gl_TEXTURE_CUBE_MAP_POSITIVE_X
   TextureCubeMapNegativeX -> gl_TEXTURE_CUBE_MAP_NEGATIVE_X
   TextureCubeMapPositiveY -> gl_TEXTURE_CUBE_MAP_POSITIVE_Y
   TextureCubeMapNegativeY -> gl_TEXTURE_CUBE_MAP_NEGATIVE_Y
   TextureCubeMapPositiveZ -> gl_TEXTURE_CUBE_MAP_POSITIVE_Z
   TextureCubeMapNegativeZ -> gl_TEXTURE_CUBE_MAP_NEGATIVE_Z

unmarshalCubeMapTarget :: GLenum -> CubeMapTarget
unmarshalCubeMapTarget x
   | x == gl_TEXTURE_CUBE_MAP_POSITIVE_X = TextureCubeMapPositiveX
   | x == gl_TEXTURE_CUBE_MAP_NEGATIVE_X = TextureCubeMapNegativeX
   | x == gl_TEXTURE_CUBE_MAP_POSITIVE_Y = TextureCubeMapPositiveY
   | x == gl_TEXTURE_CUBE_MAP_NEGATIVE_Y = TextureCubeMapNegativeY
   | x == gl_TEXTURE_CUBE_MAP_POSITIVE_Z = TextureCubeMapPositiveZ
   | x == gl_TEXTURE_CUBE_MAP_NEGATIVE_Z = TextureCubeMapNegativeZ
   | otherwise = error $ "unmarshalCubeMapTarget: unknown enum " ++ show x

