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
   TextureTarget(..),
   TextureTarget1D(..),
   TextureTarget2D(..),
   TextureTarget3D(..),
   CubeMapTarget(..), marshalCubeMapTarget, unmarshalCubeMapTarget,
) where

import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.PixelRectangles
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

class TextureTarget tt where
   -- | the marshal function for texture targets when updating, querying etc.
   marshalTextureTarget          :: tt -> GLenum
   -- | the marshal function for texture targets when binding, this is different for some
   -- targets, e.g. TextureCubeMap
   marshalTextureTargetBind      :: tt -> GLenum
   marshalTextureTargetBind = marshalTextureTarget

   -- | the marshal function for texture targets to there proxy targets
   marshalTextureTargetProxy     :: tt -> GLenum

   -- | marshal a Proxy and Texture Target to their normal enum
   marshalProxyTextureTarget :: Proxy -> tt -> GLenum
   marshalProxyTextureTarget NoProxy t = marshalTextureTarget t
   marshalProxyTextureTarget Proxy   t = marshalTextureTargetProxy t

   -- | marshal a Proxy and Texture Target to their Bind enum
   marshalProxyTextureTargetBind :: Proxy -> tt -> GLenum
   marshalProxyTextureTargetBind NoProxy t = marshalTextureTargetBind t
   marshalProxyTextureTargetBind Proxy   t = marshalTextureTargetProxy t

   -- | The GetPName to query it's maximum size
   textureTargetToMaxQuery  :: tt -> PName1I
   textureTargetToEnableCap :: tt -> EnableCap
   textureTargetToBinding   :: tt -> PName1I

data TextureTarget1D
   = Texture1D
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

data TextureTarget2D
   = Texture2D
   | TextureCubeMap CubeMapTarget
   | TextureRectangle
   | Texture1DArray
   deriving ( Eq, Ord, Show )

instance TextureTarget TextureTarget2D where
   marshalTextureTarget t = case t of
      Texture2D         -> gl_TEXTURE_2D
      TextureRectangle  -> gl_TEXTURE_RECTANGLE
      TextureCubeMap c  -> marshalCubeMapTarget c
      Texture1DArray    -> gl_TEXTURE_1D_ARRAY
   marshalTextureTargetBind t = case t of
      TextureCubeMap _  -> gl_TEXTURE_CUBE_MAP
      _                 -> marshalTextureTarget t
   marshalTextureTargetProxy t = case t of
      Texture2D         -> gl_PROXY_TEXTURE_2D
      TextureRectangle  -> gl_PROXY_TEXTURE_RECTANGLE
      TextureCubeMap _  -> gl_PROXY_TEXTURE_CUBE_MAP
      Texture1DArray    -> gl_PROXY_TEXTURE_1D_ARRAY

   textureTargetToMaxQuery t = case t of
      Texture2D         -> GetMaxTextureSize
      TextureCubeMap _  -> GetMaxCubeMapTextureSize
      TextureRectangle  -> GetMaxRectangleTextureSize
      Texture1DArray    -> GetMaxTextureSize -- TODO: is this correct?
   textureTargetToEnableCap t = case t of
      Texture2D         -> CapTexture2D
      TextureCubeMap _  -> CapTextureCubeMap
      TextureRectangle  -> CapTextureRectangle
      Texture1DArray    -> error "No enablecap for Texture1DArray" -- TODO: is this correct?
   textureTargetToBinding t = case t of
      Texture2D         -> GetTextureBinding2D
      TextureCubeMap _  -> GetTextureBindingCubeMap
      TextureRectangle  -> GetTextureBindingRectangle
      Texture1DArray    -> GetTextureBinding1DArray


data TextureTarget3D
   = Texture3D
   | Texture2DArray
   deriving ( Eq, Ord, Show )

instance TextureTarget TextureTarget3D where
   marshalTextureTarget t = case t of
      Texture3D         -> gl_TEXTURE_3D
      Texture2DArray    -> gl_TEXTURE_2D_ARRAY
   marshalTextureTargetProxy t = case t of
      Texture3D         -> gl_PROXY_TEXTURE_3D
      Texture2DArray    -> gl_PROXY_TEXTURE_2D_ARRAY
   textureTargetToMaxQuery t = case t of
      Texture3D         -> GetMax3DTextureSize
      Texture2DArray    -> GetMaxTextureSize -- TODO: is this correct?
   textureTargetToEnableCap t = case t of
      Texture3D         -> CapTexture3D
      Texture2DArray    -> error "No enablecap for Texture2DArray" -- TODO: is this correct?
   textureTargetToBinding t = case t of
      Texture3D         -> GetTextureBinding3D
      Texture2DArray    -> GetTextureBinding2DArray

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
