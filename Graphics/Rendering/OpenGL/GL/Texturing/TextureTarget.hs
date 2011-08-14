-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
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

import Graphics.Rendering.OpenGL.GL.PixelRectangles
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

class TextureTarget tt where
   marshalTextureTarget          :: tt -> GLenum
   marshalTextureTargetBind      :: tt -> GLenum
   marshalTextureTargetBind = marshalTextureTarget

   marshalTextureTargetProxy     :: tt -> GLenum

   marshalProxyTextureTarget :: Proxy -> tt -> GLenum
   marshalProxyTextureTarget NoProxy t = marshalTextureTarget t
   marshalProxyTextureTarget Proxy   t = marshalTextureTargetProxy t

   marshalProxyTextureTargetBind :: Proxy -> tt -> GLenum
   marshalProxyTextureTargetBind NoProxy t = marshalTextureTargetBind t
   marshalProxyTextureTargetBind Proxy   t = marshalTextureTargetProxy t

data TextureTarget1D
   = Texture1D
   deriving ( Eq, Ord, Show )

instance TextureTarget TextureTarget1D where
   marshalTextureTarget t = case t of
      Texture1D -> gl_TEXTURE_1D
   marshalTextureTargetProxy t = case t of
      Texture1D -> gl_PROXY_TEXTURE_1D

data TextureTarget2D
   = Texture2D
   | TextureCubeMap CubeMapTarget
   | TextureRectangle
   deriving ( Eq, Ord, Show )

instance TextureTarget TextureTarget2D where
   marshalTextureTarget t = case t of
      Texture2D        -> gl_TEXTURE_2D
      TextureRectangle -> gl_TEXTURE_RECTANGLE
      TextureCubeMap c -> marshalCubeMapTarget c
   marshalTextureTargetBind t = case t of
      TextureCubeMap _ -> gl_TEXTURE_CUBE_MAP
      _                -> marshalTextureTarget t
   marshalTextureTargetProxy t = case t of
      Texture2D        -> gl_PROXY_TEXTURE_2D
      TextureRectangle -> gl_PROXY_TEXTURE_RECTANGLE
      TextureCubeMap _ -> gl_PROXY_TEXTURE_CUBE_MAP

data TextureTarget3D
   = Texture3D
   deriving ( Eq, Ord, Show )

instance TextureTarget TextureTarget3D where
   marshalTextureTarget t = case t of
      Texture3D -> gl_TEXTURE_3D
   marshalTextureTargetProxy t = case t of
      Texture3D -> gl_PROXY_TEXTURE_3D

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

