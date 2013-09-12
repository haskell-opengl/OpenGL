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
   -- * Texture Target Classification
   BindableTextureTarget(..),
   TextureTargetCompleteWithMultisample(..),
   TextureTargetSingleWithMultisample(..),
   TextureTargetSingleWithoutMultisample(..),

   -- * One-Dimensional Texture Targets
   TextureTarget1D(..),

   -- * Two-Dimensional Texture Targets
   TextureTarget2D(..),
   TextureTarget2DMultisample(..),
   TextureTargetCubeMap(..),
   TextureTargetCubeMapFace(..),
   unmarshalTextureTargetCubeMapFace,

   -- * Three-Dimensional Texture Targets
   TextureTarget3D(..),
   TextureTarget2DMultisampleArray(..),

   -- * Texture Buffer Target
   TextureTargetBuffer(..)
) where

import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.PixelRectangles
import Graphics.Rendering.OpenGL.GL.QueryUtils.PName
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

{-
  TEXTURE_1D
  ------------------------------------------------------------------------------
  TEXTURE_2D
  TEXTURE_2D_MULTISAMPLE
  TEXTURE_1D_ARRAY
  TEXTURE_RECTANGLE
  TEXTURE_CUBE_MAP
  ------------------------------------------------------------------------------
  TEXTURE_3D
  TEXTURE_2D_ARRAY
  TEXTURE_2D_MULTISAMPLE_ARRAY
  TEXTURE_CUBE_MAP_ARRAY
  ------------------------------------------------------------------------------
  TEXTURE_BUFFER
-}
class BindableTextureTarget t where
   marshalBindableTextureTarget :: t -> GLenum          -- only used for 'glBindTexture'
   marshalBindableTextureTargetPName1I :: t -> PName1I  -- only used for 'get textureBinding'

--------------------------------------------------------------------------------

{-
  TEXTURE_1D
  ------------------------------------------------------------------------------
  TEXTURE_2D
  TEXTURE_2D_MULTISAMPLE
  TEXTURE_1D_ARRAY
  TEXTURE_RECTANGLE
  TEXTURE_CUBE_MAP
  ------------------------------------------------------------------------------
  TEXTURE_3D
  TEXTURE_2D_ARRAY
  TEXTURE_2D_MULTISAMPLE_ARRAY
  TEXTURE_CUBE_MAP_ARRAY
-}
class TextureTargetCompleteWithMultisample t where
   marshalTextureTargetCompleteWithMultisample :: Proxy -> t -> GLenum
   marshalTextureTargetCompleteWithMultisampleEnableCap :: t -> EnableCap

--------------------------------------------------------------------------------

{-
  TEXTURE_1D
  ------------------------------------------------------------------------------
  TEXTURE_2D
  TEXTURE_2D_MULTISAMPLE
  TEXTURE_1D_ARRAY
  TEXTURE_RECTANGLE
  TEXTURE_CUBE_MAP_POSITIVE_X
  TEXTURE_CUBE_MAP_NEGATIVE_X
  TEXTURE_CUBE_MAP_POSITIVE_Y
  TEXTURE_CUBE_MAP_NEGATIVE_Y
  TEXTURE_CUBE_MAP_POSITIVE_Z
  TEXTURE_CUBE_MAP_NEGATIVE_Z
  ------------------------------------------------------------------------------
  TEXTURE_3D
  TEXTURE_2D_ARRAY
  TEXTURE_2D_MULTISAMPLE_ARRAY
  TEXTURE_CUBE_MAP_ARRAY
-}
class TextureTargetSingleWithMultisample t where
   marshalTextureTargetSingleWithMultisample :: t -> GLenum

--------------------------------------------------------------------------------

{-
  TEXTURE_1D
  ------------------------------------------------------------------------------
  TEXTURE_2D
  TEXTURE_1D_ARRAY
  TEXTURE_RECTANGLE
  TEXTURE_CUBE_MAP_POSITIVE_X
  TEXTURE_CUBE_MAP_NEGATIVE_X
  TEXTURE_CUBE_MAP_POSITIVE_Y
  TEXTURE_CUBE_MAP_NEGATIVE_Y
  TEXTURE_CUBE_MAP_POSITIVE_Z
  TEXTURE_CUBE_MAP_NEGATIVE_Z
  ------------------------------------------------------------------------------
  TEXTURE_3D
  TEXTURE_2D_ARRAY
  TEXTURE_CUBE_MAP_ARRAY
-}
class TextureTargetSingleWithoutMultisample t where
   marshalTextureTargetSingleWithoutMultisample :: Proxy -> t -> GLenum

--------------------------------------------------------------------------------

data TextureTarget1D = Texture1D
   deriving ( Eq, Ord, Show )

instance BindableTextureTarget TextureTarget1D where
   marshalBindableTextureTarget = marshalTextureTargetCompleteWithMultisample NoProxy
   marshalBindableTextureTargetPName1I t = case t of
      Texture1D -> GetTextureBinding1D

instance TextureTargetCompleteWithMultisample TextureTarget1D where
   marshalTextureTargetCompleteWithMultisample p t = case p of
      NoProxy -> case t of
         Texture1D -> gl_TEXTURE_1D
      Proxy -> case t of
         Texture1D -> gl_PROXY_TEXTURE_1D
   marshalTextureTargetCompleteWithMultisampleEnableCap t = case t of
      Texture1D -> CapTexture1D

instance TextureTargetSingleWithMultisample TextureTarget1D where
   marshalTextureTargetSingleWithMultisample = marshalTextureTargetCompleteWithMultisample NoProxy

instance TextureTargetSingleWithoutMultisample TextureTarget1D where
   marshalTextureTargetSingleWithoutMultisample = marshalTextureTargetCompleteWithMultisample

--------------------------------------------------------------------------------

data TextureTarget2D =
     Texture2D
   | Texture1DArray
   | TextureRectangle
   deriving ( Eq, Ord, Show )

instance BindableTextureTarget TextureTarget2D where
   marshalBindableTextureTarget = marshalTextureTargetCompleteWithMultisample NoProxy
   marshalBindableTextureTargetPName1I t = case t of
      Texture2D -> GetTextureBinding2D
      Texture1DArray -> GetTextureBinding1DArray
      TextureRectangle -> GetTextureBindingRectangle

instance TextureTargetCompleteWithMultisample TextureTarget2D where
   marshalTextureTargetCompleteWithMultisample p t = case p of
      NoProxy -> case t of
         Texture2D -> gl_TEXTURE_2D
         Texture1DArray -> gl_TEXTURE_1D_ARRAY
         TextureRectangle -> gl_TEXTURE_RECTANGLE
      Proxy -> case t of
         Texture2D -> gl_PROXY_TEXTURE_2D
         Texture1DArray -> gl_PROXY_TEXTURE_1D_ARRAY
         TextureRectangle -> gl_PROXY_TEXTURE_RECTANGLE
   marshalTextureTargetCompleteWithMultisampleEnableCap t = case t of
      Texture2D -> CapTexture2D
      Texture1DArray -> CapTexture1DArray
      TextureRectangle -> CapTextureRectangle

instance TextureTargetSingleWithMultisample TextureTarget2D where
   marshalTextureTargetSingleWithMultisample = marshalTextureTargetCompleteWithMultisample NoProxy

instance TextureTargetSingleWithoutMultisample TextureTarget2D where
   marshalTextureTargetSingleWithoutMultisample = marshalTextureTargetCompleteWithMultisample

--------------------------------------------------------------------------------

data TextureTarget2DMultisample = Texture2DMultisample
   deriving ( Eq, Ord, Show )

instance BindableTextureTarget TextureTarget2DMultisample where
   marshalBindableTextureTarget = marshalTextureTargetCompleteWithMultisample NoProxy
   marshalBindableTextureTargetPName1I t = case t of
      Texture2DMultisample -> GetTextureBinding2DMultisample

instance TextureTargetCompleteWithMultisample TextureTarget2DMultisample where
   marshalTextureTargetCompleteWithMultisample p t = case p of
      NoProxy -> case t of
         Texture2DMultisample -> gl_TEXTURE_2D_MULTISAMPLE
      Proxy -> case t of
         Texture2DMultisample -> gl_PROXY_TEXTURE_2D_MULTISAMPLE
   marshalTextureTargetCompleteWithMultisampleEnableCap t = case t of
      Texture2DMultisample -> CapTexture2DMultisample

instance TextureTargetSingleWithMultisample TextureTarget2DMultisample where
   marshalTextureTargetSingleWithMultisample = marshalTextureTargetCompleteWithMultisample NoProxy

--------------------------------------------------------------------------------

data TextureTargetCubeMap = TextureCubeMap
   deriving ( Eq, Ord, Show )

instance BindableTextureTarget TextureTargetCubeMap where
   marshalBindableTextureTarget = marshalTextureTargetCompleteWithMultisample NoProxy
   marshalBindableTextureTargetPName1I t = case t of
      TextureCubeMap -> GetTextureBindingCubeMap

instance TextureTargetCompleteWithMultisample TextureTargetCubeMap where
   marshalTextureTargetCompleteWithMultisample p t = case p of
      NoProxy -> case t of
         TextureCubeMap -> gl_TEXTURE_CUBE_MAP
      Proxy -> case t of
         TextureCubeMap -> gl_PROXY_TEXTURE_CUBE_MAP
   marshalTextureTargetCompleteWithMultisampleEnableCap t = case t of
      TextureCubeMap -> CapTextureCubeMap

--------------------------------------------------------------------------------

data TextureTargetCubeMapFace =
     TextureCubeMapPositiveX
   | TextureCubeMapNegativeX
   | TextureCubeMapPositiveY
   | TextureCubeMapNegativeY
   | TextureCubeMapPositiveZ
   | TextureCubeMapNegativeZ
   deriving ( Eq, Ord, Show )

instance TextureTargetSingleWithMultisample TextureTargetCubeMapFace where
   marshalTextureTargetSingleWithMultisample t = case t of
      TextureCubeMapPositiveX -> gl_TEXTURE_CUBE_MAP_POSITIVE_X
      TextureCubeMapNegativeX -> gl_TEXTURE_CUBE_MAP_NEGATIVE_X
      TextureCubeMapPositiveY -> gl_TEXTURE_CUBE_MAP_POSITIVE_Y
      TextureCubeMapNegativeY -> gl_TEXTURE_CUBE_MAP_NEGATIVE_Y
      TextureCubeMapPositiveZ -> gl_TEXTURE_CUBE_MAP_POSITIVE_Z
      TextureCubeMapNegativeZ -> gl_TEXTURE_CUBE_MAP_NEGATIVE_Z

instance TextureTargetSingleWithoutMultisample TextureTargetCubeMapFace where
   marshalTextureTargetSingleWithoutMultisample _proxy = marshalTextureTargetSingleWithMultisample -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!

unmarshalTextureTargetCubeMapFace :: GLenum -> TextureTargetCubeMapFace
unmarshalTextureTargetCubeMapFace x
   | x == gl_TEXTURE_CUBE_MAP_POSITIVE_X = TextureCubeMapPositiveX
   | x == gl_TEXTURE_CUBE_MAP_NEGATIVE_X = TextureCubeMapNegativeX
   | x == gl_TEXTURE_CUBE_MAP_POSITIVE_Y = TextureCubeMapPositiveY
   | x == gl_TEXTURE_CUBE_MAP_NEGATIVE_Y = TextureCubeMapNegativeY
   | x == gl_TEXTURE_CUBE_MAP_POSITIVE_Z = TextureCubeMapPositiveZ
   | x == gl_TEXTURE_CUBE_MAP_NEGATIVE_Z = TextureCubeMapNegativeZ
   | otherwise = error $ "unmarshalTextureTargetCubeMapFace: unknown enum " ++ show x

--------------------------------------------------------------------------------

data TextureTarget3D =
     Texture3D
   | Texture2DArray
   | TextureCubeMapArray
   deriving ( Eq, Ord, Show )

instance BindableTextureTarget TextureTarget3D where
   marshalBindableTextureTarget = marshalTextureTargetCompleteWithMultisample NoProxy
   marshalBindableTextureTargetPName1I t = case t of
      Texture3D -> GetTextureBinding3D
      Texture2DArray -> GetTextureBinding2DArray
      TextureCubeMapArray -> GetTextureBindingCubeMapArray

instance TextureTargetCompleteWithMultisample TextureTarget3D where
   marshalTextureTargetCompleteWithMultisample p t = case p of
      NoProxy -> case t of
         Texture3D -> gl_TEXTURE_3D
         Texture2DArray -> gl_TEXTURE_2D_ARRAY
         TextureCubeMapArray -> gl_TEXTURE_CUBE_MAP_ARRAY
      Proxy -> case t of
         Texture3D -> gl_PROXY_TEXTURE_3D
         Texture2DArray -> gl_PROXY_TEXTURE_2D_ARRAY
         TextureCubeMapArray -> gl_PROXY_TEXTURE_CUBE_MAP_ARRAY
   marshalTextureTargetCompleteWithMultisampleEnableCap t = case t of
      Texture3D -> CapTexture3D
      Texture2DArray -> CapTexture2DArray
      TextureCubeMapArray -> CapTextureCubeMapArray

instance TextureTargetSingleWithMultisample TextureTarget3D where
   marshalTextureTargetSingleWithMultisample = marshalTextureTargetCompleteWithMultisample NoProxy

instance TextureTargetSingleWithoutMultisample TextureTarget3D where
   marshalTextureTargetSingleWithoutMultisample = marshalTextureTargetCompleteWithMultisample

--------------------------------------------------------------------------------

data TextureTarget2DMultisampleArray = Texture2DMultisampleArray
   deriving ( Eq, Ord, Show )

instance BindableTextureTarget TextureTarget2DMultisampleArray where
   marshalBindableTextureTarget = marshalTextureTargetCompleteWithMultisample NoProxy
   marshalBindableTextureTargetPName1I t = case t of
      Texture2DMultisampleArray -> GetTextureBinding2DMultisampleArray

instance TextureTargetCompleteWithMultisample TextureTarget2DMultisampleArray where
   marshalTextureTargetCompleteWithMultisample p t = case p of
      Proxy -> case t of
         Texture2DMultisampleArray -> gl_TEXTURE_2D_MULTISAMPLE_ARRAY
      NoProxy -> case t of
         Texture2DMultisampleArray -> gl_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY
   marshalTextureTargetCompleteWithMultisampleEnableCap t = case t of
      Texture2DMultisampleArray -> CapTexture2DMultisampleArray

instance TextureTargetSingleWithMultisample TextureTarget2DMultisampleArray where
   marshalTextureTargetSingleWithMultisample = marshalTextureTargetCompleteWithMultisample NoProxy

--------------------------------------------------------------------------------

data TextureTargetBuffer = TextureBuffer'
   deriving ( Eq, Ord, Show )

instance BindableTextureTarget TextureTargetBuffer where
   marshalBindableTextureTarget t = case t of
      TextureBuffer' -> gl_TEXTURE_BUFFER
   marshalBindableTextureTargetPName1I t = case t of
      TextureBuffer' -> GetTextureBindingBuffer
