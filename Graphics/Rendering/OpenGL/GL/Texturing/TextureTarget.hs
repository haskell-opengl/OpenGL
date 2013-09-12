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
   ParameterizedTextureTarget(..),
   OneDimensionalTextureTarget(..),
   TwoDimensionalTextureTarget(..),
   ThreeDimensionalTextureTarget(..),
   QueryableTextureTarget(..),
   GettableTextureTarget(..),

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
import Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable
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
  Allowed targets:

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
class ParameterizedTextureTarget t where
   marshalParameterizedTextureTarget :: t -> GLenum
   marshalParameterizedTextureTargetProxy :: t -> GLenum
   marshalParameterizedTextureTargetEnableCap :: t -> EnableCap

--------------------------------------------------------------------------------

{-
  Allowed targets:

  TEXTURE_1D
  PROXY_TEXTURE_1D
-}
class OneDimensionalTextureTarget t where
   marshalOneDimensionalTextureTarget :: Proxy -> t -> GLenum

--------------------------------------------------------------------------------

{-
  Allowed targets:

  TEXTURE_2D
  TEXTURE_1D_ARRAY
  TEXTURE_RECTANGLE
  TEXTURE_CUBE_MAP_POSITIVE_X
  TEXTURE_CUBE_MAP_NEGATIVE_X
  TEXTURE_CUBE_MAP_POSITIVE_Y
  TEXTURE_CUBE_MAP_NEGATIVE_Y
  TEXTURE_CUBE_MAP_POSITIVE_Z
  TEXTURE_CUBE_MAP_NEGATIVE_Z
  PROXY_TEXTURE_2D
  PROXY_TEXTURE_1D_ARRAY
  PROXY_TEXTURE_RECTANGLE
  PROXY_TEXTURE_CUBE_MAP

  Note: No TEXTURE_2D_MULTISAMPLE or PROXY_TEXTURE_2D_MULTISAMPLE targets! For
  these use glTexImage2DMultisample.

  Note: We technically allow non-proxy cube map and proxy cube map faces, which is wrong.
-}
class TwoDimensionalTextureTarget t where
   marshalTwoDimensionalTextureTarget :: Proxy -> t -> GLenum

--------------------------------------------------------------------------------

{-
  Allowed targets:

  TEXTURE_3D
  TEXTURE_2D_ARRAY
  TEXTURE_CUBE_MAP_ARRAY
-}

class ThreeDimensionalTextureTarget t where
   marshalThreeDimensionalTextureTarget :: Proxy -> t -> GLenum

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
class QueryableTextureTarget t where
   marshalQueryableTextureTarget :: t -> GLenum

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
class GettableTextureTarget t where
   marshalGettableTextureTarget :: t -> GLenum

--------------------------------------------------------------------------------

data TextureTarget1D = Texture1D
   deriving ( Eq, Ord, Show )

instance BindableTextureTarget TextureTarget1D where
   marshalBindableTextureTarget = marshalParameterizedTextureTarget
   marshalBindableTextureTargetPName1I t = case t of
      Texture1D -> GetTextureBinding1D

instance ParameterizedTextureTarget TextureTarget1D where
   marshalParameterizedTextureTarget t = case t of
      Texture1D -> gl_TEXTURE_1D
   marshalParameterizedTextureTargetProxy t = case t of
      Texture1D -> gl_PROXY_TEXTURE_1D
   marshalParameterizedTextureTargetEnableCap t = case t of
      Texture1D -> CapTexture1D

instance OneDimensionalTextureTarget TextureTarget1D where
   marshalOneDimensionalTextureTarget p = case p of
      NoProxy -> marshalParameterizedTextureTarget
      Proxy -> marshalParameterizedTextureTargetProxy

instance QueryableTextureTarget TextureTarget1D where
   marshalQueryableTextureTarget = marshalParameterizedTextureTarget

instance GettableTextureTarget TextureTarget1D where
   marshalGettableTextureTarget = marshalParameterizedTextureTarget

--------------------------------------------------------------------------------

data TextureTarget2D =
     Texture2D
   | Texture1DArray
   | TextureRectangle
   deriving ( Eq, Ord, Show )

instance BindableTextureTarget TextureTarget2D where
   marshalBindableTextureTarget = marshalParameterizedTextureTarget
   marshalBindableTextureTargetPName1I t = case t of
      Texture2D -> GetTextureBinding2D
      Texture1DArray -> GetTextureBinding1DArray
      TextureRectangle -> GetTextureBindingRectangle

instance ParameterizedTextureTarget TextureTarget2D where
   marshalParameterizedTextureTarget t = case t of
      Texture2D -> gl_TEXTURE_2D
      Texture1DArray -> gl_TEXTURE_1D_ARRAY
      TextureRectangle -> gl_TEXTURE_RECTANGLE
   marshalParameterizedTextureTargetProxy t = case t of
      Texture2D -> gl_PROXY_TEXTURE_2D
      Texture1DArray -> gl_PROXY_TEXTURE_1D_ARRAY
      TextureRectangle -> gl_PROXY_TEXTURE_RECTANGLE
   marshalParameterizedTextureTargetEnableCap t = case t of
      Texture2D -> CapTexture2D
      Texture1DArray -> CapTexture1DArray
      TextureRectangle -> CapTextureRectangle

instance TwoDimensionalTextureTarget TextureTarget2D where
   marshalTwoDimensionalTextureTarget p = case p of
      NoProxy -> marshalParameterizedTextureTarget
      Proxy -> marshalParameterizedTextureTargetProxy

instance QueryableTextureTarget TextureTarget2D where
   marshalQueryableTextureTarget = marshalParameterizedTextureTarget

instance GettableTextureTarget TextureTarget2D where
   marshalGettableTextureTarget = marshalParameterizedTextureTarget

--------------------------------------------------------------------------------

data TextureTarget2DMultisample = Texture2DMultisample
   deriving ( Eq, Ord, Show )

instance BindableTextureTarget TextureTarget2DMultisample where
   marshalBindableTextureTarget = marshalParameterizedTextureTarget
   marshalBindableTextureTargetPName1I t = case t of
      Texture2DMultisample -> GetTextureBinding2DMultisample

instance ParameterizedTextureTarget TextureTarget2DMultisample where
   marshalParameterizedTextureTarget t = case t of
      Texture2DMultisample -> gl_TEXTURE_2D_MULTISAMPLE
   marshalParameterizedTextureTargetProxy t = case t of
      Texture2DMultisample -> gl_PROXY_TEXTURE_2D_MULTISAMPLE
   marshalParameterizedTextureTargetEnableCap t = case t of
      Texture2DMultisample -> CapTexture2DMultisample

instance QueryableTextureTarget TextureTarget2DMultisample where
   marshalQueryableTextureTarget = marshalParameterizedTextureTarget

--------------------------------------------------------------------------------

data TextureTargetCubeMap = TextureCubeMap
   deriving ( Eq, Ord, Show )

instance BindableTextureTarget TextureTargetCubeMap where
   marshalBindableTextureTarget = marshalParameterizedTextureTarget
   marshalBindableTextureTargetPName1I t = case t of
      TextureCubeMap -> GetTextureBindingCubeMap

instance ParameterizedTextureTarget TextureTargetCubeMap where
   marshalParameterizedTextureTarget t = case t of
      TextureCubeMap -> gl_TEXTURE_CUBE_MAP
   marshalParameterizedTextureTargetProxy t = case t of
      TextureCubeMap -> gl_PROXY_TEXTURE_CUBE_MAP
   marshalParameterizedTextureTargetEnableCap t = case t of
      TextureCubeMap -> CapTextureCubeMap

instance TwoDimensionalTextureTarget TextureTargetCubeMap where
   marshalTwoDimensionalTextureTarget p = case p of
      NoProxy -> \t -> error ("No non-proxy target for " ++ show t)
      Proxy -> marshalParameterizedTextureTargetProxy

--------------------------------------------------------------------------------

data TextureTargetCubeMapFace =
     TextureCubeMapPositiveX
   | TextureCubeMapNegativeX
   | TextureCubeMapPositiveY
   | TextureCubeMapNegativeY
   | TextureCubeMapPositiveZ
   | TextureCubeMapNegativeZ
   deriving ( Eq, Ord, Show )

instance TwoDimensionalTextureTarget TextureTargetCubeMapFace where
   marshalTwoDimensionalTextureTarget p = case p of
      NoProxy -> marshalQueryableTextureTarget
      Proxy -> \t -> error ("No proxy target for " ++ show t)

instance QueryableTextureTarget TextureTargetCubeMapFace where
   marshalQueryableTextureTarget t = case t of
      TextureCubeMapPositiveX -> gl_TEXTURE_CUBE_MAP_POSITIVE_X
      TextureCubeMapNegativeX -> gl_TEXTURE_CUBE_MAP_NEGATIVE_X
      TextureCubeMapPositiveY -> gl_TEXTURE_CUBE_MAP_POSITIVE_Y
      TextureCubeMapNegativeY -> gl_TEXTURE_CUBE_MAP_NEGATIVE_Y
      TextureCubeMapPositiveZ -> gl_TEXTURE_CUBE_MAP_POSITIVE_Z
      TextureCubeMapNegativeZ -> gl_TEXTURE_CUBE_MAP_NEGATIVE_Z

instance GettableTextureTarget TextureTargetCubeMapFace where
   marshalGettableTextureTarget = marshalQueryableTextureTarget

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
   marshalBindableTextureTarget = marshalParameterizedTextureTarget
   marshalBindableTextureTargetPName1I t = case t of
      Texture3D -> GetTextureBinding3D
      Texture2DArray -> GetTextureBinding2DArray
      TextureCubeMapArray -> GetTextureBindingCubeMapArray

instance ParameterizedTextureTarget TextureTarget3D where
   marshalParameterizedTextureTarget t = case t of
      Texture3D -> gl_TEXTURE_3D
      Texture2DArray -> gl_TEXTURE_2D_ARRAY
      TextureCubeMapArray -> gl_TEXTURE_CUBE_MAP_ARRAY
   marshalParameterizedTextureTargetProxy t = case t of
      Texture3D -> gl_PROXY_TEXTURE_3D
      Texture2DArray -> gl_PROXY_TEXTURE_2D_ARRAY
      TextureCubeMapArray -> gl_PROXY_TEXTURE_CUBE_MAP_ARRAY
   marshalParameterizedTextureTargetEnableCap t = case t of
      Texture3D -> CapTexture3D
      Texture2DArray -> CapTexture2DArray
      TextureCubeMapArray -> CapTextureCubeMapArray

instance ThreeDimensionalTextureTarget TextureTarget3D where
   marshalThreeDimensionalTextureTarget p = case p of
      NoProxy -> marshalParameterizedTextureTarget
      Proxy -> marshalParameterizedTextureTargetProxy

instance QueryableTextureTarget TextureTarget3D where
   marshalQueryableTextureTarget = marshalParameterizedTextureTarget

instance GettableTextureTarget TextureTarget3D where
   marshalGettableTextureTarget = marshalParameterizedTextureTarget

--------------------------------------------------------------------------------

data TextureTarget2DMultisampleArray = Texture2DMultisampleArray
   deriving ( Eq, Ord, Show )

instance BindableTextureTarget TextureTarget2DMultisampleArray where
   marshalBindableTextureTarget = marshalParameterizedTextureTarget
   marshalBindableTextureTargetPName1I t = case t of
      Texture2DMultisampleArray -> GetTextureBinding2DMultisampleArray

instance ParameterizedTextureTarget TextureTarget2DMultisampleArray where
   marshalParameterizedTextureTarget t = case t of
      Texture2DMultisampleArray -> gl_TEXTURE_2D_MULTISAMPLE_ARRAY
   marshalParameterizedTextureTargetProxy t = case t of
      Texture2DMultisampleArray -> gl_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY
   marshalParameterizedTextureTargetEnableCap t = case t of
      Texture2DMultisampleArray -> CapTexture2DMultisampleArray

instance QueryableTextureTarget TextureTarget2DMultisampleArray where
   marshalQueryableTextureTarget = marshalParameterizedTextureTarget

--------------------------------------------------------------------------------

data TextureTargetBuffer = TextureBuffer'
   deriving ( Eq, Ord, Show )

instance BindableTextureTarget TextureTargetBuffer where
   marshalBindableTextureTarget t = case t of
      TextureBuffer' -> gl_TEXTURE_BUFFER
   marshalBindableTextureTargetPName1I t = case t of
      TextureBuffer' -> GetTextureBindingBuffer
