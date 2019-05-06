{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget
-- Copyright   :  (c) Sven Panne 2002-2019
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
import Graphics.GL

--------------------------------------------------------------------------------

class BindableTextureTarget t where
   marshalBindableTextureTarget :: t -> GLenum
   marshalBindableTextureTargetPName1I :: t -> PName1I

class ParameterizedTextureTarget t where
   marshalParameterizedTextureTarget :: t -> GLenum
   marshalParameterizedTextureTargetProxy :: t -> GLenum
   marshalParameterizedTextureTargetEnableCap :: t -> EnableCap

class OneDimensionalTextureTarget t where
   marshalOneDimensionalTextureTarget :: Proxy -> t -> GLenum

class TwoDimensionalTextureTarget t where
   marshalTwoDimensionalTextureTarget :: Proxy -> t -> GLenum

class ThreeDimensionalTextureTarget t where
   marshalThreeDimensionalTextureTarget :: Proxy -> t -> GLenum

class QueryableTextureTarget t where
   marshalQueryableTextureTarget :: t -> GLenum

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
      Texture1D -> GL_TEXTURE_1D
   marshalParameterizedTextureTargetProxy t = case t of
      Texture1D -> GL_PROXY_TEXTURE_1D
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
      Texture2D -> GL_TEXTURE_2D
      Texture1DArray -> GL_TEXTURE_1D_ARRAY
      TextureRectangle -> GL_TEXTURE_RECTANGLE
   marshalParameterizedTextureTargetProxy t = case t of
      Texture2D -> GL_PROXY_TEXTURE_2D
      Texture1DArray -> GL_PROXY_TEXTURE_1D_ARRAY
      TextureRectangle -> GL_PROXY_TEXTURE_RECTANGLE
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
      Texture2DMultisample -> GL_TEXTURE_2D_MULTISAMPLE
   marshalParameterizedTextureTargetProxy t = case t of
      Texture2DMultisample -> GL_PROXY_TEXTURE_2D_MULTISAMPLE
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
      TextureCubeMap -> GL_TEXTURE_CUBE_MAP
   marshalParameterizedTextureTargetProxy t = case t of
      TextureCubeMap -> GL_PROXY_TEXTURE_CUBE_MAP
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
      -- We could silently map this to TextureCubeMap if we wanted.
      Proxy -> \t -> error ("No proxy target for " ++ show t)

instance QueryableTextureTarget TextureTargetCubeMapFace where
   marshalQueryableTextureTarget t = case t of
      TextureCubeMapPositiveX -> GL_TEXTURE_CUBE_MAP_POSITIVE_X
      TextureCubeMapNegativeX -> GL_TEXTURE_CUBE_MAP_NEGATIVE_X
      TextureCubeMapPositiveY -> GL_TEXTURE_CUBE_MAP_POSITIVE_Y
      TextureCubeMapNegativeY -> GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
      TextureCubeMapPositiveZ -> GL_TEXTURE_CUBE_MAP_POSITIVE_Z
      TextureCubeMapNegativeZ -> GL_TEXTURE_CUBE_MAP_NEGATIVE_Z

instance GettableTextureTarget TextureTargetCubeMapFace where
   marshalGettableTextureTarget = marshalQueryableTextureTarget

unmarshalTextureTargetCubeMapFace :: GLenum -> TextureTargetCubeMapFace
unmarshalTextureTargetCubeMapFace x
   | x == GL_TEXTURE_CUBE_MAP_POSITIVE_X = TextureCubeMapPositiveX
   | x == GL_TEXTURE_CUBE_MAP_NEGATIVE_X = TextureCubeMapNegativeX
   | x == GL_TEXTURE_CUBE_MAP_POSITIVE_Y = TextureCubeMapPositiveY
   | x == GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = TextureCubeMapNegativeY
   | x == GL_TEXTURE_CUBE_MAP_POSITIVE_Z = TextureCubeMapPositiveZ
   | x == GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = TextureCubeMapNegativeZ
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
      Texture3D -> GL_TEXTURE_3D
      Texture2DArray -> GL_TEXTURE_2D_ARRAY
      TextureCubeMapArray -> GL_TEXTURE_CUBE_MAP_ARRAY
   marshalParameterizedTextureTargetProxy t = case t of
      Texture3D -> GL_PROXY_TEXTURE_3D
      Texture2DArray -> GL_PROXY_TEXTURE_2D_ARRAY
      TextureCubeMapArray -> GL_PROXY_TEXTURE_CUBE_MAP_ARRAY
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
      Texture2DMultisampleArray -> GL_TEXTURE_2D_MULTISAMPLE_ARRAY
   marshalParameterizedTextureTargetProxy t = case t of
      Texture2DMultisampleArray -> GL_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY
   marshalParameterizedTextureTargetEnableCap t = case t of
      Texture2DMultisampleArray -> CapTexture2DMultisampleArray

instance QueryableTextureTarget TextureTarget2DMultisampleArray where
   marshalQueryableTextureTarget = marshalParameterizedTextureTarget

--------------------------------------------------------------------------------

data TextureTargetBuffer = TextureBuffer'
   deriving ( Eq, Ord, Show )

instance BindableTextureTarget TextureTargetBuffer where
   marshalBindableTextureTarget t = case t of
      TextureBuffer' -> GL_TEXTURE_BUFFER
   marshalBindableTextureTargetPName1I t = case t of
      TextureBuffer' -> GetTextureBindingBuffer
