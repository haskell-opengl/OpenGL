-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for marshaling texture targets.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget (
   TextureTarget(..), marshalTextureTarget, marshalProxyTextureTarget,
   CubeMapTarget(..), marshalCubeMapTarget
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )
import Graphics.Rendering.OpenGL.GL.PixelRectangles ( Proxy(..) )

--------------------------------------------------------------------------------

data TextureTarget =
     Texture1D
   | Texture2D
   | Texture3D
   | TextureCubeMap
   | TextureRectangle
   deriving ( Eq, Ord, Show )

marshalTextureTarget :: TextureTarget -> GLenum
marshalTextureTarget x = case x of
   Texture1D -> 0xde0
   Texture2D -> 0xde1
   Texture3D -> 0x806f
   TextureCubeMap -> 0x8513
   TextureRectangle -> 0x84f5

marshalProxyTextureTarget :: Proxy -> TextureTarget -> GLenum
marshalProxyTextureTarget NoProxy x = marshalTextureTarget x
marshalProxyTextureTarget Proxy   x = case x of
   Texture1D -> 0x8063
   Texture2D -> 0x8064
   Texture3D -> 0x8070
   TextureCubeMap -> 0x851b
   TextureRectangle -> 0x84f7

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
   TextureCubeMapPositiveX -> 0x8515
   TextureCubeMapNegativeX -> 0x8516
   TextureCubeMapPositiveY -> 0x8517
   TextureCubeMapNegativeY -> 0x8518
   TextureCubeMapPositiveZ -> 0x8519
   TextureCubeMapNegativeZ -> 0x851a
