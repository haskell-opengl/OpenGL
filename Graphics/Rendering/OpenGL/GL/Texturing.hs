--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing
-- Copyright   :  (c) Sven Panne 2002-2004
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 3.8 (Texturing) of the OpenGL 1.5 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing (
   TextureTarget(..), marshalTextureTarget,
   PixelInternalFormat(..)
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )
import Graphics.Rendering.OpenGL.GL.PixelInternalFormat (
   PixelInternalFormat(..) )

--------------------------------------------------------------------------------

data TextureTarget =
     Texture1D
   | Texture2D
   | Texture3D
   | ProxyTexture1D
   | ProxyTexture2D
   | ProxyTexture3D
   | TextureCubeMap
   | ProxyTextureCubeMap
   | TextureCubeMapPositiveX
   | TextureCubeMapNegativeX
   | TextureCubeMapPositiveY
   | TextureCubeMapNegativeY
   | TextureCubeMapPositiveZ
   | TextureCubeMapNegativeZ
   deriving ( Eq, Ord, Show )

marshalTextureTarget :: TextureTarget -> GLenum
marshalTextureTarget x = case x of
   Texture1D -> 0xde0
   Texture2D -> 0xde1
   Texture3D -> 0x806f
   ProxyTexture1D -> 0x8063
   ProxyTexture2D -> 0x8064
   ProxyTexture3D -> 0x8070
   TextureCubeMap -> 0x8513
   ProxyTextureCubeMap -> 0x851b
   TextureCubeMapPositiveX -> 0x8515
   TextureCubeMapNegativeX -> 0x8516
   TextureCubeMapPositiveY -> 0x8517
   TextureCubeMapNegativeY -> 0x8518
   TextureCubeMapPositiveZ -> 0x8519
   TextureCubeMapNegativeZ -> 0x851a

--------------------------------------------------------------------------------
