--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Application
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 3.8.15 (Texture Application) of the
-- OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing.Application (
   texture
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( Capability )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapTexture1D,CapTexture2D,CapTexture3D,CapTextureCubeMap,
             CapTextureRectangle),
   makeCapability )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar )
import Graphics.Rendering.OpenGL.GL.Texturing.Specification (
   TextureTarget(..) )

--------------------------------------------------------------------------------

-- ToDo: cube maps
texture :: TextureTarget -> StateVar Capability
texture = makeCapability . textureTargetToEnableCap

textureTargetToEnableCap :: TextureTarget -> EnableCap
textureTargetToEnableCap x = case x of
    Texture1D -> CapTexture1D
    Texture2D -> CapTexture2D
    Texture3D -> CapTexture3D
    TextureCubeMap -> CapTextureCubeMap
    TextureRectangle -> CapTextureRectangle
