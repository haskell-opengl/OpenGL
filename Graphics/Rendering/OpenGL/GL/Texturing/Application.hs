--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Application
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
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

import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget

--------------------------------------------------------------------------------

{-
  Allowed targets:

  TEXTURE_1D
  ------------------------------------------------------------------------------
  TEXTURE_2D
  TEXTURE_1D_ARRAY       -- MESA_texture_array only
  TEXTURE_RECTANGLE
  TEXTURE_CUBE_MAP
  ------------------------------------------------------------------------------
  TEXTURE_3D
  TEXTURE_2D_ARRAY       -- MESA_texture_array only

  In a nutshell: All non-proxy targets, but no

  TEXTURE_2D_MULTISAMPLE
  TEXTURE_2D_MULTISAMPLE_ARRAY
  TEXTURE_CUBE_MAP_ARRAY

  although there is no principal reason why these might not be allowed in some
  extension in the future.
-}

texture :: TextureTargetCompleteWithMultisample t => t -> StateVar Capability
texture = makeCapability . marshalTextureTargetCompleteWithMultisampleEnableCap
