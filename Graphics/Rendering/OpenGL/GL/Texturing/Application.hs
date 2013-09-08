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

-- ToDo: cube maps
texture :: TextureTarget t => t -> StateVar Capability
texture = makeCapability . textureTargetToEnableCap
