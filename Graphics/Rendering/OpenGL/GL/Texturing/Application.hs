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

import Data.StateVar
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget

--------------------------------------------------------------------------------

-- ToDo: cube maps
texture :: TextureTarget t => t -> StateVar Capability
texture = makeCapability . textureTargetToEnableCap
