--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Shaders
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to sections 2.15 (Vertex Shaders) and section 3.11
-- (Fragment Shaders) of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Shaders (
   module Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects,
   module Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects,
   module Graphics.Rendering.OpenGL.GL.Shaders.Attribs,
   module Graphics.Rendering.OpenGL.GL.Shaders.Uniform,
   module Graphics.Rendering.OpenGL.GL.Shaders.Limits
) where

import Graphics.Rendering.OpenGL.GL.Shaders.Attribs
import Graphics.Rendering.OpenGL.GL.Shaders.Limits
import Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects
import Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects
import Graphics.Rendering.OpenGL.GL.Shaders.Uniform
