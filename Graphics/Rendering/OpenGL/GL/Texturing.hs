--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing
-- Copyright   :  (c) Sven Panne 2002-2005
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
   module Graphics.Rendering.OpenGL.GL.Texturing.Specification,
   module Graphics.Rendering.OpenGL.GL.Texturing.Parameters,
   module Graphics.Rendering.OpenGL.GL.Texturing.Objects,
   module Graphics.Rendering.OpenGL.GL.Texturing.Environments,
   module Graphics.Rendering.OpenGL.GL.Texturing.Application,
   module Graphics.Rendering.OpenGL.GL.Texturing.Queries
) where

import Graphics.Rendering.OpenGL.GL.Texturing.Specification
import Graphics.Rendering.OpenGL.GL.Texturing.Parameters
import Graphics.Rendering.OpenGL.GL.Texturing.Objects
import Graphics.Rendering.OpenGL.GL.Texturing.Environments
import Graphics.Rendering.OpenGL.GL.Texturing.Application
import Graphics.Rendering.OpenGL.GL.Texturing.Queries
