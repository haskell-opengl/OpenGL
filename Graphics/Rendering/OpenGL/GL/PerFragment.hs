--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PerFragment
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 4.1 (Per-Fragment Operations) of the
-- OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PerFragment (
   depthTest
) where

import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapDepthTest), makeCapability )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar )

--------------------------------------------------------------------------------

depthTest :: StateVar Bool
depthTest = makeCapability CapDepthTest
