--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.ColorSum
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 3.9 (Color Sum) of the OpenGL 1.5 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.ColorSum (
   colorSum
) where

import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapColorSum), makeCapability )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( Capability )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar )

--------------------------------------------------------------------------------

colorSum :: StateVar Capability
colorSum = makeCapability CapColorSum
