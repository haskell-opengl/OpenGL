--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.ColorSum
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 3.9 (Color Sum) of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.ColorSum (
   colorSum
) where

import Data.StateVar
import Graphics.Rendering.OpenGL.GL.Capability

--------------------------------------------------------------------------------

colorSum :: StateVar Capability
colorSum = makeCapability CapColorSum
