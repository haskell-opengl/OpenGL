-----------------------------------------------------------------------------
-- 
-- Module      :  Graphics.Rendering.OpenGL
-- Copyright   :  (c) Sven Panne 2002
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A convenience module which combines the Haskell bindings for OpenGL, the
-- industry's most widely used and supported 2D and 3D graphics API, and it's
-- accompanying utility library.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL (
     module Graphics.Rendering.OpenGL.GL
   , module Graphics.Rendering.OpenGL.GLU
) where

import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.OpenGL.GLU
