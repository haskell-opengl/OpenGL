-----------------------------------------------------------------------------
-- 
-- Module      :  Graphics.Drawing.OpenGL
-- Copyright   :  (c) Sven Panne 2002
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- $Id: OpenGL.hs,v 1.1 2002/02/24 17:18:03 panne Exp $
--
-- A convenience module which combines the Haskell bindings for OpenGL, the
-- industry's most widely used and supported 2D and 3D graphics API, and it's
-- accompanying utility library.
--
-----------------------------------------------------------------------------

module Graphics.Drawing.OpenGL (
     module Graphics.Drawing.OpenGL.GL
   , module Graphics.Drawing.OpenGL.GLU
) where

import Graphics.Drawing.OpenGL.GL
import Graphics.Drawing.OpenGL.GLU
