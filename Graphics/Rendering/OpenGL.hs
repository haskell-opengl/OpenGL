--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- A convenience module, combining the Haskell bindings for GL and GLU.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL (
   -- * Legal stuff

   -- $LegalStuff

     module Graphics.Rendering.OpenGL.GL
   , module Graphics.Rendering.OpenGL.GLU
) where

import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.OpenGL.GLU

--------------------------------------------------------------------------------
-- $LegalStuff
-- The documentation is loosely based on the man pages of the OpenGL Sample
-- Implemenation from SGI, see: <http://oss.sgi.com/projects/ogl-sample/>. It is
-- used under the SGI Free Software License B. This license requires the
-- following notice:
--
-- /License Applicability/. Except to the extent portions of this file are made
-- subject to an alternative license as permitted in the SGI Free Software
-- License B, Version 1.1 (the \"License\"), the contents of this file are
-- subject only to the provisions of the License. You may not use this file
-- except in compliance with the License. You may obtain a copy of the License
-- at Silicon Graphics, Inc., attn: Legal Services, 1600 Amphitheatre Parkway,
-- Mountain View, CA 94043-1351, or at: <http://oss.sgi.com/projects/FreeB/>.
-- 
-- Note that, as provided in the License, the Software is distributed on an \"AS
-- IS\" basis, with ALL EXPRESS AND IMPLIED WARRANTIES AND CONDITIONS
-- DISCLAIMED, INCLUDING, WITHOUT LIMITATION, ANY IMPLIED WARRANTIES AND
-- CONDITIONS OF MERCHANTABILITY, SATISFACTORY QUALITY, FITNESS FOR A PARTICULAR
-- PURPOSE, AND NON-INFRINGEMENT.
--
-- /Original Code/. The Original Code is: OpenGL Sample Implementation, Version
-- 1.2.1, released January 26, 2000, developed by Silicon Graphics, Inc. The
-- Original Code is Copyright (c) 1991-2002 Silicon Graphics, Inc.  Copyright in
-- any portions created by third parties is as indicated elsewhere herein. All
-- Rights Reserved.
