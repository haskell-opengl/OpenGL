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
   -- * Supported Extensions

   -- $SupportedExtensions

   -- * Legal stuff

   -- $LegalStuff

     module Graphics.Rendering.OpenGL.GL
   , module Graphics.Rendering.OpenGL.GLU
) where

import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.OpenGL.GLU

--------------------------------------------------------------------------------
-- $SupportedExtensions
-- There is support for full OpenGL 1.4, including the imaging subset, plus some
-- extensions:
--
-- @
-- extension                      | core since
-- -------------------------------+------------
-- GL_APPLE_packed_pixels         | 1.2
-- GL_ARB_depth_texture           | 1.4
-- GL_ARB_imaging                 |
-- GL_ARB_multisample             | 1.3
-- GL_ARB_multitexture            | 1.2.1
-- GL_ARB_point_parameters        | 1.4
-- GL_ARB_transpose_matrix        | 1.3
-- GL_ARB_window_pos              | 1.4
-- GL_EXT_abgr                    |
-- GL_EXT_bgra                    | 1.2
-- GL_EXT_blend_color             | 1.4
-- GL_EXT_blend_func_separate     | 1.4
-- GL_EXT_blend_minmax            | 1.4
-- GL_EXT_blend_subtract          | 1.4
-- GL_EXT_compiled_vertex_array   |
-- GL_EXT_depth_bounds_test       |
-- GL_EXT_draw_range_elements     | 1.2
-- GL_EXT_fog_coord               | 1.4
-- GL_EXT_multi_draw_arrays       | 1.4
-- GL_EXT_packed_pixels           | 1.2
-- GL_EXT_rescale_normal          | 1.2
-- GL_EXT_secondary_color         | 1.4
-- GL_EXT_separate_specular_color | 1.2
-- GL_EXT_stencil_two_side        |
-- GL_EXT_stencil_wrap            | 1.4
-- GL_NV_blend_square             | 1.4
-- GL_NV_depth_clamp              |
-- GL_NV_fog_distance             |
-- GL_NV_light_max_exponent       |
-- GL_NV_packed_depth_stencil     |
-- GL_NV_point_sprite             |
-- GL_NV_primitive_restart        |
-- @

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
