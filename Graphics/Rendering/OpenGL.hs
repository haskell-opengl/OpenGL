--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
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
-- There is support for full OpenGL 1.5, including the imaging subset, plus some
-- extensions:
--
-- @
-- extension                      | core since
-- -------------------------------+------------
-- GL_APPLE_packed_pixels         | 1.2
-- GL_ARB_depth_texture           | 1.4
-- GL_ARB_imaging                 |
-- GL_ARB_multisample             | 1.3
-- GL_ARB_multitexture            | 1.3
-- GL_ARB_occlusion_query         | 1.5
-- GL_ARB_point_parameters        | 1.4
-- GL_ARB_point_sprite            |
-- GL_ARB_shadow                  | 1.4
-- GL_ARB_texture_border_clamp    | 1.3
-- GL_ARB_texture_compression     | 1.3
-- GL_ARB_texture_cube_map        | 1.3
-- GL_ARB_texture_env_add         | 1.3
-- GL_ARB_texture_env_combine     | 1.3
-- GL_ARB_texture_env_crossbar    | 1.4
-- GL_ARB_texture_env_dot3        | 1.3
-- GL_ARB_texture_mirrored_repeat | 1.4
-- GL_ARB_transpose_matrix        | 1.3
-- GL_ARB_vertex_buffer_object    | 1.5
-- GL_ARB_window_pos              | 1.4
-- GL_EXT_abgr                    |
-- GL_EXT_bgra                    | 1.2
-- GL_EXT_blend_color             | 1.4
-- GL_EXT_blend_func_separate     | 1.4
-- GL_EXT_blend_logic_op          | 1.1
-- GL_EXT_blend_minmax            | 1.4
-- GL_EXT_blend_subtract          | 1.4
-- GL_EXT_color_subtable          |
-- GL_EXT_compiled_vertex_array   |
-- GL_EXT_convolution             |
-- GL_EXT_copy_texture            | 1.1
-- GL_EXT_depth_bounds_test       |
-- GL_EXT_draw_range_elements     | 1.2
-- GL_EXT_fog_coord               | 1.4
-- GL_EXT_histogram               |
-- GL_EXT_multi_draw_arrays       | 1.4
-- GL_EXT_packed_pixels           | 1.2
-- GL_EXT_polygon_offset          | 1.1
-- GL_EXT_rescale_normal          | 1.2
-- GL_EXT_secondary_color         | 1.4
-- GL_EXT_separate_specular_color | 1.2
-- GL_EXT_shadow_funcs            | 1.5
-- GL_EXT_stencil_two_side        |
-- GL_EXT_stencil_wrap            | 1.4
-- GL_EXT_subtexture              | 1.1
-- GL_EXT_texture                 | 1.1
-- GL_EXT_texture3D               | 1.2
-- GL_EXT_texture_lod_bias        | 1.4
-- GL_EXT_texture_object          | 1.1
-- GL_EXT_vertex_array            | 1.1
-- GL_HP_convolution_border_modes |
-- GL_IBM_rasterpos_clip          |
-- GL_NV_blend_square             | 1.4
-- GL_NV_depth_clamp              |
-- GL_NV_fog_distance             |
-- GL_NV_light_max_exponent       |
-- GL_NV_packed_depth_stencil     |
-- GL_NV_primitive_restart        |
-- GL_SGIS_generate_mipmap        | 1.4
-- GL_SGIS_texture_edge_clamp     |
-- GL_SGIS_texture_lod            |
-- GL_SGI_color_matrix            |
-- GL_SGI_color_table             |
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
