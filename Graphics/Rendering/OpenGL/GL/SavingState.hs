--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.SavingState
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 6.1.14 (Saving and Restoring State) of the
-- OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.SavingState (
   ServerAttributeGroup(..), preservingAttrib,
   ClientAttributeGroup(..), preservingClientAttrib
) where

import Graphics.Rendering.OpenGL.GL.Exception ( bracket_ )
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (
   glPopAttrib, glPopClientAttrib, glPushAttrib, glPushClientAttrib,
   gl_ACCUM_BUFFER_BIT, gl_ALL_ATTRIB_BITS, gl_CLIENT_ALL_ATTRIB_BITS,
   gl_CLIENT_PIXEL_STORE_BIT, gl_CLIENT_VERTEX_ARRAY_BIT,
   gl_CURRENT_BIT, gl_ENABLE_BIT, gl_EVAL_BIT, gl_FOG_BIT,
   gl_HINT_BIT, gl_LIGHTING_BIT, gl_LINE_BIT, gl_LIST_BIT,
   gl_MULTISAMPLE_BIT, gl_PIXEL_MODE_BIT, gl_POINT_BIT,
   gl_POLYGON_BIT, gl_POLYGON_STIPPLE_BIT, gl_SCISSOR_BIT,
   gl_TEXTURE_BIT, gl_TRANSFORM_BIT, gl_VIEWPORT_BIT )
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

data ServerAttributeGroup =
     CurrentAttributes
   | PointAttributes
   | LineAttributes
   | PolygonAttributes
   | PolygonStippleAttributes
   | PixelModeAttributes
   | LightingAttributes
   | FogAttributes
   | DepthBufferAttributes
   | AccumBufferAttributes
   | StencilBufferAttributes
   | ViewportAttributes
   | TransformAttributes
   | EnableAttributes
   | ColorBufferAttributes
   | HintAttributes
   | EvalAttributes
   | ListAttributes
   | TextureAttributes
   | ScissorAttributes
   | MultisampleAttributes
   | AllServerAttributes
   deriving ( Eq, Ord, Show )

marshalServerAttributeGroup :: ServerAttributeGroup -> GLbitfield
marshalServerAttributeGroup x = fromIntegral $ case x of
   CurrentAttributes -> gl_CURRENT_BIT
   PointAttributes -> gl_POINT_BIT
   LineAttributes -> gl_LINE_BIT
   PolygonAttributes -> gl_POLYGON_BIT
   PolygonStippleAttributes -> gl_POLYGON_STIPPLE_BIT
   PixelModeAttributes -> gl_PIXEL_MODE_BIT
   LightingAttributes -> gl_LIGHTING_BIT
   FogAttributes -> gl_FOG_BIT
   DepthBufferAttributes -> gl_DEPTH_BUFFER_BIT
   AccumBufferAttributes -> gl_ACCUM_BUFFER_BIT
   StencilBufferAttributes -> gl_STENCIL_BUFFER_BIT
   ViewportAttributes -> gl_VIEWPORT_BIT
   TransformAttributes -> gl_TRANSFORM_BIT
   EnableAttributes -> gl_ENABLE_BIT
   ColorBufferAttributes -> gl_COLOR_BUFFER_BIT
   HintAttributes -> gl_HINT_BIT
   EvalAttributes -> gl_EVAL_BIT
   ListAttributes -> gl_LIST_BIT
   TextureAttributes -> gl_TEXTURE_BIT
   ScissorAttributes -> gl_SCISSOR_BIT
   MultisampleAttributes -> gl_MULTISAMPLE_BIT
   AllServerAttributes -> gl_ALL_ATTRIB_BITS

--------------------------------------------------------------------------------

preservingAttrib :: [ServerAttributeGroup] -> IO a -> IO a
preservingAttrib groups = bracket_ (pushAttrib groups) glPopAttrib

pushAttrib :: [ServerAttributeGroup] -> IO ()
pushAttrib = glPushAttrib . sum . map marshalServerAttributeGroup

--------------------------------------------------------------------------------

data ClientAttributeGroup =
     PixelStoreAttributes
   | VertexArrayAttributes
   | AllClientAttributes
   deriving ( Eq, Ord, Show )

marshalClientAttributeGroup :: ClientAttributeGroup -> GLbitfield
marshalClientAttributeGroup x = fromIntegral $ case x of
   PixelStoreAttributes -> gl_CLIENT_PIXEL_STORE_BIT
   VertexArrayAttributes -> gl_CLIENT_VERTEX_ARRAY_BIT
   AllClientAttributes -> gl_CLIENT_ALL_ATTRIB_BITS

--------------------------------------------------------------------------------

preservingClientAttrib :: [ClientAttributeGroup] -> IO a -> IO a
preservingClientAttrib groups =
   bracket_ (pushClientAttrib groups) glPopClientAttrib

pushClientAttrib :: [ClientAttributeGroup] -> IO ()
pushClientAttrib = glPushClientAttrib . sum . map marshalClientAttributeGroup
