--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.SavingState
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
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
import Graphics.Rendering.OpenGL.Raw

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
marshalServerAttributeGroup x = case x of
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
marshalClientAttributeGroup x = case x of
   PixelStoreAttributes -> gl_CLIENT_PIXEL_STORE_BIT
   VertexArrayAttributes -> gl_CLIENT_VERTEX_ARRAY_BIT
   AllClientAttributes -> gl_CLIENT_ALL_ATTRIB_BITS

--------------------------------------------------------------------------------

preservingClientAttrib :: [ClientAttributeGroup] -> IO a -> IO a
preservingClientAttrib groups =
   bracket_ (pushClientAttrib groups) glPopClientAttrib

pushClientAttrib :: [ClientAttributeGroup] -> IO ()
pushClientAttrib = glPushClientAttrib . sum . map marshalClientAttributeGroup
