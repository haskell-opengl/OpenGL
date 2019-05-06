--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.SavingState
-- Copyright   :  (c) Sven Panne 2002-2019
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
import Graphics.GL

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
   CurrentAttributes -> GL_CURRENT_BIT
   PointAttributes -> GL_POINT_BIT
   LineAttributes -> GL_LINE_BIT
   PolygonAttributes -> GL_POLYGON_BIT
   PolygonStippleAttributes -> GL_POLYGON_STIPPLE_BIT
   PixelModeAttributes -> GL_PIXEL_MODE_BIT
   LightingAttributes -> GL_LIGHTING_BIT
   FogAttributes -> GL_FOG_BIT
   DepthBufferAttributes -> GL_DEPTH_BUFFER_BIT
   AccumBufferAttributes -> GL_ACCUM_BUFFER_BIT
   StencilBufferAttributes -> GL_STENCIL_BUFFER_BIT
   ViewportAttributes -> GL_VIEWPORT_BIT
   TransformAttributes -> GL_TRANSFORM_BIT
   EnableAttributes -> GL_ENABLE_BIT
   ColorBufferAttributes -> GL_COLOR_BUFFER_BIT
   HintAttributes -> GL_HINT_BIT
   EvalAttributes -> GL_EVAL_BIT
   ListAttributes -> GL_LIST_BIT
   TextureAttributes -> GL_TEXTURE_BIT
   ScissorAttributes -> GL_SCISSOR_BIT
   MultisampleAttributes -> GL_MULTISAMPLE_BIT
   AllServerAttributes -> GL_ALL_ATTRIB_BITS

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
   PixelStoreAttributes -> GL_CLIENT_PIXEL_STORE_BIT
   VertexArrayAttributes -> GL_CLIENT_VERTEX_ARRAY_BIT
   AllClientAttributes -> GL_CLIENT_ALL_ATTRIB_BITS

--------------------------------------------------------------------------------

preservingClientAttrib :: [ClientAttributeGroup] -> IO a -> IO a
preservingClientAttrib groups =
   bracket_ (pushClientAttrib groups) glPopClientAttrib

pushClientAttrib :: [ClientAttributeGroup] -> IO ()
pushClientAttrib = glPushClientAttrib . sum . map marshalClientAttributeGroup
