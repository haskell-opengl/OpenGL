--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.SavingState
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 6.1.12 (Saving and Restoring State) of the
-- OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.SavingState (
   ServerAttributeGroup(..), preservingAttrib,
   ClientAttributeGroup(..), preservingClientAttrib
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLbitfield )
import Graphics.Rendering.OpenGL.GL.Exception ( finally )

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
   CurrentAttributes -> 0x1
   PointAttributes -> 0x2
   LineAttributes -> 0x4
   PolygonAttributes -> 0x8
   PolygonStippleAttributes -> 0x10
   PixelModeAttributes -> 0x20
   LightingAttributes -> 0x40
   FogAttributes -> 0x80
   DepthBufferAttributes -> 0x100
   AccumBufferAttributes -> 0x200
   StencilBufferAttributes -> 0x400
   ViewportAttributes -> 0x800
   TransformAttributes -> 0x1000
   EnableAttributes -> 0x2000
   ColorBufferAttributes -> 0x4000
   HintAttributes -> 0x8000
   EvalAttributes -> 0x10000
   ListAttributes -> 0x20000
   TextureAttributes -> 0x40000
   ScissorAttributes -> 0x80000
   MultisampleAttributes -> 0x20000000
   AllServerAttributes -> 0xffffffff

--------------------------------------------------------------------------------

preservingAttrib :: [ServerAttributeGroup] -> IO a -> IO a
preservingAttrib groups action =
   (do pushAttrib groups ; action) `finally` glPopAttrib

pushAttrib :: [ServerAttributeGroup] -> IO ()
pushAttrib = glPushAttrib . sum . map marshalServerAttributeGroup

foreign import CALLCONV unsafe "glPushAttrib"
   glPushAttrib :: GLbitfield -> IO ()

foreign import CALLCONV unsafe "glPopAttrib"
   glPopAttrib :: IO ()

--------------------------------------------------------------------------------

data ClientAttributeGroup =
     PixelStoreAttributes
   | VertexArrayAttributes
   | AllClientAttributes
   deriving ( Eq, Ord, Show )

marshalClientAttributeGroup :: ClientAttributeGroup -> GLbitfield
marshalClientAttributeGroup x = case x of
   PixelStoreAttributes -> 0x1
   VertexArrayAttributes -> 0x2
   AllClientAttributes -> 0xffffffff

--------------------------------------------------------------------------------

preservingClientAttrib :: [ClientAttributeGroup] -> IO a -> IO a
preservingClientAttrib groups action =
   (do pushClientAttrib groups ; action) `finally` glPopClientAttrib

pushClientAttrib :: [ClientAttributeGroup] -> IO ()
pushClientAttrib = glPushClientAttrib . sum . map marshalClientAttributeGroup

foreign import CALLCONV unsafe "glPushClientAttrib"
   glPushClientAttrib :: GLbitfield -> IO ()

foreign import CALLCONV unsafe "glPopClientAttrib"
   glPopClientAttrib :: IO ()
