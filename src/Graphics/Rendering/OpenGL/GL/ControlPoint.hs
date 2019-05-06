{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.ControlPoint
-- Copyright   :  (c) Sven Panne 2002-2019
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for handling control points.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.ControlPoint (
   ControlPoint(..)
) where

import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.Tensor
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.Domain
import Graphics.Rendering.OpenGL.GL.VertexArrays
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.GL

--------------------------------------------------------------------------------

class ControlPoint c where
   map1Target       :: Domain d => c d -> GLenum
   map2Target       :: Domain d => c d -> GLenum
   enableCap1       :: Domain d => c d -> EnableCap
   enableCap2       :: Domain d => c d -> EnableCap
   numComponents    :: Domain d => c d -> Stride
   peekControlPoint :: Domain d => Ptr (c d) -> IO (c d)
   pokeControlPoint :: Domain d => Ptr (c d) -> (c d) -> IO ()

instance ControlPoint Vertex3 where
   map1Target       = marshalMapTarget . const Map1Vertex3
   map2Target       = marshalMapTarget . const Map2Vertex3
   enableCap1       = const CapMap1Vertex3
   enableCap2       = const CapMap2Vertex3
   numComponents    = const 3
   peekControlPoint = peek
   pokeControlPoint = poke

instance ControlPoint Vertex4 where
   map1Target       = marshalMapTarget . const Map1Vertex4
   map2Target       = marshalMapTarget . const Map2Vertex4
   enableCap1       = const CapMap1Vertex4
   enableCap2       = const CapMap2Vertex4
   numComponents    = const 4
   peekControlPoint = peek
   pokeControlPoint = poke

instance ControlPoint Index1 where
   map1Target       = marshalMapTarget . const Map1Index
   map2Target       = marshalMapTarget . const Map2Index
   enableCap1       = const CapMap1Index
   enableCap2       = const CapMap2Index
   numComponents    = const 1
   peekControlPoint = peek
   pokeControlPoint = poke

instance ControlPoint Color4 where
   map1Target       = marshalMapTarget . const Map1Color4
   map2Target       = marshalMapTarget . const Map2Color4
   enableCap1       = const CapMap1Color4
   enableCap2       = const CapMap2Color4
   numComponents    = const 4
   peekControlPoint = peek
   pokeControlPoint = poke

instance ControlPoint Normal3 where
   map1Target       = marshalMapTarget . const Map1Normal
   map2Target       = marshalMapTarget . const Map2Normal
   enableCap1       = const CapMap1Normal
   enableCap2       = const CapMap2Normal
   numComponents    = const 3
   peekControlPoint = peek
   pokeControlPoint = poke

instance ControlPoint TexCoord1 where
   map1Target       = marshalMapTarget . const Map1TextureCoord1
   map2Target       = marshalMapTarget . const Map2TextureCoord1
   enableCap1       = const CapMap1TextureCoord1
   enableCap2       = const CapMap2TextureCoord1
   numComponents    = const 1
   peekControlPoint = peek
   pokeControlPoint = poke

instance ControlPoint TexCoord2 where
   map1Target       = marshalMapTarget . const Map1TextureCoord2
   map2Target       = marshalMapTarget . const Map2TextureCoord2
   enableCap1       = const CapMap1TextureCoord2
   enableCap2       = const CapMap2TextureCoord2
   numComponents    = const 2
   peekControlPoint = peek
   pokeControlPoint = poke

instance ControlPoint TexCoord3 where
   map1Target       = marshalMapTarget . const Map1TextureCoord3
   map2Target       = marshalMapTarget . const Map2TextureCoord3
   enableCap1       = const CapMap1TextureCoord3
   enableCap2       = const CapMap2TextureCoord3
   numComponents    = const 3
   peekControlPoint = peek
   pokeControlPoint = poke

instance ControlPoint TexCoord4 where
   map1Target       = marshalMapTarget . const Map1TextureCoord4
   map2Target       = marshalMapTarget . const Map2TextureCoord4
   enableCap1       = const CapMap1TextureCoord4
   enableCap2       = const CapMap2TextureCoord4
   numComponents    = const 4
   peekControlPoint = peek
   pokeControlPoint = poke

--------------------------------------------------------------------------------

data MapTarget =
     Map1Color4
   | Map1Index
   | Map1Normal
   | Map1TextureCoord1
   | Map1TextureCoord2
   | Map1TextureCoord3
   | Map1TextureCoord4
   | Map1Vertex3
   | Map1Vertex4
   | Map2Color4
   | Map2Index
   | Map2Normal
   | Map2TextureCoord1
   | Map2TextureCoord2
   | Map2TextureCoord3
   | Map2TextureCoord4
   | Map2Vertex3
   | Map2Vertex4

marshalMapTarget :: MapTarget -> GLenum
marshalMapTarget x = case x of
   Map1Color4 -> GL_MAP1_COLOR_4
   Map1Index -> GL_MAP1_INDEX
   Map1Normal -> GL_MAP1_NORMAL
   Map1TextureCoord1 -> GL_MAP1_TEXTURE_COORD_1
   Map1TextureCoord2 -> GL_MAP1_TEXTURE_COORD_2
   Map1TextureCoord3 -> GL_MAP1_TEXTURE_COORD_3
   Map1TextureCoord4 -> GL_MAP1_TEXTURE_COORD_4
   Map1Vertex3 -> GL_MAP1_VERTEX_3
   Map1Vertex4 -> GL_MAP1_VERTEX_4
   Map2Color4 -> GL_MAP2_COLOR_4
   Map2Index -> GL_MAP2_INDEX
   Map2Normal -> GL_MAP2_NORMAL
   Map2TextureCoord1 -> GL_MAP2_TEXTURE_COORD_1
   Map2TextureCoord2 -> GL_MAP2_TEXTURE_COORD_2
   Map2TextureCoord3 -> GL_MAP2_TEXTURE_COORD_3
   Map2TextureCoord4 -> GL_MAP2_TEXTURE_COORD_4
   Map2Vertex3 -> GL_MAP2_VERTEX_3
   Map2Vertex4 -> GL_MAP2_VERTEX_4
