-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.ControlPoint
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for handling control points.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.ControlPoint (
   ControlPoint(..)
) where

import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable(peek,poke) )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )
import Graphics.Rendering.OpenGL.GL.Capability ( EnableCap(..) )
import Graphics.Rendering.OpenGL.GL.Domain ( Domain )
import Graphics.Rendering.OpenGL.GL.VertexArrays ( Stride )
import Graphics.Rendering.OpenGL.GL.VertexSpec (
   Vertex3, Vertex4, Index1, Color4, Normal3, TexCoord1, TexCoord2, TexCoord3,
   TexCoord4 )

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
   Map1Color4 -> 0xd90
   Map1Index -> 0xd91
   Map1Normal -> 0xd92
   Map1TextureCoord1 -> 0xd93
   Map1TextureCoord2 -> 0xd94
   Map1TextureCoord3 -> 0xd95
   Map1TextureCoord4 -> 0xd96
   Map1Vertex3 -> 0xd97
   Map1Vertex4 -> 0xd98
   Map2Color4 -> 0xdb0
   Map2Index -> 0xdb1
   Map2Normal -> 0xdb2
   Map2TextureCoord1 -> 0xdb3
   Map2TextureCoord2 -> 0xdb4
   Map2TextureCoord3 -> 0xdb5
   Map2TextureCoord4 -> 0xdb6
   Map2Vertex3 -> 0xdb7
   Map2Vertex4 -> 0xdb8
