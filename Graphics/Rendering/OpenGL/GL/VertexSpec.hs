module Graphics.Rendering.OpenGL.GL.VertexSpec where

import Foreign.Ptr ( castPtr )
import Foreign.Storable ( Storable(..) )

data Vertex3 a = Vertex3 a a a deriving ( Eq, Ord )

instance Storable a => Storable (Vertex3 a) where
   sizeOf ~(Vertex3 x _ _) = 3 * sizeOf x

   alignment ~(Vertex3 x _ _) = alignment x

   peek ptr= do
      x <- peekElemOff (castPtr ptr) 0
      y <- peekElemOff (castPtr ptr) 1
      z <- peekElemOff (castPtr ptr) 2
      return $ Vertex3 x y z

   poke ptr (Vertex3 x y z) = do
      pokeElemOff (castPtr ptr) 0 x
      pokeElemOff (castPtr ptr) 1 y
      pokeElemOff (castPtr ptr) 2 z

data Vertex4 a = Vertex4 a a a a deriving ( Eq, Ord )

instance Storable a => Storable (Vertex4 a) where
   sizeOf ~(Vertex4 x _ _ _) = 4 * sizeOf x

   alignment ~(Vertex4 x _ _ _) = alignment x

   peek ptr = do
      x <- peekElemOff (castPtr ptr) 0
      y <- peekElemOff (castPtr ptr) 1
      z <- peekElemOff (castPtr ptr) 2
      w <- peekElemOff (castPtr ptr) 3
      return $ Vertex4 x y z w

   poke ptr (Vertex4 x y z w) = do
      pokeElemOff (castPtr ptr) 0 x
      pokeElemOff (castPtr ptr) 1 y
      pokeElemOff (castPtr ptr) 2 z
      pokeElemOff (castPtr ptr) 3 w

data Normal3 a = Normal3 a a a deriving ( Eq, Ord )

data Color3 a = Color3 a a a deriving ( Eq ,Ord )

newtype ColorIndex a = ColorIndex a deriving ( Eq, Ord )
