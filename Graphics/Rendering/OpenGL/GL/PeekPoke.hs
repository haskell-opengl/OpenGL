-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PeekPoke
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This is a purely internal module with peek- and poke-related utilities.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PeekPoke (
   poke1, poke2, poke3, poke4,
   peek1, peek2, peek3, peek4
) where

import Foreign.Ptr ( Ptr, castPtr )
import Foreign.Storable ( Storable(peekElemOff,pokeElemOff) )

--------------------------------------------------------------------------------
-- The implementation is little bit verbose/redundant, but seems to generate
-- better code than mapM/zipWithM_.

--------------------------------------------------------------------------------

{-# INLINE poke1 #-}
poke1 :: Storable b => Ptr a -> b -> IO ()
poke1 ptr x =
   pokeElemOff (castPtr ptr) 0 x

{-# INLINE poke2 #-}
poke2 :: Storable b => Ptr a -> b -> b -> IO ()
poke2 ptr x y = do
   pokeElemOff (castPtr ptr) 0 x
   pokeElemOff (castPtr ptr) 1 y

{-# INLINE poke3 #-}
poke3 :: Storable b => Ptr a -> b -> b -> b -> IO ()
poke3 ptr x y z = do
   pokeElemOff (castPtr ptr) 0 x
   pokeElemOff (castPtr ptr) 1 y
   pokeElemOff (castPtr ptr) 2 z

{-# INLINE poke4 #-}
poke4 :: Storable b => Ptr a -> b -> b -> b -> b -> IO ()
poke4 ptr x y z w = do
   pokeElemOff (castPtr ptr) 0 x
   pokeElemOff (castPtr ptr) 1 y
   pokeElemOff (castPtr ptr) 2 z
   pokeElemOff (castPtr ptr) 3 w

--------------------------------------------------------------------------------

{-# INLINE peek1 #-}
peek1 :: Storable a => (a -> b) -> Ptr c -> IO b
peek1 f ptr = do
   x <- peekElemOff (castPtr ptr) 0
   return $ f x

{-# INLINE peek2 #-}
peek2 :: Storable a => (a -> a -> b) -> Ptr c -> IO b
peek2 f ptr = do
   x <- peekElemOff (castPtr ptr) 0
   y <- peekElemOff (castPtr ptr) 1
   return $ f x y

{-# INLINE peek3 #-}
peek3 :: Storable a => (a -> a -> a -> b) -> Ptr c -> IO b
peek3 f ptr = do
   x <- peekElemOff (castPtr ptr) 0
   y <- peekElemOff (castPtr ptr) 1
   z <- peekElemOff (castPtr ptr) 2
   return $ f x y z

{-# INLINE peek4 #-}
peek4 :: Storable a => (a -> a -> a -> a -> b) -> Ptr c -> IO b
peek4 f ptr = do
   x <- peekElemOff (castPtr ptr) 0
   y <- peekElemOff (castPtr ptr) 1
   z <- peekElemOff (castPtr ptr) 2
   w <- peekElemOff (castPtr ptr) 3
   return $ f x y z w
