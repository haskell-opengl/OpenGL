-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PeekPoke
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module with peek- and poke-related utilities.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PeekPoke (
   poke1, poke2, poke3, poke4,
   peek1, peek2, peek3, peek4
) where

import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable(peekElemOff,pokeElemOff) )

--------------------------------------------------------------------------------
-- The implementation is little bit verbose/redundant, but seems to generate
-- better code than mapM/zipWithM_.

--------------------------------------------------------------------------------

{-# INLINE poke1 #-}
poke1 :: Storable a => Ptr a -> a -> IO ()
poke1 ptr x =
   pokeElemOff ptr 0 x

{-# INLINE poke2 #-}
poke2 :: Storable a => Ptr a -> a -> a -> IO ()
poke2 ptr x y = do
   pokeElemOff ptr 0 x
   pokeElemOff ptr 1 y

{-# INLINE poke3 #-}
poke3 :: Storable a => Ptr a -> a -> a -> a -> IO ()
poke3 ptr x y z = do
   pokeElemOff ptr 0 x
   pokeElemOff ptr 1 y
   pokeElemOff ptr 2 z

{-# INLINE poke4 #-}
poke4 :: Storable a => Ptr a -> a -> a -> a -> a -> IO ()
poke4 ptr x y z w = do
   pokeElemOff ptr 0 x
   pokeElemOff ptr 1 y
   pokeElemOff ptr 2 z
   pokeElemOff ptr 3 w

--------------------------------------------------------------------------------

{-# INLINE peek1 #-}
peek1 :: Storable a => (a -> b) -> Ptr a -> IO b
peek1 f ptr = do
   x <- peekElemOff ptr 0
   return $ f x

{-# INLINE peek2 #-}
peek2 :: Storable a => (a -> a -> b) -> Ptr a -> IO b
peek2 f ptr = do
   x <- peekElemOff ptr 0
   y <- peekElemOff ptr 1
   return $ f x y

{-# INLINE peek3 #-}
peek3 :: Storable a => (a -> a -> a -> b) -> Ptr a -> IO b
peek3 f ptr = do
   x <- peekElemOff ptr 0
   y <- peekElemOff ptr 1
   z <- peekElemOff ptr 2
   return $ f x y z

{-# INLINE peek4 #-}
peek4 :: Storable a => (a -> a -> a -> a -> b) -> Ptr a -> IO b
peek4 f ptr = do
   x <- peekElemOff ptr 0
   y <- peekElemOff ptr 1
   z <- peekElemOff ptr 2
   w <- peekElemOff ptr 3
   return $ f x y z w
