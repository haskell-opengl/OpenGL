-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Exception
-- Copyright   :  (c) Sven Panne 2002-2004
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module to compensate for differences between
-- Haskell implementations.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Exception (
   bracket, bracket_, unsafeBracket_, finallyRet
) where

#ifdef __NHC__
import Control.Monad ( liftM2 )

{-# INLINE bracket #-}
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after thing = do
   a <- before 
   r <- thing a
   after a
   return r

{-# INLINE bracket_ #-}
bracket_ :: IO a -> IO b -> IO c -> IO c
bracket_ = unsafeBracket_

{-# INLINE finallyRet #-}
finallyRet :: IO a -> IO b -> IO a
finallyRet = liftM2 (,)
#else
import Control.Exception ( bracket, bracket_, finally )
import Data.IORef ( newIORef, readIORef, writeIORef )

{-# INLINE finallyRet #-}
finallyRet :: IO a -> IO b -> IO (a, b)
a `finallyRet` sequel = do
   r2Ref <- newIORef undefined
   r1 <- a `finally` (sequel >>= writeIORef r2Ref)
   r2 <- readIORef r2Ref
   return (r1, r2)
#endif

{-# INLINE unsafeBracket_ #-}
unsafeBracket_ :: IO a -> IO b -> IO c -> IO c
unsafeBracket_ before after thing = do
   before
   r <- thing
   after
   return r
