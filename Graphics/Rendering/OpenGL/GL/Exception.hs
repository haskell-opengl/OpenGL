-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Exception
-- Copyright   :  (c) Sven Panne 2002-2005
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

import Data.IORef ( newIORef, readIORef, writeIORef )

#ifdef __NHC__
import qualified IO ( bracket, bracket_ )

{-# INLINE bracket #-}
bracket  :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket = IO.bracket

{-# INLINE bracket_ #-}
bracket_ :: IO a -> IO b -> IO c -> IO c
bracket_ before = IO.bracket_ before . const

finally :: IO a -> IO b -> IO a
finally = flip . bracket_ . return $ undefined
#else
import Control.Exception ( bracket, bracket_, finally )
#endif

{-# INLINE unsafeBracket_ #-}
unsafeBracket_ :: IO a -> IO b -> IO c -> IO c
unsafeBracket_ before after thing = do
   before
   r <- thing
   after
   return r

{-# INLINE finallyRet #-}
finallyRet :: IO a -> IO b -> IO (a, b)
a `finallyRet` sequel = do
   r2Ref <- newIORef undefined
   r1 <- a `finally` (sequel >>= writeIORef r2Ref)
   r2 <- readIORef r2Ref
   return (r1, r2)
