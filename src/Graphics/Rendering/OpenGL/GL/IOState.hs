{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.IOState
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for an IO monad with a pointer as an
-- additional state, basically a /StateT (Ptr s) IO a/.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.IOState (
   IOState(..), getIOState, peekIOState, evalIOState, nTimes
) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ( Applicative(..) )
#endif
import Control.Monad ( ap, liftM, replicateM )
import Foreign.Ptr ( Ptr, plusPtr )
import Foreign.Storable ( Storable(sizeOf,peek) )

--------------------------------------------------------------------------------

newtype IOState s a = IOState { runIOState :: Ptr s -> IO (a, Ptr s) }

instance Applicative (IOState s) where
   pure  = return
   (<*>) = ap

instance Functor (IOState s) where
   fmap = liftM

instance Monad (IOState s) where
   return a = IOState $ \s -> return (a, s)
   m >>= k  = IOState $ \s -> do (a, s') <- runIOState m s ; runIOState (k a) s'
   fail str = IOState $ \_ -> fail str

getIOState :: IOState s (Ptr s)
getIOState = IOState $ \s -> return (s, s)

putIOState :: Ptr s -> IOState s ()
putIOState s = IOState $ \_ -> return ((), s)

peekIOState :: Storable a => IOState a a
peekIOState = do
   ptr <- getIOState
   x <- liftIOState $ peek ptr
   putIOState (ptr `plusPtr` sizeOf x)
   return x

liftIOState :: IO a -> IOState s a
liftIOState m = IOState $ \s -> do a <- m ; return (a, s)

evalIOState :: IOState s a -> Ptr s -> IO a
evalIOState m s = do (a, _) <- runIOState m s ; return a

nTimes :: Integral a => a -> IOState b c -> IOState b [c]
nTimes n = replicateM (fromIntegral n)
