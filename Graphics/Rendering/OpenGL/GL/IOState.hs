-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.IOState
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for an IO monad with an additional state.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.IOState (
   IOState(..), getIOState, putIOState, modifyIOState, peekIOState, liftIOState,
   evalIOState, execIOState, mapIOState, withIOState
) where

import Foreign.Ptr ( Ptr, plusPtr )
import Foreign.Storable ( Storable(sizeOf,peek) )

--------------------------------------------------------------------------------

newtype IOState s a = IOState { runIOState :: s -> IO (a, s) }

instance Functor (IOState s) where
   fmap f m = IOState $ \s -> do (x, s') <- runIOState m s ; return (f x, s')

instance Monad (IOState s) where
   return a = IOState $ \s -> return (a, s)
   m >>= k  = IOState $ \s -> do (a, s') <- runIOState m s ; runIOState (k a) s'
   fail str = IOState $ \_ -> fail str

getIOState :: IOState s s
getIOState = IOState $ \s -> return (s, s)

putIOState :: s -> IOState s ()
putIOState s = IOState $ \_ -> return ((), s)

modifyIOState :: (s -> s) -> IOState s ()
modifyIOState f = do s <- getIOState ; putIOState (f s)

peekIOState :: Storable a => IOState (Ptr a) a
peekIOState = do
   ptr <- getIOState
   x <- liftIOState $ peek ptr
   putIOState (ptr `plusPtr` sizeOf x)
   return x

liftIOState :: IO a -> IOState s a
liftIOState m = IOState $ \s -> do a <- m ; return (a, s)

evalIOState :: IOState s a -> s -> IO a
evalIOState m s = do (a, _) <- runIOState m s ; return a

execIOState :: IOState s a -> s -> IO s
execIOState m s = do (_, s') <- runIOState m s ; return s'

mapIOState :: (IO (a, s) -> IO (b, s)) -> IOState s a -> IOState s b
mapIOState f m = IOState $ f . runIOState m

withIOState :: (s -> s) -> IOState s a -> IOState s a
withIOState f m = IOState $ runIOState m . f
