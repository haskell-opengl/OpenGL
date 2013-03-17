--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.StateVar
-- Copyright   :  (c) Sven Panne 2009
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- State variables are references in the IO monad, like 'IORef's or parts of
-- the OpenGL state. Note that state variables are not neccessarily writable or
-- readable, they may come in read-only or write-only flavours, too. As a very
-- simple example for a state variable, consider an explicitly allocated memory
-- buffer. This buffer can easily be converted into a 'StateVar':
--
-- @
-- makeStateVarFromPtr :: Storable a => Ptr a -> StateVar a
-- makeStateVarFromPtr p = makeStateVar (peek p) (poke p)
-- @
--
-- The example below puts 11 into a state variable (i.e. into the buffer),
-- increments the contents of the state variable by 22, and finally prints the
-- resulting content:
--
-- @
--   do p <- malloc :: IO (Ptr Int)
--      let v = makeStateVarFromPtr p
--      v $= 11
--      v $~ (+ 22)
--      x <- get v
--      print x
-- @
--
-- 'IORef's are state variables, too, so an example with them looks extremely
-- similiar:
--
-- @
--   do v <- newIORef (0 :: Int)
--      v $= 11
--      v $~ (+ 22)
--      x <- get v
--      print x
-- @
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.StateVar (
   -- * Readable State Variables
   HasGetter(..),
   GettableStateVar, makeGettableStateVar,
   -- * Writable State Variables
   HasSetter(..),
   SettableStateVar, makeSettableStateVar,
   -- * General State Variables
   StateVar, makeStateVar,
   -- * Utility Functions
   ($~), ($=!), ($~!)
) where

import Data.IORef ( IORef, readIORef, writeIORef )

--------------------------------------------------------------------------------

infixr 2 $=

--------------------------------------------------------------------------------

-- | The class of all readable state variables.
class HasGetter g where
   -- | Read the value of a state variable.
   get :: g a -> IO a

instance HasGetter IORef where
   get = readIORef

-- | A concrete implementation of a read-only state variable, carrying an IO
-- action to read the value.
newtype GettableStateVar a = GettableStateVar (IO a)

instance HasGetter GettableStateVar where
   get (GettableStateVar g) = g

-- | Construct a 'GettableStateVar' from an IO action.
makeGettableStateVar :: IO a -> GettableStateVar a
makeGettableStateVar = GettableStateVar

--------------------------------------------------------------------------------

-- | The class of all writable state variables.
class HasSetter s where
   -- | Write a new value into a state variable.
   ($=) :: s a -> a -> IO ()

instance HasSetter IORef where
   ($=) = writeIORef

-- | A concrete implementation of a write-only state variable, carrying an IO
-- action to write the new value.
newtype SettableStateVar a = SettableStateVar (a -> IO ())

instance HasSetter SettableStateVar where
   ($=) (SettableStateVar s) a = s a

-- | Construct a 'SettableStateVar' from an IO action.
makeSettableStateVar :: (a -> IO ()) -> SettableStateVar a
makeSettableStateVar = SettableStateVar

--------------------------------------------------------------------------------

-- | A concrete implementation of a readable and writable state variable,
-- carrying one IO action to read the value and another IO action to write the
-- new value.
data StateVar a =
   StateVar (GettableStateVar a) (SettableStateVar a)

instance HasGetter StateVar where
   get (StateVar g _) = get g

instance HasSetter StateVar where
   ($=) (StateVar _ s) a = s $= a

-- | Construct a 'StateVar' from two IO actions, one for reading and one for
-- writing.
makeStateVar :: IO a -> (a -> IO ()) -> StateVar a
makeStateVar g s = StateVar (makeGettableStateVar g) (makeSettableStateVar s)

--------------------------------------------------------------------------------

-- | A modificator convenience function, transforming the contents of a state
-- variable with a given funtion.

($~) :: (HasGetter v, HasSetter v) => v a -> (a -> a) -> IO ()
v $~ f = do
   x <- get v
   v $= f x

-- | A variant of '$=' which is strict in the value to be set.
($=!) :: HasSetter s => s a -> a -> IO ()
v $=! x = x `seq` v $= x

-- | A variant of '$~' which is strict in the transformed value.
($~!) :: (HasGetter v, HasSetter v) => v a -> (a -> a) -> IO ()
v $~! f = do
   x <- get v
   v $=! f x
