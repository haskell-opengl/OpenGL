--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.StateVar
-- Copyright   :  (c) Sven Panne 2002-2004
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.StateVar (
   HasGetter(..),
   GettableStateVar, makeGettableStateVar,
   HasSetter(..), set,
   SettableStateVar, makeSettableStateVar,
   StateVar, makeStateVar
) where

--------------------------------------------------------------------------------

infixr 2 $=

--------------------------------------------------------------------------------

class HasGetter g where
   get :: g a -> IO a

--------------------------------------------------------------------------------

newtype GettableStateVar a = GettableStateVar (IO a)

instance HasGetter GettableStateVar where
   get (GettableStateVar g) = g

makeGettableStateVar :: IO a -> GettableStateVar a
makeGettableStateVar = GettableStateVar

--------------------------------------------------------------------------------

class HasSetter s where
   ($=) :: s a -> a -> IO ()

set :: [IO ()] -> IO ()
set = sequence_

--------------------------------------------------------------------------------

newtype SettableStateVar a = SettableStateVar (a -> IO ())

instance HasSetter SettableStateVar where
   ($=) (SettableStateVar s) a = s a

makeSettableStateVar :: (a -> IO ()) -> SettableStateVar a
makeSettableStateVar = SettableStateVar

--------------------------------------------------------------------------------

data StateVar a =
   StateVar (GettableStateVar a) (SettableStateVar a)

instance HasGetter StateVar where
   get (StateVar g _) = get g

instance HasSetter StateVar where
   ($=) (StateVar _ s) a = s $= a

makeStateVar :: IO a -> (a -> IO ()) -> StateVar a
makeStateVar g s = StateVar (makeGettableStateVar g) (makeSettableStateVar s)
