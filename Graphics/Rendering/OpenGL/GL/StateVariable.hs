--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.StateVariable
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.StateVariable (
   HasGetter(..),
   GettableStateVariable, makeGettableStateVariable,
   Setter, HasSetter(..), set,
   SettableStateVariable, makeSettableStateVariable,
   StateVariable, makeStateVariable
) where


--------------------------------------------------------------------------------

class HasGetter g where
   get :: g a -> IO a

--------------------------------------------------------------------------------

newtype GettableStateVariable a = GettableStateVariable (IO a)

instance HasGetter GettableStateVariable where
   get (GettableStateVariable g) = g

makeGettableStateVariable :: IO a -> GettableStateVariable a
makeGettableStateVariable = GettableStateVariable

--------------------------------------------------------------------------------

newtype Setter = Setter { execute :: IO () }

class HasSetter s where
   (=:) :: s a -> a -> Setter

set :: [Setter] -> IO ()
set = mapM_ execute

--------------------------------------------------------------------------------

newtype SettableStateVariable a = SettableStateVariable (a -> IO ())

instance HasSetter SettableStateVariable where
   (=:) (SettableStateVariable s) a = Setter (s a)

makeSettableStateVariable :: (a -> IO ()) -> SettableStateVariable a
makeSettableStateVariable = SettableStateVariable

--------------------------------------------------------------------------------

data StateVariable a =
   StateVariable (GettableStateVariable a) (SettableStateVariable a)

instance HasGetter StateVariable where
   get (StateVariable g _) = get g

instance HasSetter StateVariable where
   (=:) (StateVariable _ s) a = s =: a

makeStateVariable :: IO a -> (a -> IO ()) -> StateVariable a
makeStateVariable g s =
   StateVariable (makeGettableStateVariable g)
                 (makeSettableStateVariable s)
