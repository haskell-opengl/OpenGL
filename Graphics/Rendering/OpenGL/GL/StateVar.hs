--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.StateVar
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.StateVar (
   HasGetter(..),
   GettableStateVar, makeGettableStateVar,
   HasSetter(..), set,
   SettableStateVar, makeSettableStateVar,
   StateVar, makeStateVar, makeStateVarMaybe
) where

import Control.Monad ( liftM )

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

--------------------------------------------------------------------------------

makeStateVarMaybe :: StateVar Bool -> IO a -> (a -> IO ()) -> StateVar (Maybe a)
makeStateVarMaybe var getAct setAct =
   makeStateVar (getStateVarMaybe var getAct) (setStateVarMaybe var setAct)

getStateVarMaybe :: StateVar Bool -> IO a -> IO (Maybe a)
getStateVarMaybe var act = do
   enabled <- get var
   if enabled
      then liftM Just act
      else return Nothing

setStateVarMaybe :: StateVar Bool -> (a -> IO ()) -> Maybe a -> IO ()
setStateVarMaybe var act = maybe (var $= False) (\x -> var $= True >> act x)
