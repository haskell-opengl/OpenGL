-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Extensions
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for handling the OpenGL extension
-- mechanism.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO,
   Invoker, getProcAddress   -- used only internally
) where

import Foreign ( unsafePerformIO )
import Foreign.C.String ( CString, withCString )
import Foreign.Ptr ( FunPtr, nullFunPtr )

#ifdef __HUGS__
{-# CBITS HsOpenGL.c #-}
#endif

--------------------------------------------------------------------------------

type Invoker a = FunPtr a -> a

getProcAddress :: String -> String -> IO (FunPtr a)
getProcAddress ext call =
   throwIfNull ("unknown OpenGL call " ++ call ++ ", check for " ++ ext) $
      withCString call hOpenGL_getProcAddress

throwIfNull :: String -> IO (FunPtr a) -> IO (FunPtr a)
throwIfNull msg act = do
   res <- act
   if res == nullFunPtr
      then ioError (userError msg)
      else return res

foreign import CALLCONV unsafe "hOpenGL_getProcAddress" hOpenGL_getProcAddress
   :: CString -> IO (FunPtr a)
