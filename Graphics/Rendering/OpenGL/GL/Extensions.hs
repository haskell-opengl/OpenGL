-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Extensions
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
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

import Foreign.C.String ( CString, withCString )
import Foreign.Ptr ( FunPtr, nullFunPtr )
import System.IO.Unsafe ( unsafePerformIO )

#ifdef __HUGS__
{-# CFILES cbits/HsOpenGL.c #-}
#endif

--------------------------------------------------------------------------------

type Invoker a = FunPtr a -> a

getProcAddress :: String -> String -> IO (FunPtr a)
getProcAddress ext call =
   throwIfNull ("unknown OpenGL call " ++ call ++ ", check for " ++ ext) $
      withCString call hs_OpenGL_getProcAddress

throwIfNull :: String -> IO (FunPtr a) -> IO (FunPtr a)
throwIfNull msg act = do
   res <- act
   if res == nullFunPtr
      then ioError (userError msg)
      else return res

foreign import CALLCONV unsafe "hs_OpenGL_getProcAddress" hs_OpenGL_getProcAddress
   :: CString -> IO (FunPtr a)
