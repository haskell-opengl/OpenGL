/* -----------------------------------------------------------------------------
 *
 * Module      :  GL extension support for Graphics.Rendering.OpenGL
 * Copyright   :  (c) Sven Panne 2003
 * License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
 * 
 * Maintainer  :  sven_panne@yahoo.com
 * Stability   :  experimental
 * Portability :  portable
 *
 * This header should only define preprocessor macros!
 *
 * -------------------------------------------------------------------------- */

#ifndef HSOPENGLEXT_H
#define HSOPENGLEXT_H

#define EXTENSION_ENTRY(ext,entry,dynName,ptrName,ty) \
foreign import CALLCONV unsafe "dynamic" dynName :: Graphics.Rendering.OpenGL.GL.Extensions.Invoker (ty) ; \
ptrName :: FunPtr a ; \
ptrName = unsafePerformIO (Graphics.Rendering.OpenGL.GL.Extensions.getProcAddress (ext) (entry)) ; \
{-# NOINLINE ptrName #-}

#endif
