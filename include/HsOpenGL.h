/* -----------------------------------------------------------------------------
 *
 * Module      :  C support for Graphics.Rendering.OpenGL
 * Copyright   :  (c) Sven Panne 2003
 * License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
 * 
 * Maintainer  :  sven_panne@yahoo.com
 * Stability   :  provisional
 * Portability :  portable
 *
 * -------------------------------------------------------------------------- */

#ifndef HSOPENGL_H
#define HSOPENGL_H

#include "config.h"

#ifdef _WIN32

/* for the prototype of wglGetProcAddress */
#include <windows.h>
#include <GL/glu.h>

#elif defined(USE_QUARTZ_OPENGL)

#include <OpenGL/glu.h>

#else

/* Hmmm, this seems to be necessary to get a prototype for glXGetProcAddressARB
   when Mesa is used. Strange... */
#ifndef GLX_GLXEXT_PROTOTYPES
#define GLX_GLXEXT_PROTOTYPES
#endif

#include <GL/glu.h>
#include <GL/glx.h>

#endif

extern void* hOpenGL_getProcAddress(char* procName);

#endif
