/* -----------------------------------------------------------------------------
 *
 * Module      :  C support for Graphics.Rendering.OpenGL.GL.Extensions
 * Copyright   :  (c) Sven Panne 2003
 * License     :  BSD-style (see the file libraries/GLUT/LICENSE)
 * 
 * Maintainer  :  sven_panne@yahoo.com
 * Stability   :  experimental
 * Portability :  portable
 *
 * -------------------------------------------------------------------------- */

#include "HsOpenGL.h"
#include <string.h>

/* procName is really a const char*, but currently we can't specify this in
   Haskell's FFI and consequently get a warning from the C compiler. */
void*
hOpenGL_getProcAddress(char *procName)
{
#if defined(_WIN32)
  void* addr = wglGetProcAddress((LPCSTR)procName);
#else
  void* addr = glXGetProcAddressARB((const GLubyte*)procName);
#endif
  if (addr != NULL) {
    return addr;
  }
#ifdef GLU_VERSION_1_3
  if (strcmp(procName, "gluUnProject4") == 0) {
    return gluUnProject4;
  }
  if (strcmp(procName, "gluBuild3DMipmapLevels") == 0) {
    return gluBuild3DMipmapLevels;
  }
#endif
  return NULL;
}
