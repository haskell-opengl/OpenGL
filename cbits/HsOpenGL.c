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

#ifdef USE_QUARTZ_OPENGL

#include <mach-o/dyld.h>
#include <stdlib.h>
#include <stdio.h>

static void*
NSGLGetProcAddress(const char *name)
{
  NSSymbol symbol;

  /* Prepend a '_' for the Unix C symbol mangling convention */
  char* symbolName = (char*)malloc(strlen(name) + 2);
  if (!symbolName) {
    fprintf(stderr, "Failed to allocate memory for NSGLGetProcAddress\n");
    return NULL;
  }
  symbolName[0] = '_';
  strcpy(symbolName + 1, name);

  if (!NSIsSymbolNameDefined(symbolName)) {
    free(symbolName);
    return NULL;
  }

  symbol = NSLookupAndBindSymbol(symbolName);
  free(symbolName);
  if (!symbol) {
    return NULL;
  }

  return NSAddressOfSymbol(symbol);
}
#endif


/* procName is really a const char*, but currently we can't specify this in
   Haskell's FFI and consequently get a warning from the C compiler. */
void*
hOpenGL_getProcAddress(char *procName)
{
#if defined(_WIN32)
  void* addr = wglGetProcAddress((LPCSTR)procName);
#elif defined(USE_QUARTZ_OPENGL)
  void *addr = NSGLGetProcAddress(procName);
#else
  void* addr = glXGetProcAddressARB((const GLubyte*)procName);
#endif
  if (addr != NULL) {
    return addr;
  }
  /* There is no way to get GLU extensions at runtime so we fake this, being
     careful about GLU headers on WinDoze, which have a bad habit of lying
     about their actual version. */
#if defined(GLU_VERSION_1_3) && !defined(_WIN32)
  if (strcmp(procName, "gluUnProject4") == 0) {
    return gluUnProject4;
  }
  if (strcmp(procName, "gluBuild3DMipmapLevels") == 0) {
    return gluBuild3DMipmapLevels;
  }
#endif
  return NULL;
}
