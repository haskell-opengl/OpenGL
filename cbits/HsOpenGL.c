/* -----------------------------------------------------------------------------
 *
 * Module      :  C support for Graphics.Rendering.OpenGL.GL.Extensions
 * Copyright   :  (c) Sven Panne 2002-2005
 * License     :  BSD-style (see the file libraries/GLUT/LICENSE)
 * 
 * Maintainer  :  sven.panne@aedion.de
 * Stability   :  experimental
 * Portability :  portable
 *
 * -------------------------------------------------------------------------- */

#include "HsOpenGL.h"
#include <string.h>

#if defined(_WIN32)
#define hs_OpenGL_gpa(x) wglGetProcAddress((LPCSTR)(x))
#elif defined(USE_QUARTZ_OPENGL)
#include <mach-o/dyld.h>
#include <stdlib.h>
#include <stdio.h>

static void*
hs_OpenGL_gpa(const char *name)
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

/* ToDo: This should really be based on a feature test. */
#elif defined(__sgi) || defined (__sun)
#include <dlfcn.h>

static const char* gpaNames[] = {
  "glXGetProcAddress", "glXGetProcAddressARB", "glXGetProcAddressEXT",
  "_glXGetProcAddress", "_glXGetProcAddressARB", "_glXGetProcAddressEXT"
};

static void*
hs_OpenGL_gpa(const char *name)
{
  static int firstTime = 1;
  static void *handle = NULL;
  static void *gpa = NULL;

  if (firstTime) {
    firstTime = 0;

    /* Get a handle for our executable. */
    handle = dlopen(NULL, RTLD_LAZY);
    /* If fail this early, there's not much we can do about it. */
    if (!handle) {
      return NULL;
    }

    {
      /* Let's see if our platform supports a glXGetProcAddress() variant. */
      int numNames = (int)(sizeof(gpaNames) / sizeof(gpaNames[0]));
      int i;
      for (i = 0;   (!gpa) && (i < numNames);   ++i) {
        gpa = dlsym(handle, gpaNames[i]);
      }
    }
  }

  if (gpa) {
    /* Fine, we seem to have some kind of glXGetProcAddress(), so use it. */
    return ((void *(*)(const GLubyte *))gpa)(name);
  } else if (handle) {
    /* Fallback to dlsym() if we have no glXGetProcAddress(), although we then
       ignore the fact that OpenGL entry points could be context dependent. */
    return dlsym(handle, name);
  } else {
    return NULL;
  }
}

#else
#define hs_OpenGL_gpa(x) glXGetProcAddressARB((const GLubyte*)(x))
#endif


/* procName is really a const char*, but currently we can't specify this in
   Haskell's FFI and consequently get a warning from the C compiler. */
void*
hs_OpenGL_getProcAddress(char *procName)
{
  void* addr = hs_OpenGL_gpa(procName);
  if (addr) {
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
