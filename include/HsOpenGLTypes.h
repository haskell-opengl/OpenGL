/* -----------------------------------------------------------------------------
 *
 * Module      :  Macros for basic GL types to get better Haddock documentation
 * Copyright   :  (c) Sven Panne 2003
 * License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
 * 
 * Maintainer  :  sven_panne@yahoo.com
 * Stability   :  provisional
 * Portability :  portable
 *
 * This header should only define preprocessor macros!
 *
 * -------------------------------------------------------------------------- */

#ifndef HSOPENGLTYPES_H
#define HSOPENGLTYPES_H

#include "MachDeps.h"

/* Using a type synonym in an instance head is not Haskell98, but it is much
   better for a useful documentation. */
#ifdef __HADDOCK__
#define GLbyte_   GLbyte
#define GLdouble_ GLdouble
#define GLfloat_  GLfloat
#define GLint_    GLint
#define GLshort_  GLshort
#define GLubyte_  GLubyte
#define GLuint_   GLuint
#define GLushort_ GLushort
#else
#define GLbyte_   HTYPE_GLBYTE
#define GLdouble_ HTYPE_GLDOUBLE
#define GLfloat_  HTYPE_GLFLOAT
#define GLint_    HTYPE_GLINT
#define GLshort_  HTYPE_GLSHORT
#define GLubyte_  HTYPE_GLUBYTE
#define GLuint_   HTYPE_GLUINT
#define GLushort_ HTYPE_GLUSHORT
#endif

#endif
