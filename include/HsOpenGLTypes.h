/* -----------------------------------------------------------------------------
 *
 * Module      :  Macros for basic GL types to get better Haddock documentation
 * Copyright   :  (c) Sven Panne 2002-2005
 * License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
 * 
 * Maintainer  :  sven.panne@aedion.de
 * Stability   :  provisional
 * Portability :  portable
 *
 * This header should only define preprocessor macros!
 *
 * -------------------------------------------------------------------------- */

#ifndef HSOPENGLTYPES_H
#define HSOPENGLTYPES_H

#include "HsOpenGLConfig.h"

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
#define GLbyte_   CSChar
#define GLdouble_ CDouble
#define GLfloat_  CFloat
#define GLint_    CInt
#define GLshort_  CShort
#define GLubyte_  CUChar
#define GLuint_   CUInt
#define GLushort_ CUShort
#endif

#endif
