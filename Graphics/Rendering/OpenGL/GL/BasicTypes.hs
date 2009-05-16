--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.BasicTypes
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 2.3 (GL Command Sytax) of the OpenGL 2.1
-- specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLboolean, GLbyte, GLubyte, GLchar, GLshort, GLushort, GLint, GLuint,
   GLsizei, GLenum, GLintptr, GLsizeiptr, GLbitfield, GLfloat, GLclampf,
   GLdouble, GLclampd, Capability(..)
) where

--------------------------------------------------------------------------------

#include "HsOpenGLConfig.h"

--------------------------------------------------------------------------------

import Data.Int
import Data.Word

-- If we don't find modern OpenGL headers at configuration time, make some
-- educated guesses here.

#if !defined(HTYPE_GLCHAR) || !defined(HTYPE_GLINTPTR) || !defined(HTYPE_GLSIZEIPTR)
import Foreign.C.Types
#endif

#if !defined(HTYPE_GLCHAR)
#define HTYPE_GLCHAR CChar
#endif

#if !defined(HTYPE_GLINTPTR)
#define HTYPE_GLINTPTR CPtrdiff
#endif

#if !defined(HTYPE_GLSIZEIPTR)
#define HTYPE_GLSIZEIPTR CPtrdiff
#endif

--------------------------------------------------------------------------------

-- | Boolean (min. 1 bit)
type GLboolean = HTYPE_GLBOOLEAN

-- | Signed 2\'s complement binary integer (min. 8 bits)
type GLbyte = HTYPE_GLBYTE

-- | Unsigned binary integer (min. 8 bits)
type GLubyte = HTYPE_GLUBYTE

-- | Characters making up strings
type GLchar = HTYPE_GLCHAR

-- | Signed 2\'s complement binary integer (min. 16 bits)
type GLshort = HTYPE_GLSHORT

-- | Unsigned binary integer (min. 16 bits)
type GLushort = HTYPE_GLUSHORT

-- | Signed 2\'s complement binary integer (min. 32 bits)
type GLint = HTYPE_GLINT

-- | Unsigned binary integer (min. 32 bits)
type GLuint = HTYPE_GLUINT

-- | Non-negatitve binary integer size (min. 32 bits)
type GLsizei = HTYPE_GLSIZEI

-- | Enumerated binary integer value (min. 32 bits)
type GLenum = HTYPE_GLENUM

-- | Signed 2\'s complement binary integer (sufficiently large enough to hold
-- any address)
type GLintptr = HTYPE_GLINTPTR

-- | Non-negatitve binary integer size (sufficiently large enough to hold any
-- address)
type GLsizeiptr = HTYPE_GLSIZEIPTR

-- | Bit field (min. 32 bits)
type GLbitfield = HTYPE_GLBITFIELD

-- | Floating-point value (min. 32 bits)
type GLfloat = HTYPE_GLFLOAT

-- | Floating-point value clamped to [0,1] (min. 32 bits)
type GLclampf = HTYPE_GLCLAMPF

-- | Floating-point value (min. 64 bits)
type GLdouble = HTYPE_GLDOUBLE

-- | Floating-point value clamped to [0,1] (min. 64 bits)
type GLclampd = HTYPE_GLCLAMPD

--------------------------------------------------------------------------------

data Capability =
     Disabled
   | Enabled
   deriving ( Eq, Ord, Show )
