--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.BasicTypes
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 2.3 (GL Command Sytax) of the OpenGL 1.5
-- specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLboolean, GLbitfield, GLbyte, GLshort, GLint, GLintptr, GLubyte,
   GLushort, GLuint, GLsizei, GLsizeiptr, GLfloat, GLclampf, GLdouble, GLclampd,
   Capability(..)
) where

--------------------------------------------------------------------------------

import Data.Int
import Data.Word

--------------------------------------------------------------------------------

#include "HsOpenGLConfig.h"

--------------------------------------------------------------------------------

-- | Enumerated binary integer value (min. 32 bits)
type GLenum = HTYPE_GLENUM

-- | Boolean (min. 1 bit)
type GLboolean = HTYPE_GLBOOLEAN

-- | Bit field (min. 32 bits)
type GLbitfield = HTYPE_GLBITFIELD

-- | Signed 2\'s complement binary integer (min. 8 bits)
type GLbyte = HTYPE_GLBYTE

-- | Signed 2\'s complement binary integer (min. 16 bits)
type GLshort = HTYPE_GLSHORT

-- | Signed 2\'s complement binary integer (min. 32 bits)
type GLint = HTYPE_GLINT

-- | Signed 2\'s complement binary integer (sufficiently large enough to hold
-- any address)
type GLintptr = Int32 -- TODO: Use autoconf stuff for this!

-- | Unsigned binary integer (min. 8 bits)
type GLubyte = HTYPE_GLUBYTE

-- | Unsigned binary integer (min. 16 bits)
type GLushort = HTYPE_GLUSHORT

-- | Unsigned binary integer (min. 32 bits)
type GLuint = HTYPE_GLUINT

-- | Non-negatitve binary integer size (min. 32 bits)
type GLsizei = HTYPE_GLSIZEI

-- | Non-negatitve binary integer size (sufficiently large enough to hold any
-- address)
type GLsizeiptr = Int32 -- TODO: Use autoconf stuff for this!

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
