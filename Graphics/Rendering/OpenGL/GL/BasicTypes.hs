module Graphics.Rendering.OpenGL.GL.BasicTypes where

import Data.Int
import Data.Word

#include "MachDeps.h"

type GLenum     = HTYPE_GLENUM
type GLboolean  = HTYPE_GLBOOLEAN
type GLbitfield = HTYPE_GLBITFIELD
type GLbyte     = HTYPE_GLBYTE
type GLshort    = HTYPE_GLSHORT
type GLint      = HTYPE_GLINT
type GLubyte    = HTYPE_GLUBYTE
type GLushort   = HTYPE_GLUSHORT
type GLuint     = HTYPE_GLUINT
type GLsizei    = HTYPE_GLSIZEI
type GLfloat    = HTYPE_GLFLOAT
type GLclampf   = HTYPE_GLCLAMPF
type GLdouble   = HTYPE_GLDOUBLE
type GLclampd   = HTYPE_GLCLAMPD

marshalGLboolean :: Bool -> GLboolean
marshalGLboolean False = 0
marshalGLboolean True  = 1

unmarshalGLboolean :: GLboolean -> Bool
unmarshalGLboolean = (/= 0)
