-- #prune
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.Errors
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module corresponds to section 2.5 (GL Errors) of the OpenGL 1.4 specs
-- and chapter 8 (Errors) of the GLU specs, offering a generalized view of
-- errors in GL and GLU.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GLU.Errors (
   Error(..), ErrorCategory(..), errors,
   makeError   -- used only internally
) where

import Foreign.C.String ( CString, peekCString )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar )

--------------------------------------------------------------------------------
-- Alas, GL and GLU define error enumerants with the same names, so we have to
-- rename these via CPP trickery to avoid name clashes. Ugly, ugly, ugly...

#define InvalidEnum      GL_InvalidEnum
#define InvalidValue     GL_InvalidValue
#define InvalidOperation GL_InvalidOperation
#define StackOverflow    GL_StackOverflow
#define StackUnderflow   GL_StackUnderflow
#define OutOfMemory      GL_OutOfMemory
#define TableTooLarge    GL_TableTooLarge
#define ErrorCode        GL_ErrorCode
#define marshalErrorCode gl_marshalErrorCode

#define HOPENGL_IMPORT_ErrorCode
#define HOPENGL_IMPORT_marshalErrorCode

#include "../GL/Constants.incl"

#undef InvalidEnum
#undef InvalidValue
#undef InvalidOperation
#undef StackOverflow
#undef StackUnderflow
#undef OutOfMemory
#undef TableTooLarge
#undef ErrorCode         
#undef marshalErrorCode

--------------------------------------------------------------------------------
-- See comment above

#define InvalidEnum      GLU_InvalidEnum
#define InvalidValue     GLU_InvalidValue
#define InvalidOperation GLU_InvalidOperation
#define OutOfMemory      GLU_OutOfMemory
#define ErrorCode        GLU_ErrorCode
#define marshalErrorCode glu_marshalErrorCode

#define HOPENGL_IMPORT_ErrorCode
#define HOPENGL_IMPORT_marshalErrorCode

#define HOPENGL_IMPORT_NurbsError
#define HOPENGL_IMPORT_marshalNurbsError

#define HOPENGL_IMPORT_TessError
#define HOPENGL_IMPORT_marshalTessError

#include "Constants.incl"

#undef InvalidEnum     
#undef InvalidValue    
#undef InvalidOperation
#undef OutOfMemory     
#undef ErrorCode         
#undef marshalErrorCode

--------------------------------------------------------------------------------

-- | GL\/GLU errors consist of a general error category and a description of
-- what went wrong.

data Error = Error ErrorCategory String
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

-- | General GL\/GLU error categories

data ErrorCategory
   = InvalidEnum
   | InvalidValue
   | InvalidOperation
   | StackOverflow
   | StackUnderflow
   | OutOfMemory
   | TableTooLarge
   | TesselatorError
   | NURBSError
   deriving ( Eq, Ord, Show )

unmarshalErrorCategory :: GLenum -> ErrorCategory
unmarshalErrorCategory c
   | isInvalidEnum c      = InvalidEnum
   | isInvalidValue c     = InvalidValue
   | isOutOfMemory c      = OutOfMemory
   | isInvalidOperation c = InvalidOperation
   | isStackOverflow c    = StackOverflow
   | isStackUnderflow c   = StackUnderflow
   | isTableTooLarge c    = TableTooLarge
   | isTesselatorError c  = TesselatorError
   | isNURBSError c       = NURBSError
   | otherwise = error "unmarshalErrorCategory"

isInvalidEnum :: GLenum -> Bool
isInvalidEnum c =
   c == gl_marshalErrorCode  GL_InvalidEnum ||
   c == glu_marshalErrorCode GLU_InvalidEnum

isInvalidValue :: GLenum -> Bool
isInvalidValue c =
   c == gl_marshalErrorCode  GL_InvalidValue ||
   c == glu_marshalErrorCode GLU_InvalidValue

isOutOfMemory :: GLenum -> Bool
isOutOfMemory c =
   c == gl_marshalErrorCode  GL_OutOfMemory ||
   c == glu_marshalErrorCode GLU_OutOfMemory

isInvalidOperation :: GLenum -> Bool
isInvalidOperation c =
   c == gl_marshalErrorCode  GL_InvalidOperation ||
   c == glu_marshalErrorCode GLU_InvalidOperation

isStackOverflow :: GLenum -> Bool
isStackOverflow c =
   c == gl_marshalErrorCode GL_StackOverflow

isStackUnderflow :: GLenum -> Bool
isStackUnderflow c =
   c == gl_marshalErrorCode GL_StackUnderflow

isTableTooLarge :: GLenum -> Bool
isTableTooLarge c =
   c == gl_marshalErrorCode GL_TableTooLarge

isTesselatorError :: GLenum -> Bool
isTesselatorError c =
   marshalTessError TessError1 <= c &&
   c <= marshalTessError TessError8

isNURBSError :: GLenum -> Bool
isNURBSError c =
   marshalNurbsError NurbsError1 <= c &&
   c <= marshalNurbsError NurbsError37

--------------------------------------------------------------------------------

-- The returned error string is statically allocated, so peekCString
-- does the right thing here. No malloc/free necessary here.

makeError :: GLenum -> IO Error
makeError e = do
   let category = unmarshalErrorCategory e
   description <- peekCString =<< gluErrorString e
   return $ Error category description

foreign import CALLCONV unsafe "gluErrorString" gluErrorString ::
   GLenum -> IO CString

--------------------------------------------------------------------------------

-- | When an error occurs, it is recorded in this state variable and no other
-- errors are recorded. Reading 'errors' returns the currently recorded errors
-- (there may be more than one due to a possibly distributed implementation) and
-- resets the state variable to @[]@, re-enabling the recording of future
-- errors. The value @[]@ means that there has been no detectable error since
-- the last time 'errors' was read, or since the GL was initialized.

errors :: GettableStateVar [Error]
errors = makeGettableStateVar $ getErrors []
   where getErrors acc = do
            errorCode <- glGetError
            if errorCode == gl_marshalErrorCode NoError
               then mapM makeError (reverse acc)
               else getErrors (errorCode:acc)

foreign import CALLCONV unsafe "glGetError" glGetError :: IO GLenum
