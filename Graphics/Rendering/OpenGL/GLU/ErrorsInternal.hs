-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.ErrorsInternal
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module corresponding to some parts of section 2.5
-- (GL Errors) of the OpenGL 1.4 specs and chapter 8 (Errors) of the GLU specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GLU.ErrorsInternal (
   Error(..), ErrorCategory(..), makeError, isError
) where

import Foreign.Ptr ( Ptr, castPtr )
import Foreign.C.String ( peekCString )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLubyte )

--------------------------------------------------------------------------------
-- Alas, GL and GLU define error enumerants with the same names, so we have to
-- rename these to avoid name clashes. Ugly, ugly, ugly...

data GL_ErrorCode =
     GL_NoError
   | GL_InvalidEnum
   | GL_InvalidValue
   | GL_InvalidOperation
   | GL_StackOverflow
   | GL_StackUnderflow
   | GL_OutOfMemory
   | GL_TableTooLarge

gl_marshalErrorCode :: GL_ErrorCode -> GLenum
gl_marshalErrorCode x = case x of
   GL_NoError -> 0x0
   GL_InvalidEnum -> 0x500
   GL_InvalidValue -> 0x501
   GL_InvalidOperation -> 0x502
   GL_StackOverflow -> 0x503
   GL_StackUnderflow -> 0x504
   GL_OutOfMemory -> 0x505
   GL_TableTooLarge -> 0x8031

--------------------------------------------------------------------------------
-- See comment above

data GLU_ErrorCode =
     GLU_InvalidEnum
   | GLU_InvalidValue
   | GLU_OutOfMemory
   | GLU_InvalidOperation

glu_marshalErrorCode :: GLU_ErrorCode -> GLenum
glu_marshalErrorCode x = case x of
   GLU_InvalidEnum -> 0x18a24
   GLU_InvalidValue -> 0x18a25
   GLU_OutOfMemory -> 0x18a26
   GLU_InvalidOperation -> 0x18a28

--------------------------------------------------------------------------------

-- only the errors with the smallest and the largsest enum value
data NurbsError =
     NurbsError1
   | NurbsError37

marshalNurbsError :: NurbsError -> GLenum
marshalNurbsError x = case x of
   NurbsError1 -> 0x1879b
   NurbsError37 -> 0x187bf

--------------------------------------------------------------------------------

-- only the errors with the smallest and the largsest enum value
data TessError =
     TessError1
   | TessError8

marshalTessError :: TessError -> GLenum
marshalTessError x = case x of
   TessError1 -> 0x18737
   TessError8 -> 0x1873e

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
   ptr <- gluErrorString e
   description <- peekCString (castPtr ptr)
   return $ Error category description

foreign import CALLCONV unsafe "gluErrorString" gluErrorString ::
   GLenum -> IO (Ptr GLubyte)

--------------------------------------------------------------------------------

isError :: GLenum -> Bool
isError = (/= gl_marshalErrorCode GL_NoError)
