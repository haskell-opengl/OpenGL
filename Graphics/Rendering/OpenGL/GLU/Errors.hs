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
   deriving ( Eq, Ord, Show )

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
   deriving ( Eq, Ord, Show )

glu_marshalErrorCode :: GLU_ErrorCode -> GLenum
glu_marshalErrorCode x = case x of
   GLU_InvalidEnum -> 0x18a24
   GLU_InvalidValue -> 0x18a25
   GLU_OutOfMemory -> 0x18a26
   GLU_InvalidOperation -> 0x18a28

--------------------------------------------------------------------------------

data NurbsError =
     NurbsError1
   | NurbsError2
   | NurbsError3
   | NurbsError4
   | NurbsError5
   | NurbsError6
   | NurbsError7
   | NurbsError8
   | NurbsError9
   | NurbsError10
   | NurbsError11
   | NurbsError12
   | NurbsError13
   | NurbsError14
   | NurbsError15
   | NurbsError16
   | NurbsError17
   | NurbsError18
   | NurbsError19
   | NurbsError20
   | NurbsError21
   | NurbsError22
   | NurbsError23
   | NurbsError24
   | NurbsError25
   | NurbsError26
   | NurbsError27
   | NurbsError28
   | NurbsError29
   | NurbsError30
   | NurbsError31
   | NurbsError32
   | NurbsError33
   | NurbsError34
   | NurbsError35
   | NurbsError36
   | NurbsError37
   deriving ( Eq, Ord, Show )

marshalNurbsError :: NurbsError -> GLenum
marshalNurbsError x = case x of
   NurbsError1 -> 0x1879b
   NurbsError2 -> 0x1879c
   NurbsError3 -> 0x1879d
   NurbsError4 -> 0x1879e
   NurbsError5 -> 0x1879f
   NurbsError6 -> 0x187a0
   NurbsError7 -> 0x187a1
   NurbsError8 -> 0x187a2
   NurbsError9 -> 0x187a3
   NurbsError10 -> 0x187a4
   NurbsError11 -> 0x187a5
   NurbsError12 -> 0x187a6
   NurbsError13 -> 0x187a7
   NurbsError14 -> 0x187a8
   NurbsError15 -> 0x187a9
   NurbsError16 -> 0x187aa
   NurbsError17 -> 0x187ab
   NurbsError18 -> 0x187ac
   NurbsError19 -> 0x187ad
   NurbsError20 -> 0x187ae
   NurbsError21 -> 0x187af
   NurbsError22 -> 0x187b0
   NurbsError23 -> 0x187b1
   NurbsError24 -> 0x187b2
   NurbsError25 -> 0x187b3
   NurbsError26 -> 0x187b4
   NurbsError27 -> 0x187b5
   NurbsError28 -> 0x187b6
   NurbsError29 -> 0x187b7
   NurbsError30 -> 0x187b8
   NurbsError31 -> 0x187b9
   NurbsError32 -> 0x187ba
   NurbsError33 -> 0x187bb
   NurbsError34 -> 0x187bc
   NurbsError35 -> 0x187bd
   NurbsError36 -> 0x187be
   NurbsError37 -> 0x187bf

--------------------------------------------------------------------------------

data TessError =
     TessError1
   | TessError2
   | TessError3
   | TessError4
   | TessError5
   | TessError6
   | TessError7
   | TessError8
   | TessMissingBeginPolygon
   | TessMissingBeginContour
   | TessMissingEndPolygon
   | TessMissingEndContour
   | TessCoordTooLarge
   | TessNeedCombineCallback
   deriving ( Eq, Ord, Show )

marshalTessError :: TessError -> GLenum
marshalTessError x = case x of
   TessError1 -> 0x18737
   TessError2 -> 0x18738
   TessError3 -> 0x18739
   TessError4 -> 0x1873a
   TessError5 -> 0x1873b
   TessError6 -> 0x1873c
   TessError7 -> 0x1873d
   TessError8 -> 0x1873e
   TessMissingBeginPolygon -> 0x18737
   TessMissingBeginContour -> 0x18738
   TessMissingEndPolygon -> 0x18739
   TessMissingEndContour -> 0x1873a
   TessCoordTooLarge -> 0x1873b
   TessNeedCombineCallback -> 0x1873c

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

-- | When an error occurs, it is recorded in this state variable and no further
-- errors are recorded. Reading 'errors' returns the currently recorded errors
-- (there may be more than one due to a possibly distributed implementation) and
-- resets the state variable to @[]@, re-enabling the recording of future
-- errors. The value @[]@ means that there has been no detectable error since
-- the last time 'errors' was read, or since the GL was initialized.

errors :: GettableStateVar [Error]
errors = makeGettableStateVar $ getErrors []
   where getErrors acc = do
            errorCode <- glGetError
            if errorCode == gl_marshalErrorCode GL_NoError
               then mapM makeError (reverse acc)
               else getErrors (errorCode:acc)

foreign import CALLCONV unsafe "glGetError" glGetError :: IO GLenum
