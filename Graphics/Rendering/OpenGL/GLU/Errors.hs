--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.Errors
-- Copyright   :  (c) Sven Panne 2002
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module corresponds to section 2.5 (GL Errors) of the OpenGL 1.4 specs
-- and chapter 8 (Errors) of the GLU specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GLU.Errors (
   Error(..), ErrorCategory(..), getError,
   makeError, outOfMemoryError   -- used only internally
) where

import Foreign.C.String ( CString, peekCString )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )
import qualified Graphics.Rendering.OpenGL.GLU.Constants as C (
   ErrorCode(..), marshalErrorCode,
   NurbsError(..), marshalNurbsError,
   TessError(..), marshalTessError )

--------------------------------------------------------------------------------

data Error = Error ErrorCategory String
   deriving ( Eq, Ord )

--------------------------------------------------------------------------------

data ErrorCategory
   = NoError
   | InvalidEnum
   | InvalidValue
   | InvalidOperation
   | StackOverflow
   | StackUnderflow
   | OutOfMemory
   | TableTooLarge
   | TextureTooLarge
   | TesselatorError
   | NURBSError
   deriving ( Eq, Ord )

unmarshalErrorCategory :: GLenum -> ErrorCategory
unmarshalErrorCategory c
   | c == gl_NO_ERROR          = NoError
   | isInvalidEnum c           = InvalidEnum
   | isInvalidValue c          = InvalidValue
   | isOutOfMemory c           = OutOfMemory
   | isInvalidOperation c      = InvalidOperation
   | c == gl_STACK_OVERFLOW    = StackOverflow
   | c == gl_STACK_UNDERFLOW   = StackUnderflow
   | c == gl_TABLE_TOO_LARGE   = TableTooLarge
   | c == gl_TEXTURE_TOO_LARGE = TextureTooLarge
   | isTesselatorError c       = TesselatorError
   | isNURBSError c            = NURBSError
   | otherwise = error "unmarshalErrorCategory"

isInvalidEnum :: GLenum -> Bool
isInvalidEnum c =
   c == gl_INVALID_ENUM ||
   c == C.marshalErrorCode C.InvalidEnum

isInvalidValue :: GLenum -> Bool
isInvalidValue c =
   c == gl_INVALID_VALUE ||
   c == C.marshalErrorCode C.InvalidValue

isOutOfMemory :: GLenum -> Bool
isOutOfMemory c =
   c == gl_OUT_OF_MEMORY ||
   c == C.marshalErrorCode C.OutOfMemory

isInvalidOperation :: GLenum -> Bool
isInvalidOperation c =
   c == gl_INVALID_OPERATION ||
   c == C.marshalErrorCode C.InvalidOperation

isTesselatorError :: GLenum -> Bool
isTesselatorError c =
   C.marshalTessError C.TessError1 <= c &&
   c <= C.marshalTessError C.TessError8

isNURBSError :: GLenum -> Bool
isNURBSError c =
   C.marshalNurbsError C.NurbsError1 <= c &&
   c <= C.marshalNurbsError C.NurbsError37

--------------------------------------------------------------------------------

-- The returned error string is statically allocated, so peekCString
-- does the right thing here. No malloc/free necessary here.

makeError :: GLenum -> IO Error
makeError e = do
   let category = unmarshalErrorCategory e
   description <- gluErrorString e >>= peekCString
   return $ Error category description

foreign import CALLCONV unsafe "gluErrorString" gluErrorString ::
   GLenum -> IO CString

outOfMemoryError :: Error
outOfMemoryError = Error OutOfMemory "out of memory"

--------------------------------------------------------------------------------

getError :: IO Error
getError = glGetError >>= makeError

foreign import CALLCONV unsafe "glGetError" glGetError :: IO GLenum

--------------------------------------------------------------------------------

-- TODO: UGLY, UGLY, UGLY, remove this somehow...

gl_NO_ERROR, gl_INVALID_ENUM, gl_INVALID_VALUE, gl_INVALID_OPERATION,
 gl_STACK_OVERFLOW, gl_STACK_UNDERFLOW, gl_OUT_OF_MEMORY, gl_TABLE_TOO_LARGE,
 gl_TEXTURE_TOO_LARGE :: GLenum
gl_NO_ERROR           = 0
gl_INVALID_ENUM       = 0x0500
gl_INVALID_VALUE      = 0x0501
gl_INVALID_OPERATION  = 0x0502
gl_STACK_OVERFLOW     = 0x0503
gl_STACK_UNDERFLOW    = 0x0504
gl_OUT_OF_MEMORY      = 0x0505
gl_TABLE_TOO_LARGE    = 0x8031
gl_TEXTURE_TOO_LARGE  = 0x8065
