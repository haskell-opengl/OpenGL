{-# OPTIONS_GHC -fno-cse #-}

-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.ErrorsInternal
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module corresponding to some parts of section 2.5
-- (GL Errors) of the OpenGL 2.1 specs and chapter 8 (Errors) of the GLU specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GLU.ErrorsInternal (
   Error(..), ErrorCategory(..), getErrors,
   recordErrorCode, recordInvalidEnum, recordInvalidValue, recordOutOfMemory
) where

import Foreign.Ptr ( castPtr )
import Foreign.C.String ( peekCString )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import System.IO.Unsafe ( unsafePerformIO )
import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility
import Graphics.Rendering.OpenGL.Raw.Core31

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
   | InvalidFramebufferOperation
   | OutOfMemory
   | StackOverflow
   | StackUnderflow
   | TableTooLarge
   | TesselatorError
   | NURBSError
   deriving ( Eq, Ord, Show )

unmarshalErrorCategory :: GLenum -> ErrorCategory
unmarshalErrorCategory c
   | isInvalidEnum c      = InvalidEnum
   | isInvalidValue c     = InvalidValue
   | isInvalidOperation c = InvalidOperation
   | isInvalidFramebufferOperation c = InvalidFramebufferOperation
   | isOutOfMemory c      = OutOfMemory
   | isStackOverflow c    = StackOverflow
   | isStackUnderflow c   = StackUnderflow
   | isTableTooLarge c    = TableTooLarge
   | isTesselatorError c  = TesselatorError
   | isNURBSError c       = NURBSError
   | otherwise = error "unmarshalErrorCategory"

isInvalidEnum :: GLenum -> Bool
isInvalidEnum c = c == gl_INVALID_ENUM || c == glu_INVALID_ENUM

isInvalidValue :: GLenum -> Bool
isInvalidValue c = c == gl_INVALID_VALUE || c == glu_INVALID_VALUE

isInvalidOperation :: GLenum -> Bool
isInvalidOperation c = c == gl_INVALID_OPERATION || c == glu_INVALID_OPERATION

isInvalidFramebufferOperation :: GLenum -> Bool
isInvalidFramebufferOperation c = c == gl_INVALID_FRAMEBUFFER_OPERATION

isOutOfMemory :: GLenum -> Bool
isOutOfMemory c = c == gl_OUT_OF_MEMORY || c == glu_OUT_OF_MEMORY

isStackOverflow :: GLenum -> Bool
isStackOverflow c = c == gl_STACK_OVERFLOW

isStackUnderflow :: GLenum -> Bool
isStackUnderflow c = c == gl_STACK_UNDERFLOW

isTableTooLarge :: GLenum -> Bool
isTableTooLarge c = c == gl_TABLE_TOO_LARGE

isTesselatorError :: GLenum -> Bool
isTesselatorError c = glu_TESS_ERROR1 <= c && c <= glu_TESS_ERROR8

isNURBSError :: GLenum -> Bool
isNURBSError c = glu_NURBS_ERROR1 <= c && c <= glu_NURBS_ERROR37

--------------------------------------------------------------------------------

-- The returned error string is statically allocated, so peekCString
-- does the right thing here. No malloc/free necessary here.

makeError :: GLenum -> IO Error
makeError e = do
   let category = unmarshalErrorCategory e
   ptr <- gluErrorString e
   description <- peekCString (castPtr ptr)
   return $ Error category description

--------------------------------------------------------------------------------

-- This seems to be a common Haskell hack nowadays: A plain old global variable
-- with an associated getter and mutator. Perhaps some language/library support
-- is needed?

{-# NOINLINE theRecordedErrors #-}
theRecordedErrors :: IORef ([GLenum],Bool)
theRecordedErrors = unsafePerformIO (newIORef ([], True))

getRecordedErrors :: IO ([GLenum],Bool)
getRecordedErrors =  readIORef theRecordedErrors

setRecordedErrors :: ([GLenum],Bool) -> IO ()
setRecordedErrors = writeIORef theRecordedErrors

--------------------------------------------------------------------------------

getGLErrors :: IO [GLenum]
getGLErrors = getGLErrorsAux []
   where getGLErrorsAux acc = do
            errorCode <- glGetError
            if isError errorCode
               then getGLErrorsAux (errorCode : acc)
               else return $ reverse acc

isError :: GLenum -> Bool
isError = (/= gl_NO_ERROR)

--------------------------------------------------------------------------------

getErrors :: IO [Error]
getErrors = do
   es <- getErrorCodesAux (const ([], True))
   mapM makeError es

recordErrorCode :: GLenum -> IO ()
recordErrorCode e = do
   -- We don't need the return value because this calls setRecordedErrors
   _ <- getErrorCodesAux (\es -> (if null es then [e] else [], False))
   return ()

recordInvalidEnum :: IO ()
recordInvalidEnum = recordErrorCode gl_INVALID_ENUM

recordInvalidValue :: IO ()
recordInvalidValue = recordErrorCode gl_INVALID_VALUE

recordOutOfMemory :: IO ()
recordOutOfMemory = recordErrorCode gl_OUT_OF_MEMORY

-- ToDo: Make this thread-safe
getErrorCodesAux :: ([GLenum] -> ([GLenum],Bool)) -> IO [GLenum]
getErrorCodesAux f = do
   (recordedErrors, useGLErrors) <- getRecordedErrors
   glErrors <- getGLErrors
   let es = if useGLErrors then recordedErrors ++ glErrors else recordedErrors
   setRecordedErrors (f es)
   return es
