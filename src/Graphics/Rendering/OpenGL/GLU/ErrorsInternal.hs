{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.ErrorsInternal
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
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

import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL.Raw
import System.IO.Unsafe ( unsafePerformIO )

--------------------------------------------------------------------------------

-- | GL\/GLU errors consist of a general error category and a description of
-- what went wrong.

data Error = Error ErrorCategory String
   deriving ( Eq, Ord, Show )

-- | General GL\/GLU error categories

data ErrorCategory
   = ContextLost
   | InvalidEnum
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

makeError :: GLenum -> Error
makeError c
   -- GL errors
   | c == gl_CONTEXT_LOST =
       Error ContextLost "context lost"
   | c == gl_INVALID_ENUM =
       Error InvalidEnum "invalid enumerant"
   | c == gl_INVALID_VALUE =
       Error InvalidValue  "invalid value"
   | c == gl_INVALID_OPERATION =
       Error InvalidOperation "invalid operation"
   | c == gl_INVALID_FRAMEBUFFER_OPERATION =
       Error InvalidFramebufferOperation "invalid framebuffer operation"
   | c == gl_OUT_OF_MEMORY
       = Error OutOfMemory "out of memory"
   | c == gl_STACK_OVERFLOW =
       Error StackOverflow "stack overflow"
   | c == gl_STACK_UNDERFLOW =
       Error StackUnderflow "stack underflow"
   | c == gl_TABLE_TOO_LARGE =
       Error TableTooLarge "table too large"
   -- GLU errors
   | c == glu_INVALID_ENUM =
       Error InvalidEnum "invalid enumerant"
   | c == glu_INVALID_VALUE =
       Error InvalidValue  "invalid value"
   | c == glu_INVALID_OPERATION =
       Error InvalidOperation "invalid operation"
   | c == glu_OUT_OF_MEMORY
       = Error OutOfMemory "out of memory"
   -- GLU tesselator error
   | c == glu_TESS_ERROR1 =
       Error TesselatorError "gluTessBeginPolygon() must precede a gluTessEndPolygon()"
   | c == glu_TESS_ERROR2 =
       Error TesselatorError "gluTessBeginContour() must precede a gluTessEndContour()"
   | c == glu_TESS_ERROR3 =
       Error TesselatorError "gluTessEndPolygon() must follow a gluTessBeginPolygon()"
   | c == glu_TESS_ERROR4 =
       Error TesselatorError "gluTessEndContour() must follow a gluTessBeginContour()"
   | c == glu_TESS_ERROR5 =
       Error TesselatorError "a coordinate is too large"
   | c == glu_TESS_ERROR6 =
       Error TesselatorError "need combine callback"
   | c == glu_TESS_ERROR7 =
       Error TesselatorError "tesselation error 7"
   | c == glu_TESS_ERROR8 =
       Error TesselatorError "tesselation error 8"
   -- GLU NUBRS errors
   | c == glu_NURBS_ERROR1 =
       Error NURBSError "spline order un-supported"
   | c == glu_NURBS_ERROR2 =
       Error NURBSError "too few knots"
   | c == glu_NURBS_ERROR3 =
       Error NURBSError "valid knot range is empty"
   | c == glu_NURBS_ERROR4 =
       Error NURBSError "decreasing knot sequence knot"
   | c == glu_NURBS_ERROR5 =
       Error NURBSError "knot multiplicity greater than order of spline"
   | c == glu_NURBS_ERROR6 =
       Error NURBSError "gluEndCurve() must follow gluBeginCurve()"
   | c == glu_NURBS_ERROR7 =
       Error NURBSError "gluBeginCurve() must precede gluEndCurve()"
   | c == glu_NURBS_ERROR8 =
       Error NURBSError "missing or extra geometric data"
   | c == glu_NURBS_ERROR9 =
       Error NURBSError "can't draw piecewise linear trimming curves"
   | c == glu_NURBS_ERROR10 =
       Error NURBSError "missing or extra domain data"
   | c == glu_NURBS_ERROR11 =
       Error NURBSError "missing or extra domain data"
   | c == glu_NURBS_ERROR12 =
       Error NURBSError "gluEndTrim() must precede gluEndSurface()"
   | c == glu_NURBS_ERROR13 =
       Error NURBSError "gluBeginSurface() must precede gluEndSurface()"
   | c == glu_NURBS_ERROR14 =
       Error NURBSError "curve of improper type passed as trim curve"
   | c == glu_NURBS_ERROR15 =
       Error NURBSError "gluBeginSurface() must precede gluBeginTrim()"
   | c == glu_NURBS_ERROR16 =
       Error NURBSError "gluEndTrim() must follow gluBeginTrim()"
   | c == glu_NURBS_ERROR17 =
       Error NURBSError "gluBeginTrim() must precede gluEndTrim()"
   | c == glu_NURBS_ERROR18 =
       Error NURBSError "invalid or missing trim curve"
   | c == glu_NURBS_ERROR19 =
       Error NURBSError "gluBeginTrim() must precede gluPwlCurve()"
   | c == glu_NURBS_ERROR20 =
       Error NURBSError "piecewise linear trimming curve referenced twice"
   | c == glu_NURBS_ERROR21 =
       Error NURBSError "piecewise linear trimming curve and nurbs curve mixed"
   | c == glu_NURBS_ERROR22 =
       Error NURBSError "improper usage of trim data type"
   | c == glu_NURBS_ERROR23 =
       Error NURBSError "nurbs curve referenced twice"
   | c == glu_NURBS_ERROR24 =
       Error NURBSError "nurbs curve and piecewise linear trimming curve mixed"
   | c == glu_NURBS_ERROR25 =
       Error NURBSError "nurbs surface referenced twice"
   | c == glu_NURBS_ERROR26 =
       Error NURBSError "invalid property"
   | c == glu_NURBS_ERROR27 =
       Error NURBSError "gluEndSurface() must follow gluBeginSurface()"
   | c == glu_NURBS_ERROR28 =
       Error NURBSError "intersecting or misoriented trim curves"
   | c == glu_NURBS_ERROR29 =
       Error NURBSError "intersecting trim curves"
   | c == glu_NURBS_ERROR30 =
       Error NURBSError "UNUSED"
   | c == glu_NURBS_ERROR31 =
       Error NURBSError "unconnected trim curves"
   | c == glu_NURBS_ERROR32 =
       Error NURBSError "unknown knot error"
   | c == glu_NURBS_ERROR33 =
       Error NURBSError "negative vertex count encountered"
   | c == glu_NURBS_ERROR34 =
       Error NURBSError "negative byte-stride encounteed"
   | c == glu_NURBS_ERROR35 =
       Error NURBSError "unknown type descriptor"
   | c == glu_NURBS_ERROR36 =
       Error NURBSError "null control point reference"
   | c == glu_NURBS_ERROR37 =
       Error NURBSError "duplicate point on piecewise linear trimming curve"
   -- Something went terribly wrong...
   | otherwise = error "makeError"

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
getErrors = map makeError `fmap` getErrorCodesAux (const ([], True))

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
