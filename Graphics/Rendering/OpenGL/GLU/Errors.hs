--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.Errors
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 2.5 (GL Errors) of the OpenGL 1.4 specs
-- and chapter 8 (Errors) of the GLU specs, offering a generalized view of
-- errors in GL and GLU.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GLU.Errors (
   Error(..), ErrorCategory(..), errors
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar )
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal (
   Error(..), ErrorCategory(..), makeError, isError )

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
            if isError errorCode
               then getErrors (errorCode:acc)
               else mapM makeError (reverse acc)

foreign import CALLCONV unsafe "glGetError" glGetError :: IO GLenum
