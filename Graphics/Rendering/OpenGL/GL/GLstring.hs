-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Rendering.OpenGL.GL.GLstring
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <sven.panne@aedion.de>
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.GLstring (
    GLstringLen, peekGLstringLen, withGLstringLen, withGLstring, stringQuery
) where

import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr

import Graphics.Rendering.OpenGL.Raw.Core31

import Graphics.Rendering.OpenGL.GL.StateVar

-----------------------------------------------------------------------------

type GLstringLen = (Ptr GLchar, GLsizei)

peekGLstringLen :: GLstringLen -> IO String
peekGLstringLen (p,l) = peekCAStringLen (castPtr p, fromIntegral l)

withGLstringLen :: String -> (GLstringLen -> IO a) -> IO a
withGLstringLen s act =
   withCAStringLen s $ \(p,len) ->
      act (castPtr p, fromIntegral len)

withGLstring :: String -> (Ptr GLchar -> IO a) -> IO a
withGLstring s act = withCAString s $ act . castPtr

stringQuery :: GettableStateVar GLsizei -> (GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) -> GettableStateVar String
stringQuery lengthVar getStr =
   makeGettableStateVar $ do
      len <- get lengthVar -- Note: This includes the NUL character!
      if len == 0
        then return ""
        else allocaArray (fromIntegral len) $ \buf -> do
                getStr len nullPtr buf
                peekGLstringLen (buf, len-1)
