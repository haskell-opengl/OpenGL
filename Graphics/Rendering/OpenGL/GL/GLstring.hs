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
    peekGLstringLen, withGLStringLen, withGLString,
    stringQuery
) where

import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr

import Graphics.Rendering.OpenGL.Raw.Core31

import Graphics.Rendering.OpenGL.GL.StateVar

-----------------------------------------------------------------------------

type GLStringLen = (Ptr GLchar, GLsizei)

peekGLstringLen :: GLStringLen -> IO String
peekGLstringLen (p,l) = peekCAStringLen (castPtr p, fromIntegral l)

withGLStringLen :: String -> (GLStringLen -> IO a) -> IO a
withGLStringLen s act =
   withCAStringLen s $ \(p,len) ->
      act (castPtr p, fromIntegral len)

withGLString :: String -> (Ptr GLchar -> IO a) -> IO a
withGLString s act = withCAString s $ act . castPtr

stringQuery :: GettableStateVar GLsizei -> (GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) -> GettableStateVar String
stringQuery lengthVar getStr =
   makeGettableStateVar $ do
      len <- get lengthVar -- Note: This includes the NUL character!
      if len == 0
        then return ""
        else allocaArray (fromIntegral len) $ \buf -> do
                getStr len nullPtr buf
                peekGLstringLen (buf, len-1)
