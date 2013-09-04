{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.GLstring
-- Copyright   :  (c) Sven Panne 2006-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling GL strings.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.GLstring (
    GLstringLen, peekGLstringLen, withGLstringLen, withGLstring
) where

import Foreign.C.String
import Foreign.Ptr
import Graphics.Rendering.OpenGL.Raw.Core31

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
