{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.RenderMode
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module related to the current render mode.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.RenderMode (
   RenderMode(..), withRenderMode, renderMode
) where

import Data.StateVar
import Graphics.Rendering.OpenGL.GL.Exception
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

data RenderMode =
     Render
   | Feedback
   | Select
   deriving ( Eq, Ord, Show )

marshalRenderMode :: RenderMode -> GLenum
marshalRenderMode x = case x of
   Render -> gl_RENDER
   Feedback -> gl_FEEDBACK
   Select -> gl_SELECT

unmarshalRenderMode :: GLenum -> RenderMode
unmarshalRenderMode x
   | x == gl_RENDER = Render
   | x == gl_FEEDBACK = Feedback
   | x == gl_SELECT = Select
   | otherwise = error ("unmarshalRenderMode: illegal value " ++ show x)

--------------------------------------------------------------------------------

withRenderMode :: RenderMode -> IO a -> IO (a, GLint)
withRenderMode newMode action = do
   oldMode <- get renderMode
   _ <- setRenderMode newMode
   action `finallyRet` setRenderMode oldMode

setRenderMode :: RenderMode -> IO GLint
setRenderMode = glRenderMode . marshalRenderMode

--------------------------------------------------------------------------------

renderMode :: GettableStateVar RenderMode
renderMode = makeGettableStateVar $ getEnum1 unmarshalRenderMode GetRenderMode
