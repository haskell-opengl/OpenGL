-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.RenderMode
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module related to the current render mode.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.RenderMode (
   RenderMode(..), withRenderMode, renderMode
) where

import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.Exception
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (
   glRenderMode, gl_FEEDBACK, gl_RENDER, gl_SELECT )
import Graphics.Rendering.OpenGL.Raw.Core31

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
