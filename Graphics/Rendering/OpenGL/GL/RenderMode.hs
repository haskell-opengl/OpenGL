-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.RenderMode
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module related to the current render mode.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.RenderMode (
   RenderMode(..), withRenderMode, renderMode
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLint )
import Graphics.Rendering.OpenGL.GL.Exception ( finallyRet )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetRenderMode), getEnum1 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasGetter(get), GettableStateVar, makeGettableStateVar )

--------------------------------------------------------------------------------

data RenderMode =
     Render
   | Feedback
   | Select
   deriving ( Eq, Ord, Show )

marshalRenderMode :: RenderMode -> GLenum
marshalRenderMode x = case x of
   Render -> 0x1c00
   Feedback -> 0x1c01
   Select -> 0x1c02

unmarshalRenderMode :: GLenum -> RenderMode
unmarshalRenderMode x
   | x == 0x1c00 = Render
   | x == 0x1c01 = Feedback
   | x == 0x1c02 = Select
   | otherwise = error ("unmarshalRenderMode: illegal value " ++ show x)

--------------------------------------------------------------------------------

withRenderMode :: RenderMode -> IO a -> IO (a, GLint)
withRenderMode newMode action = do
   oldMode <- get renderMode
   setRenderMode newMode
   action `finallyRet` setRenderMode oldMode

setRenderMode :: RenderMode -> IO GLint
setRenderMode = glRenderMode . marshalRenderMode

foreign import CALLCONV unsafe "glRenderMode" glRenderMode :: GLenum -> IO GLint

--------------------------------------------------------------------------------

renderMode :: GettableStateVar RenderMode
renderMode = makeGettableStateVar $ getEnum1 unmarshalRenderMode GetRenderMode
