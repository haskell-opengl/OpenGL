-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.RenderMode
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module related to the current render mode.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.RenderMode (
   RenderMode(..), withRenderMode, renderMode
) where

import Data.IORef ( newIORef, readIORef, writeIORef )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLint )
import Graphics.Rendering.OpenGL.GL.Exception ( finally )
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
   c <- newIORef 0
   val <- action `finally` (setRenderMode oldMode >>= writeIORef c)
   count <- readIORef c
   return (val, count)

setRenderMode :: RenderMode -> IO GLint
setRenderMode = glRenderMode . marshalRenderMode

foreign import CALLCONV unsafe "glRenderMode" glRenderMode :: GLenum -> IO GLint

--------------------------------------------------------------------------------

renderMode :: GettableStateVar RenderMode
renderMode = makeGettableStateVar $ getEnum1 unmarshalRenderMode GetRenderMode
