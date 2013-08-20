--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferTarget
-- Copyright   :  (c) Sven Panne 2013
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for handling RenderbufferTargets.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferTarget (
   RenderbufferTarget(..), marshalRenderbufferTarget, getRBParameteriv,
   Samples(..)
) where

import Foreign.Marshal
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

data RenderbufferTarget = Renderbuffer
   deriving ( Eq, Ord, Show )

marshalRenderbufferTarget :: RenderbufferTarget -> GLenum
marshalRenderbufferTarget x = case x of
    Renderbuffer -> gl_RENDERBUFFER

-----------------------------------------------------------------------------

getRBParameteriv :: RenderbufferTarget -> (GLint -> a) -> GLenum -> IO a
getRBParameteriv rbt f p =
   alloca $ \buf -> do
      glGetRenderbufferParameteriv (marshalRenderbufferTarget rbt) p buf
      peek1 f buf
-----------------------------------------------------------------------------

newtype Samples = Samples GLsizei
   deriving ( Eq, Ord, Show )
