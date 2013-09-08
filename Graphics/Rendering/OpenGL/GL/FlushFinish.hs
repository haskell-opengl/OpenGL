--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.FlushFinish
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 2.3.2 (Flush and Finish) of the OpenGL 4.4
-- specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.FlushFinish (
   flush, finish      
) where

import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

-- | Different GL implementations buffer commands in several different
-- locations, including network buffers and the graphics accelerator itself.
-- 'flush' empties all of these buffers, causing all issued commands to be
-- executed as quickly as they are accepted by the actual rendering engine.
-- Though this execution may not be completed in any particular time period, it
-- does complete in finite time.
-- 
-- Because any GL program might be executed over a network, or on an accelerator
-- that buffers commands, all programs should call 'flush' whenever they count
-- on having all of their previously issued commands completed. For example,
-- call 'flush' before waiting for user input that depends on the generated
-- image.
-- 
-- Note that 'flush' can return at any time. It does not wait until the
-- execution of all previously issued GL commands is complete.

flush :: IO ()
flush = glFlush

-- | 'finish' does not return until the effects of all previously called GL
-- commands are complete. Such effects include all changes to GL state, all
-- changes to connection state, and all changes to the frame buffer contents.
-- 
-- Note that 'finish' requires a round trip to the server.

finish :: IO ()
finish = glFinish
