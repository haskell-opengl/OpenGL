-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <sven.panne@aedion.de>
-- Stability   :
-- Portability :
--
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects (
   FramebufferObject,
   defaultFramebufferObject,
   FramebufferTarget(..), marshalFramebufferTarget,

   bindFramebuffer,

   FramebufferStatus(..), framebufferStatus,
) where

import Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObject
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.Raw.Core31

-----------------------------------------------------------------------------

defaultFramebufferObject :: FramebufferObject
defaultFramebufferObject = FramebufferObject 0

-----------------------------------------------------------------------------

data FramebufferTarget =
     DrawFramebuffer
   | ReadFramebuffer
   | Framebuffer
   deriving ( Eq, Ord, Show )

marshalFramebufferTarget :: FramebufferTarget -> GLenum
marshalFramebufferTarget xs = case xs of
   DrawFramebuffer -> gl_DRAW_FRAMEBUFFER
   ReadFramebuffer -> gl_READ_FRAMEBUFFER
   Framebuffer -> gl_FRAMEBUFFER

marshalFramebufferTargetBinding :: FramebufferTarget -> PName1I
marshalFramebufferTargetBinding x = case x of
   DrawFramebuffer -> GetDrawFramebufferBinding
   ReadFramebuffer -> GetReadFramebufferBinding
   Framebuffer -> GetFramebufferBinding

-----------------------------------------------------------------------------

bindFramebuffer :: FramebufferTarget -> StateVar FramebufferObject
bindFramebuffer fbt =
    makeStateVar (getBoundFramebuffer fbt) (setFramebuffer fbt)

getBoundFramebuffer :: FramebufferTarget -> IO FramebufferObject
getBoundFramebuffer =
   getInteger1 (FramebufferObject . fromIntegral) . marshalFramebufferTargetBinding

setFramebuffer :: FramebufferTarget -> FramebufferObject -> IO ()
setFramebuffer fbt =
   glBindFramebuffer (marshalFramebufferTarget fbt) . framebufferID

-----------------------------------------------------------------------------

data FramebufferStatus =
     Complete
   | Undefined
   | IncompleteMissingAttachment
   | IncompleteDrawBuffer
   | IncompleteReadBuffer
   | IncompleteMultiSample
   | Unsupported
   deriving ( Eq, Ord, Show )

unmarshalFramebufferStatus :: GLenum -> FramebufferStatus
unmarshalFramebufferStatus x
   | x == gl_FRAMEBUFFER_COMPLETE = Complete
   | x == gl_FRAMEBUFFER_UNDEFINED = Undefined
   | x == gl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
      = IncompleteMissingAttachment
   | x == gl_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER = IncompleteDrawBuffer
   | x == gl_FRAMEBUFFER_INCOMPLETE_READ_BUFFER = IncompleteReadBuffer
   | x == gl_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE = IncompleteMultiSample
   | x == gl_FRAMEBUFFER_UNSUPPORTED = Unsupported
   | otherwise = error $ "unmarshalFramebufferStatus: unknown value: "
      ++ show x
-----------------------------------------------------------------------------

framebufferStatus :: FramebufferTarget -> GettableStateVar FramebufferStatus
framebufferStatus t = makeGettableStateVar $ fmap unmarshalFramebufferStatus
   . glCheckFramebufferStatus . marshalFramebufferTarget $ t

-----------------------------------------------------------------------------
