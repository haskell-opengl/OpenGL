-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects
-- Copyright   :  (c) Sven Panne 2011-2019, Lars Corbijn 2011-2016
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects (
   FramebufferObject, defaultFramebufferObject,
   FramebufferTarget(..), bindFramebuffer,
   FramebufferStatus(..), framebufferStatus,
) where

import Data.StateVar
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObject
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferTarget
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.GL

-----------------------------------------------------------------------------

defaultFramebufferObject :: FramebufferObject
defaultFramebufferObject = FramebufferObject 0

-----------------------------------------------------------------------------

bindFramebuffer :: FramebufferTarget -> StateVar FramebufferObject
bindFramebuffer fbt =
    makeStateVar (getBoundFramebuffer fbt) (setFramebuffer fbt)

marshalFramebufferTargetBinding :: FramebufferTarget -> PName1I
marshalFramebufferTargetBinding x = case x of
   DrawFramebuffer -> GetDrawFramebufferBinding
   ReadFramebuffer -> GetReadFramebufferBinding
   Framebuffer -> GetFramebufferBinding

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
   | x == GL_FRAMEBUFFER_COMPLETE = Complete
   | x == GL_FRAMEBUFFER_UNDEFINED = Undefined
   | x == GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
      = IncompleteMissingAttachment
   | x == GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER = IncompleteDrawBuffer
   | x == GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER = IncompleteReadBuffer
   | x == GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE = IncompleteMultiSample
   | x == GL_FRAMEBUFFER_UNSUPPORTED = Unsupported
   | otherwise = error $ "unmarshalFramebufferStatus: unknown value: "
      ++ show x

-----------------------------------------------------------------------------

framebufferStatus :: FramebufferTarget -> GettableStateVar FramebufferStatus
framebufferStatus t = makeGettableStateVar $ fmap unmarshalFramebufferStatus
   . glCheckFramebufferStatus . marshalFramebufferTarget $ t

-----------------------------------------------------------------------------
