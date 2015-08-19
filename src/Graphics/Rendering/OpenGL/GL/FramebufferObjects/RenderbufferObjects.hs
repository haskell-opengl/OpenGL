-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects
-- Copyright   :  (c) Sven Panne, Lars Corbijn 2011-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects (
   RenderbufferObject,
   noRenderbufferObject,
   RenderbufferTarget(..),
   RenderbufferSize(..), Samples(..),

   bindRenderbuffer,

   renderbufferStorage, renderbufferStorageMultiSample,
) where

import Data.StateVar
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObject
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferTarget
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat
import Graphics.Rendering.OpenGL.Raw

-----------------------------------------------------------------------------

noRenderbufferObject :: RenderbufferObject
noRenderbufferObject = RenderbufferObject 0

-----------------------------------------------------------------------------

data RenderbufferSize = RenderbufferSize !GLsizei !GLsizei
   deriving ( Eq, Ord, Show )

-----------------------------------------------------------------------------

bindRenderbuffer :: RenderbufferTarget -> StateVar RenderbufferObject
bindRenderbuffer rbt =
    makeStateVar (getBoundRenderbuffer rbt) (setRenderbuffer rbt)

marshalRenderbufferTargetBinding :: RenderbufferTarget -> PName1I
marshalRenderbufferTargetBinding x = case x of
    Renderbuffer -> GetRenderbufferBinding

getBoundRenderbuffer :: RenderbufferTarget -> IO RenderbufferObject
getBoundRenderbuffer =
   getInteger1 (RenderbufferObject . fromIntegral) . marshalRenderbufferTargetBinding

setRenderbuffer :: RenderbufferTarget -> RenderbufferObject -> IO ()
setRenderbuffer rbt = glBindRenderbuffer (marshalRenderbufferTarget rbt)
   . renderbufferID

-----------------------------------------------------------------------------

renderbufferStorageMultiSample :: RenderbufferTarget -> Samples
   -> PixelInternalFormat -> RenderbufferSize -> IO ()
renderbufferStorageMultiSample rbt (Samples s) pif (RenderbufferSize w h) =
   glRenderbufferStorageMultisample (marshalRenderbufferTarget rbt) s
       (marshalPixelInternalFormat' pif) w h


renderbufferStorage :: RenderbufferTarget -> PixelInternalFormat
   -> RenderbufferSize -> IO ()
renderbufferStorage rbt pif (RenderbufferSize w h) =
    glRenderbufferStorage (marshalRenderbufferTarget rbt)
       (marshalPixelInternalFormat' pif) w h
