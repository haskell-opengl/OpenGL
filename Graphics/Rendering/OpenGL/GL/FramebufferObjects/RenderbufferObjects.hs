-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Rendering.OpenGL.GL.FramebufferObjects.RendebufferObjects
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <sven.panne@aedion.de>
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects (
   RenderbufferObject(RenderbufferObject),
   noRenderbufferObject,
   RenderbufferTarget(..), marshalRenderbufferTarget,
   RenderbufferSize(..), Samples(..),

   bindRenderbuffer,

   renderbufferStorage, renderbufferStorageMultiSample,

   getRBParameteriv,
) where

import Foreign.Marshal
import Graphics.Rendering.OpenGL.Raw.Core31

import Graphics.Rendering.OpenGL.GL.ObjectName
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat

-----------------------------------------------------------------------------

data RenderbufferObject = RenderbufferObject{ rbufferID :: GLuint}

instance ObjectName RenderbufferObject where
   genObjectNames n =
      allocaArray n $ \buf -> do
         glGenRenderbuffers (fromIntegral n) buf
         fmap (map RenderbufferObject) $ peekArray n buf
   deleteObjectNames objs = withArrayLen (map rbufferID objs) $
      glDeleteRenderbuffers . fromIntegral
   isObjectName = fmap unmarshalGLboolean . glIsRenderbuffer . rbufferID

noRenderbufferObject :: RenderbufferObject
noRenderbufferObject = RenderbufferObject 0

-----------------------------------------------------------------------------

data RenderbufferTarget =
   Renderbuffer

marshalRenderbufferTarget :: RenderbufferTarget -> GLenum
marshalRenderbufferTarget x = case x of
    Renderbuffer -> gl_RENDERBUFFER

marshalRenderbufferTargetBinding :: RenderbufferTarget -> PName1I
marshalRenderbufferTargetBinding x = case x of
    Renderbuffer -> GetRenderbufferBinding
-----------------------------------------------------------------------------

data RenderbufferSize = RenderbufferSize !GLsizei !GLsizei

newtype Samples = Samples GLsizei

-----------------------------------------------------------------------------

bindRenderbuffer :: RenderbufferTarget -> StateVar RenderbufferObject
bindRenderbuffer rbt =
    makeStateVar (getBoundRenderbuffer rbt) (setRenderbuffer rbt)

getBoundRenderbuffer :: RenderbufferTarget -> IO RenderbufferObject
getBoundRenderbuffer = getInteger1 (RenderbufferObject . fromIntegral)
   . marshalRenderbufferTargetBinding

setRenderbuffer :: RenderbufferTarget -> RenderbufferObject -> IO ()
setRenderbuffer rbt = glBindRenderbuffer (marshalRenderbufferTarget rbt)
   . rbufferID

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

-----------------------------------------------------------------------------

getRBParameteriv :: RenderbufferTarget -> (GLint -> a) -> GLenum -> IO a
getRBParameteriv rbt f p = alloca $ \buf -> do
   glGetRenderbufferParameteriv (marshalRenderbufferTarget rbt)
      p buf
   peek1 f buf
