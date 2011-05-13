-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Rendering.OpenGL.GL.TransformFeedback
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

module Graphics.Rendering.OpenGL.GL.TransformFeedback (
   -- * starting and ending
   beginTransformFeedback, endTransformFeedback,

   TransformFeedbackBufferMode(..), unmarshalTransformFeedbackBufferMode,

   -- * limits
   maxTransformFeedbackSeparateAttribs,
   maxTransformFeedbackInterleavedComponents,
   maxTransformFeedbackSeparateComponents
) where

import Data.StateVar
import Graphics.Rendering.OpenGL.Raw.Core32
import Graphics.Rendering.OpenGL.GL.PrimitiveMode
import Graphics.Rendering.OpenGL.GL.QueryUtils

beginTransformFeedback :: PrimitiveMode -> IO ()
beginTransformFeedback = glBeginTransformFeedback . marshalPrimitiveMode

endTransformFeedback :: IO ()
endTransformFeedback = glEndTransformFeedback


--TranformFeedbackBuffer mode
data TransformFeedbackBufferMode = InterleavedAttribs | SeperateAttribs

marshalTransformFeedbackBufferMode :: TransformFeedbackBufferMode -> GLenum
marshalTransformFeedbackBufferMode x = case x of
   InterleavedAttribs -> gl_INTERLEAVED_ATTRIBS
   SeperateAttribs -> gl_SEPARATE_ATTRIBS

unmarshalTransformFeedbackBufferMode :: GLenum -> TransformFeedbackBufferMode
unmarshalTransformFeedbackBufferMode x
   | x == gl_INTERLEAVED_ATTRIBS = InterleavedAttribs
   | x == gl_SEPARATE_ATTRIBS = SeperateAttribs
   | otherwise = error $ "unmarshalTransformFeedbackBufferMode: illegal value " ++ show x

-- limits
maxTransformFeedbackSeparateAttribs :: GettableStateVar GLint
maxTransformFeedbackSeparateAttribs = makeGettableStateVar $
   getInteger1 fromIntegral GetMaxTransformFeedbackSeparateAttribs

maxTransformFeedbackInterleavedComponents :: GettableStateVar GLint
maxTransformFeedbackInterleavedComponents = makeGettableStateVar $
   getInteger1 fromIntegral GetMaxTransformFeedbackInterleavedComponents

maxTransformFeedbackSeparateComponents :: GettableStateVar GLint
maxTransformFeedbackSeparateComponents = makeGettableStateVar $
   getInteger1 fromIntegral GetMaxTransformFeedbackSeparateComponents
