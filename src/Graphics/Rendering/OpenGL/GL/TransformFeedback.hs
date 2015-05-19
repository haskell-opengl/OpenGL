-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.TransformFeedback
-- Copyright   :  (c) Sven Panne, Lars Corbijn 2011-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.TransformFeedback (
   -- * starting and ending
   beginTransformFeedback, endTransformFeedback,

   -- * TransformFeedbackBufferMode
   TransformFeedbackBufferMode(..), marshalTransformFeedbackBufferMode,
   unmarshalTransformFeedbackBufferMode,

   -- * Shader related
   transformFeedbackBufferMode,
   transformFeedbackVaryings,
   setTransformFeedbackVaryings,

   -- * limits
   maxTransformFeedbackSeparateAttribs,
   maxTransformFeedbackInterleavedComponents,
   maxTransformFeedbackSeparateComponents
) where

import Data.StateVar
import Foreign.Marshal.Array
import Graphics.Rendering.OpenGL.GL.ByteString
import Graphics.Rendering.OpenGL.GL.DataType
import Graphics.Rendering.OpenGL.GL.PrimitiveMode
import Graphics.Rendering.OpenGL.GL.PrimitiveModeInternal
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import Graphics.Rendering.OpenGL.GL.Shaders.Variables
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

beginTransformFeedback :: PrimitiveMode -> IO ()
beginTransformFeedback = glBeginTransformFeedback . marshalPrimitiveMode

endTransformFeedback :: IO ()
endTransformFeedback = glEndTransformFeedback

--------------------------------------------------------------------------------

data TransformFeedbackBufferMode =
     InterleavedAttribs
   | SeparateAttribs
   | SeperateAttribs
   deriving ( Eq, Ord, Show )
{-# DEPRECATED SeperateAttribs "Use 'SeparateAttribs' instead." #-}

marshalTransformFeedbackBufferMode :: TransformFeedbackBufferMode -> GLenum
marshalTransformFeedbackBufferMode x = case x of
   InterleavedAttribs -> gl_INTERLEAVED_ATTRIBS
   SeparateAttribs -> gl_SEPARATE_ATTRIBS
   SeperateAttribs -> gl_SEPARATE_ATTRIBS

unmarshalTransformFeedbackBufferMode :: GLenum -> TransformFeedbackBufferMode
unmarshalTransformFeedbackBufferMode x
   | x == gl_INTERLEAVED_ATTRIBS = InterleavedAttribs
   | x == gl_SEPARATE_ATTRIBS = SeparateAttribs
   | otherwise = error $ "unmarshalTransformFeedbackBufferMode: illegal value " ++ show x

-- limits
-- | Max number of seprate atributes or varyings than can be captured
-- in transformfeedback, initial value 4
maxTransformFeedbackSeparateAttribs :: GettableStateVar GLint
maxTransformFeedbackSeparateAttribs = makeGettableStateVar $
   getInteger1 fromIntegral GetMaxTransformFeedbackSeparateAttribs

-- | Max number of components to write to a single buffer in
-- interleaved mod, initial value 64
maxTransformFeedbackInterleavedComponents :: GettableStateVar GLint
maxTransformFeedbackInterleavedComponents = makeGettableStateVar $
   getInteger1 fromIntegral GetMaxTransformFeedbackInterleavedComponents

-- | Max number of components per attribute or varying in seperate mode
-- initial value 4
maxTransformFeedbackSeparateComponents :: GettableStateVar GLint
maxTransformFeedbackSeparateComponents = makeGettableStateVar $
   getInteger1 fromIntegral GetMaxTransformFeedbackSeparateComponents

--------------------------------------------------------------------------------

-- | Set all the transform feedbacks varyings for this program
-- it overwrites any previous call to this function
setTransformFeedbackVaryings :: Program -> [String]
   -> TransformFeedbackBufferMode -> IO ()
setTransformFeedbackVaryings (Program program) sts tfbm = do
   ptSts <- mapM (\x -> withGLstring x return) sts
   stsPtrs <- newArray ptSts
   glTransformFeedbackVaryings program (fromIntegral . length $ sts)  stsPtrs
      (marshalTransformFeedbackBufferMode tfbm)

-- | Get the currently used transformFeedbackBufferMode
transformFeedbackBufferMode
   :: Program -> GettableStateVar TransformFeedbackBufferMode
transformFeedbackBufferMode = programVar1
   (unmarshalTransformFeedbackBufferMode . fromIntegral)
   TransformFeedbackBufferMode

-- | The number of varyings that are currently recorded when in
-- transform feedback mode
numTransformFeedbackVaryings :: Program -> GettableStateVar GLuint
numTransformFeedbackVaryings =
   programVar1 fromIntegral TransformFeedbackVaryings

-- | The maximum length of a varying's name for transform feedback mode
transformFeedbackVaryingMaxLength :: Program -> GettableStateVar GLsizei
transformFeedbackVaryingMaxLength
   = programVar1 fromIntegral TransformFeedbackVaryingMaxLength

-- | The name, datatype and size of the transform feedback varyings.
transformFeedbackVaryings :: Program -> GettableStateVar [(GLint, DataType, String)]
transformFeedbackVaryings =
   activeVars
      numTransformFeedbackVaryings
      transformFeedbackVaryingMaxLength
      glGetTransformFeedbackVarying
      unmarshalDataType
