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
   VaryingIndex, MaxLength,
   getTransformFeedbackVaryings, setTransformFeedbackVaryings,
   getTransformFeedbackVarying, getTransformFeedbackVaryingMaxLength,

   -- * limits
   maxTransformFeedbackSeparateAttribs,
   maxTransformFeedbackInterleavedComponents,
   maxTransformFeedbackSeparateComponents
) where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.ByteString
import Graphics.Rendering.OpenGL.GL.DataType
import Graphics.Rendering.OpenGL.GL.PrimitiveMode
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

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

-----------------------------------------------------------------------------

type VaryingIndex = GLuint
type MaxLength = GLsizei

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
getTransformFeedbackVaryings :: Program -> GettableStateVar GLuint
getTransformFeedbackVaryings
    = programVar1 fromIntegral TransformFeedbackVaryings

-- | The maximum length of a varying's name for transform feedback mode
getTransformFeedbackVaryingMaxLength :: Program -> GettableStateVar GLuint
getTransformFeedbackVaryingMaxLength
   = programVar1 fromIntegral TransformFeedbackVaryingMaxLength

-- | Get the name, datatype and size of a single transform feedback
-- varying.
getTransformFeedbackVarying :: Program
   -> VaryingIndex -- ^ the index in a previous array of names of
                   -- setTransformFeedbackVaryings
   -> MaxLength -- ^ the maximum length of the returned string
   -> IO (String, DataType, GLsizei) -- ^ The name of the varying, it's type
                                     -- and size
getTransformFeedbackVarying (Program program) vi ml = do
   alloca $ \nameLengthBuf ->
      alloca $ \sizeBuf ->
          alloca $ \typeBuf -> do
             n <- createAndTrimByteString ml $ \nameBuf -> do
                glGetTransformFeedbackVarying program vi ml nameLengthBuf sizeBuf typeBuf nameBuf
                peek nameLengthBuf
             s <- peek sizeBuf
             d <- peek typeBuf
             return (unpackUtf8 n, unmarshalDataType d, s)
