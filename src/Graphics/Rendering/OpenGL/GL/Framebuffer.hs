--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Framebuffer
-- Copyright   :  (c) Sven Panne 2002-2015
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 17.4 (Whole Framebuffer Operations) of the
-- OpenGL 4.5 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Framebuffer (
   -- * Selecting a Buffer for Writing
   BufferMode(..),
   drawBuffer, namedFramebufferDrawBuffer,
   drawBuffers, namedFramebufferDrawBuffers,
   DrawBufferIndex, drawBufferi,
   maxDrawBuffers,

   -- * Fine Control of Buffer Updates
   indexMask, colorMask, colorMaski, depthMask,
   stencilMask, stencilMaskSeparate,

   -- * Clearing the Buffers
   ClearBuffer(..), clear,
   clearColor, clearIndex, clearDepth, clearDepthf, clearStencil, clearAccum,

   ClearBufferCommand(..), clearBuffer, clearNamedFramebuffer,

   -- * Invalidating Framebuffer Contents
   invalidateSubFramebuffer, invalidateNamedFramebufferSubData,
   invalidateFramebuffer, invalidateNamedFramebufferData,

   -- * The Accumulation Buffer
   AccumOp(..), accum,

   -- * Querying the Buffer Configuration
   auxBuffers, doubleBuffer, stereoBuffer,
   rgbaBits, stencilBits, depthBits, accumBits, rgbaSignedComponents,
) where

import Control.Monad
import Data.Maybe
import Data.StateVar
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Graphics.Rendering.OpenGL.GL.BufferMode
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.Face
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObject
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjectAttachment
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferTarget
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

-- | When colors are written to the framebuffer, they are written into the color
-- buffers specified by 'drawBuffer'.
--
-- If more than one color buffer is selected for drawing, then blending or
-- logical operations are computed and applied independently for each color
-- buffer and can produce different results in each buffer.
--
-- Monoscopic contexts include only left buffers, and stereoscopic contexts
-- include both left and right buffers. Likewise, single-buffered contexts
-- include only front buffers, and double-buffered contexts include both front
-- and back buffers. The context is selected at GL initialization.
--
-- The initial value is 'FrontBuffers' for single-buffered contexts, and
-- 'BackBuffers' for double-buffered contexts.

drawBuffer :: StateVar BufferMode
drawBuffer = makeStateVar getDrawBuffer setDrawBuffer

getDrawBuffer :: IO BufferMode
getDrawBuffer = getDrawBufferi 0

setDrawBuffer :: BufferMode -> IO ()
setDrawBuffer mode =
   withBufferModes [mode] $ \[m] ->
      glDrawBuffer m

-- | The direct-state-access version of 'drawBuffer'.

namedFramebufferDrawBuffer :: FramebufferObject -> SettableStateVar BufferMode
namedFramebufferDrawBuffer fbo =
   makeSettableStateVar $ \mode ->
      withBufferModes [mode] $ \[m] ->
         glNamedFramebufferDrawBuffer (framebufferID fbo) m

-- | 'drawBuffers' defines the draw buffers to which all fragment colors are
-- written. The draw buffers being defined correspond in order to the respective
-- fragment colors. The draw buffer for fragment colors beyond those specified
-- is set to 'NoBuffers'.
--
-- Except for 'NoBuffers', a buffer may not appear more then once in the given
-- list. Specifying a buffer more then once will result in an
-- 'Graphics.Rendering.OpenGL.GLU.Errors.InvalidOperation'.
--
-- If fixed-function fragment shading is being performed, 'drawBuffers'
-- specifies a set of draw buffers into which the fragment color is written.
--
-- If a fragment shader writes to @gl_FragColor@, 'drawBuffers' specifies a set
-- of draw buffers into which the single fragment color defined by
-- @gl_FragColor@ is written. If a fragment shader writes to @gl_FragData@,
-- 'drawBuffers' specifies a set of draw buffers into which each of the multiple
-- fragment colors defined by @gl_FragData@ are separately written. If a
-- fragment shader writes to neither @gl_FragColor@ nor @gl_FragData@, the
-- values of the fragment colors following shader execution are undefined, and
-- may differ for each fragment color.

drawBuffers :: StateVar [BufferMode]
drawBuffers = makeStateVar getDrawBuffers setDrawBuffers

getDrawBuffers :: IO [BufferMode]
getDrawBuffers = do
   n <- get maxDrawBuffers
   mapM getDrawBufferi [ 0 .. n-1 ]

setDrawBuffers :: [BufferMode] -> IO ()
setDrawBuffers modes =
   withBufferModes modes $ \ms ->
      withArrayLen ms $ \len ->
         glDrawBuffers (fromIntegral len)

-- | The direct-state-access version of 'drawBuffers'.

namedFramebufferDrawBuffers :: FramebufferObject -> SettableStateVar [BufferMode]
namedFramebufferDrawBuffers fbo =
   makeSettableStateVar $ \modes ->
      withBufferModes modes $ \ms ->
         withArrayLen ms $ \len ->
            glNamedFramebufferDrawBuffers (framebufferID fbo) (fromIntegral len)

withBufferModes :: [BufferMode] -> ([GLenum] -> IO ()) -> IO ()
withBufferModes modes success
   | all isJust ms = success (catMaybes ms)
   | otherwise = recordInvalidValue
   where ms = map marshalBufferMode modes

-- | The index of the draw buffer.

type DrawBufferIndex = GLuint

-- | 'drawBufferi' is a fast query function. For indices in the range
-- 0..'maxDrawBuffers'-1 its results is the same as selecting the corresponding
-- element from the list returned by 'drawBuffers', but this function uses only
-- one GL function call instead of 'maxDrawBuffers' ones.

drawBufferi :: DrawBufferIndex -> GettableStateVar BufferMode
drawBufferi = makeGettableStateVar . getDrawBufferi . fromIntegral

getDrawBufferi :: GLsizei -> IO BufferMode
getDrawBufferi = getEnum1 unmarshalBufferMode . GetDrawBufferN

-- | Contains the maximum number of buffers that can activated via 'drawBuffers'
-- or which can be simultaneously written into from within a fragment shader
-- using the special output variable array @gl_FragData@. This constant
-- effectively defines the size of the @gl_FragData@ array. The minimum legal
-- value is 1.

maxDrawBuffers :: GettableStateVar GLsizei
maxDrawBuffers = makeGettableStateVar $ getSizei1 id GetMaxDrawBuffers

--------------------------------------------------------------------------------

-- | Controls the writing of individual bits in the color index buffers. The
-- least significant /n/ bits of its value, where /n/ is the number of bits in a
-- color index buffer, specify a mask. Where a 1 appears in the mask, it is
-- possible to write to the corresponding bit in the color index buffer (or
-- buffers). Where a 0 appears, the corresponding bit is write-protected.
--
-- This mask is used only in color index mode, and it affects only the buffers
-- currently selected for writing (see 'drawBuffer'). Initially, all bits are
-- enabled for writing.

indexMask :: StateVar GLuint
indexMask =
   makeStateVar (getInteger1 fromIntegral GetIndexWritemask) glIndexMask

-- | Controls whether the individual color components in the framebuffer can or
-- cannot be written. If the red flag is 'Disabled', for example, no change is
-- made to the red component of any pixel in any of the color buffers,
-- regardless of the drawing operation attempted. Initially, all color
-- components can be written.
--
-- Changes to individual bits of components cannot be controlled. Rather,
-- changes are either enabled or disabled for entire color components.
-- Furthermore, this mask is used only in RGBA mode.

colorMask :: StateVar (Color4 Capability)
colorMask =
   makeStateVar
      (getBoolean4 (\r g b a -> Color4 (unmarshalCapability r)
                                       (unmarshalCapability g)
                                       (unmarshalCapability b)
                                       (unmarshalCapability a))
                                       GetColorWritemask)
      (\(Color4 r g b a) -> glColorMask (marshalCapability r)
                                        (marshalCapability g)
                                        (marshalCapability b)
                                        (marshalCapability a))

-- | 'colorMaski' is a version of 'colorMask' that only applies to the specified
-- draw buffer.

colorMaski :: DrawBufferIndex -> StateVar (Color4 Capability)
colorMaski idx =
   makeStateVar
      (getBoolean4i (\r g b a -> Color4 (unmarshalCapability r)
                                        (unmarshalCapability g)
                                        (unmarshalCapability b)
                                        (unmarshalCapability a))
                    GetColorWritemask idx)
      (\(Color4 r g b a) -> glColorMaski idx
                                         (marshalCapability r)
                                         (marshalCapability g)
                                         (marshalCapability b)
                                         (marshalCapability a))

-- | Controls whether the depth buffer is enabled for writing. The initial state
-- is 'Enabled'.

depthMask :: StateVar Capability
depthMask = makeStateVar (getBoolean1 unmarshalCapability GetDepthWritemask)
                         (glDepthMask . marshalCapability)

-- | Controls the writing of individual bits in the stencil planes. The least
-- significant /n/ bits of its value, where /n/ is the number of bits in the
-- stencil buffer, specify a mask. Where a 1 appears in the mask, it is possible
-- to write to the corresponding bit in the stencil buffer. Where a 0 appears,
-- the corresponding bit is write-protected. Initially, all bits are enabled for
-- writing.

stencilMask :: StateVar GLuint
stencilMask =
   makeStateVar (get (stencilMaskSeparate Front)) glStencilMask

-- | A per-face version of 'stencilMask'.

stencilMaskSeparate :: Face -> StateVar GLuint
stencilMaskSeparate face =
   makeStateVar
      (case face of
          Front -> getInteger1 fromIntegral GetStencilWritemask
          Back -> getInteger1 fromIntegral GetStencilBackWritemask
          FrontAndBack -> do recordInvalidEnum; return 0)
      (glStencilMaskSeparate (marshalFace face))

--------------------------------------------------------------------------------

-- | The buffers which can be cleared with 'clear'.

data ClearBuffer =
     ColorBuffer   -- ^ The buffers currently enabled for color writing.
   | AccumBuffer   -- ^ The accumulation buffer.
   | StencilBuffer -- ^ The stencil buffer.
   | DepthBuffer   -- ^ The depth buffer.
   deriving ( Eq, Ord, Show )

marshalClearBuffer :: ClearBuffer -> GLbitfield
marshalClearBuffer x = case x of
   ColorBuffer -> gl_COLOR_BUFFER_BIT
   AccumBuffer -> gl_ACCUM_BUFFER_BIT
   StencilBuffer -> gl_STENCIL_BUFFER_BIT
   DepthBuffer -> gl_DEPTH_BUFFER_BIT

-- | Set the bitplane area of the window to values previously selected by
-- 'clearColor', 'clearIndex', 'clearDepth', 'clearStencil', and 'clearAccum'.
-- Multiple color buffers can be cleared simultaneously by selecting more than
-- one buffer at a time using 'drawBuffer'.
--
-- The pixel ownership test, the scissor test, dithering, and the buffer
-- writemasks affect the operation of 'clear'. The scissor box bounds the
-- cleared region. Alpha function, blend function, logical operation,
-- stenciling, texure mapping, and depth-buffering are ignored by 'clear'.
--
-- 'clear' takes a list of buffers, indicating which buffers are to be cleared.
-- If a buffer is not present, then a 'clear' directed at that buffer has no
-- effect.
--
-- The value to which each buffer is cleared depends on the setting of the clear
-- value for that buffer.

clear :: [ClearBuffer] -> IO ()
clear = glClear . sum . map marshalClearBuffer

-- | Controls the red, green, blue, and alpha values used by 'clear' to clear
-- the color buffers. Initially, all values are 0.

clearColor :: StateVar (Color4 GLfloat)
clearColor = makeStateVar (getFloat4 Color4 GetColorClearValue)
                          (\(Color4 r g b a) -> glClearColor r g b a)

-- | Controls the index /c/ used by 'clear' to clear the color index buffers.
-- /c/ is not clamped. Rather, /c/ is converted to a fixed-point value with
-- unspecified precision to the right of the binary point. The integer part of
-- this value is then masked with 2^/m/-1, where /m/ is the number of bits in a
-- color index stored in the framebuffer. Initially, the value is 0.

clearIndex :: StateVar (Index1 GLfloat)
clearIndex = makeStateVar (getFloat1 Index1 GetIndexClearValue)
                          (\(Index1 i) -> glClearIndex i)

-- | Controls the depth value used by 'clear' to clear the depth buffer. The
-- initial value is 1.

clearDepth :: StateVar GLdouble
clearDepth = makeStateVar (getDouble1 id GetDepthClearValue) glClearDepth

-- | A variant of 'clearDepth' with a 'GLfloat' argument.

clearDepthf :: StateVar GLfloat
clearDepthf = makeStateVar (getFloat1 id GetDepthClearValue) glClearDepthf

-- | Controls the value /s/ used by 'clear' to clear the stencil buffer. /s/ is
-- masked with 2^/m/-1, where /m/ is the number of bits in the stencil buffer.
-- Initially, the value is 0.

clearStencil :: StateVar GLint
clearStencil = makeStateVar (getInteger1 id GetStencilClearValue) glClearStencil

-- | Controls the red, green, blue, and alpha values used by 'clear' to clear
-- the accumulation buffer. Values written into 'clearAccum' are clamped to the
-- range [-1, 1]. The initial values are all 0.

clearAccum :: StateVar (Color4 GLfloat)
clearAccum =
   makeStateVar (getFloat4 Color4 GetAccumClearValue)
                (\(Color4 r g b a) -> glClearAccum r g b a)

-- | Describes which buffer(s) to clear and the value to use.

data ClearBufferCommand
   = ClearColorBufferInt DrawBufferIndex (Color4 GLint)
   -- ^ Clear the signed integer color buffer(s) at the given index.
   | ClearColorBufferFloat DrawBufferIndex (Color4 GLfloat)
   -- ^ Clear the fixed- or floating-point color buffer(s) at the given index.
   | ClearColorBufferUint DrawBufferIndex (Color4 GLuint)
   -- ^ Clear the unsigned color buffer(s) at the given index.
   | ClearDepthBuffer GLfloat
   -- ^ Clear the depth buffer.
   | ClearStencilBuffer GLint
   -- ^ Clear the stencil buffer.
   | ClearDepthAndStencilBuffers GLfloat GLint
   -- ^ Clear the depth buffer and the stencil buffer.
   deriving ( Eq, Ord, Show )

-- | Clear the given buffer(s).

clearBuffer :: ClearBufferCommand -> IO ()
clearBuffer cmd = case cmd of
   ClearColorBufferInt i c ->
      with c $ glClearBufferiv gl_COLOR (fromIntegral i) . castPtr
   ClearColorBufferFloat i c ->
      with c $ glClearBufferfv gl_COLOR (fromIntegral i) . castPtr
   ClearColorBufferUint i c ->
      with c $ glClearBufferuiv gl_COLOR (fromIntegral i) . castPtr
   ClearDepthBuffer d ->
      with d $ glClearBufferfv gl_DEPTH 0
   ClearStencilBuffer s ->
      with s $ glClearBufferiv gl_STENCIL 0
   ClearDepthAndStencilBuffers d s ->
      glClearBufferfi gl_DEPTH_STENCIL 0 d s

-- | The direct-state-access version of 'clearBuffer'.

clearNamedFramebuffer :: FramebufferObject -> ClearBufferCommand -> IO ()
clearNamedFramebuffer fbo cmd = case cmd of
   ClearColorBufferInt i c ->
      with c $ glClearNamedFramebufferiv f gl_COLOR (fromIntegral i) . castPtr
   ClearColorBufferFloat i c ->
      with c $ glClearNamedFramebufferfv f gl_COLOR (fromIntegral i) . castPtr
   ClearColorBufferUint i c ->
      with c $ glClearNamedFramebufferuiv f gl_COLOR (fromIntegral i) . castPtr
   ClearDepthBuffer d ->
      with d $ glClearNamedFramebufferfv f gl_DEPTH 0
   ClearStencilBuffer s ->
      with s $ glClearNamedFramebufferiv f gl_STENCIL 0
   ClearDepthAndStencilBuffers d s ->
      glClearNamedFramebufferfi f gl_DEPTH_STENCIL d s
   where f = framebufferID fbo

--------------------------------------------------------------------------------

-- | Invalidate a region of the attachments bound to the given target.

invalidateSubFramebuffer :: FramebufferTarget -> [FramebufferObjectAttachment] -> (Position, Size) -> IO ()
invalidateSubFramebuffer target attachments (Position x y, Size w h) =
   withAttachments attachments $ \numAttachments atts ->
      glInvalidateSubFramebuffer (marshalFramebufferTarget target) numAttachments atts x y w h

-- | The direct-state-access version of 'invalidateSubFramebuffer'.

invalidateNamedFramebufferSubData :: FramebufferObject -> [FramebufferObjectAttachment] -> (Position, Size) -> IO ()
invalidateNamedFramebufferSubData fbo attachments (Position x y, Size w h) =
   withAttachments attachments $ \numAttachments atts ->
      glInvalidateNamedFramebufferSubData (framebufferID fbo) numAttachments atts x y w h

-- | A version of 'invalidateSubFramebuffer' affecting the whole viewport.

invalidateFramebuffer :: FramebufferTarget -> [FramebufferObjectAttachment] -> IO ()
invalidateFramebuffer target attachments =
   withAttachments attachments $
      glInvalidateFramebuffer (marshalFramebufferTarget target)

-- | The direct-state-access version of 'invalidateFramebuffer'.

invalidateNamedFramebufferData :: FramebufferObject -> [FramebufferObjectAttachment] -> IO ()
invalidateNamedFramebufferData fbo attachments =
   withAttachments attachments $
      glInvalidateNamedFramebufferData (framebufferID fbo)

withAttachments :: [FramebufferObjectAttachment] -> (GLsizei -> Ptr GLenum -> IO ()) -> IO ()
withAttachments attachments success
   | all isJust atts = withArrayLen (catMaybes atts) $ \len buf ->
                          success (fromIntegral len) buf
   | otherwise = recordInvalidEnum
   where atts = map marshalFramebufferObjectAttachment attachments

--------------------------------------------------------------------------------

-- | The implementation and context dependent number of auxiliary buffers.

auxBuffers :: GettableStateVar GLsizei
auxBuffers = makeGettableStateVar $ getSizei1 id GetAuxBuffers

-- | 'True' if front and back buffers exist.

doubleBuffer :: GettableStateVar Bool
doubleBuffer =
   makeGettableStateVar $ getBoolean1 unmarshalGLboolean GetDoublebuffer

-- | 'True' if left and right buffers exist.

stereoBuffer :: GettableStateVar Bool
stereoBuffer =
    makeGettableStateVar $ getBoolean1 unmarshalGLboolean GetStereo

rgbaBits :: GettableStateVar (Color4 GLsizei)
rgbaBits =
   makeGettableStateVar $
      liftM4 Color4 (getSizei1 id GetRedBits)
                    (getSizei1 id GetGreenBits)
                    (getSizei1 id GetBlueBits)
                    (getSizei1 id GetAlphaBits)

stencilBits :: GettableStateVar GLsizei
stencilBits = makeGettableStateVar $ getSizei1 id GetStencilBits

depthBits :: GettableStateVar GLsizei
depthBits = makeGettableStateVar $ getSizei1 id GetDepthBits

accumBits :: GettableStateVar (Color4 GLsizei)
accumBits =
   makeGettableStateVar $
      liftM4 Color4 (getSizei1 id GetAccumRedBits)
                    (getSizei1 id GetAccumGreenBits)
                    (getSizei1 id GetAccumBlueBits)
                    (getSizei1 id GetAccumAlphaBits)

rgbaSignedComponents :: GettableStateVar (Color4 Bool)
rgbaSignedComponents =
   makeGettableStateVar $
      getInteger4 (\r g b a -> Color4 (unmarshalGLboolean r)
                                      (unmarshalGLboolean g)
                                      (unmarshalGLboolean b)
                                      (unmarshalGLboolean a))
                  GetRGBASignedComponents

--------------------------------------------------------------------------------

-- | An operation on the accumulation buffer.

data AccumOp =
     Accum
     -- ^ Obtains /R/, /G/, /B/, and /A/ values from the buffer currently
     -- selected for reading (see
     -- 'Graphics.Rendering.OpenGL.GL.ReadCopyPixels.readBuffer'). Each
     -- component value is divided by 2^/n/-1, where /n/ is the number of bits
     -- allocated to each color component in the currently selected buffer. The
     -- result is a floating-point value in the range [0, 1], which is
     -- multiplied by the value given to 'accum' and added to the corresponding
     -- pixel component in the accumulation buffer, thereby updating the
     -- accumulation buffer.
   | Load
     -- ^ Similar to 'Accum', except that the current value in the accumulation
     -- buffer is not used in the calculation of the new value. That is, the
     -- /R/, /G/, /B/, and /A/ values from the currently selected buffer are
     -- divided by 2^/n/-1, multiplied by the value given to 'accum', and then
     -- stored in the corresponding accumulation buffer cell, overwriting the
     -- current value.
   | Return
     -- ^ Transfers accumulation buffer values to the color buffer or buffers
     -- currently selected for writing. Each /R/, /G/, /B/, and /A/ component
     -- is multiplied by the value given to 'accum', then multiplied by 2^/n/-1,
     -- clamped to the range [0, 2^/n/-1], and stored in the corresponding
     -- display buffer cell. The only fragment operations that are applied to
     -- this transfer are pixel ownership, scissor, dithering, and color
     -- writemasks.
   | Mult
     -- ^ Multiplies each /R/, /G/, /B/, and /A/ in the accumulation buffer by
     -- the value given to 'accum' and returns the scaled component to its
     -- corresponding accumulation buffer location.
   | Add
     -- ^ Adds the value given to 'accum' to each /R/, /G/, /B/, and /A/ in the
     -- accumulation buffer.
   deriving ( Eq, Ord, Show )

marshalAccumOp :: AccumOp -> GLenum
marshalAccumOp x = case x of
   Accum -> gl_ACCUM
   Load -> gl_LOAD
   Return -> gl_RETURN
   Mult -> gl_MULT
   Add -> gl_ADD

--------------------------------------------------------------------------------

-- | The accumulation buffer is an extended-range color buffer. Images are not
-- rendered into it. Rather, images rendered into one of the color buffers are
-- added to the contents of the accumulation buffer after rendering. Effects
-- such as antialiasing (of points, lines, and polygons), motion blur, and
-- depth of field can be created by accumulating images generated with different
-- transformation matrices.
--
-- Each pixel in the accumulation buffer consists of red, green, blue, and alpha
-- values. The number of bits per component in the accumulation buffer depends
-- on the implementation (see 'accumBits'). Regardless of the number of bits per
-- component, the range of values stored by each component is [-1, 1]. The
-- accumulation buffer pixels are mapped one-to-one with frame buffer pixels.
--
-- 'accum' operates on the accumulation buffer. The first argument selects an
-- accumulation buffer operation. The second argument, is a floating-point value
-- to be used in that operation, see 'AccumOp'.
--
-- All accumulation buffer operations are limited to the area of the current
-- scissor box and applied identically to the red, green, blue, and alpha
-- components of each pixel. If an 'accum' operation results in a value outside
-- the range [-1, 1], the contents of an accumulation buffer pixel component
-- are undefined.
--
-- To clear the accumulation buffer, use 'clearAccum' to specify the clear
-- value, then call 'clear' with the accumulation buffer enabled.

accum :: AccumOp -> GLfloat -> IO ()
accum = glAccum . marshalAccumOp
