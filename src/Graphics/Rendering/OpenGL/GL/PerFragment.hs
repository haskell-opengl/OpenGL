--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PerFragment
-- Copyright   :  (c) Sven Panne 2002-2015
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 4.1 (Per-Fragment Operations) of the
-- OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PerFragment (
   -- * Discarding Primitives Before Rasterization
   rasterizerDiscard, discardingRasterizer,

   -- * Scissor Test
   scissor,

   -- * Multisample Fragment Operations
   sampleAlphaToCoverage,  sampleAlphaToOne, sampleCoverage,

   -- * Depth Bounds Test
   depthBounds,

   -- * Alpha Test
   ComparisonFunction(..), alphaFunc,

   -- * Stencil Test
   stencilTest, stencilFunc, stencilFuncSeparate, StencilOp(..), stencilOp,
   stencilOpSeparate, activeStencilFace,

   -- * Depth Buffer Test
   depthFunc,

   -- * Blending
   blend, blendBuffer, BlendEquation(..), blendEquation, blendEquationSeparate,
   BlendingFactor(..), blendFuncSeparate, blendFunc, blendColor,

   -- * Dithering
   dither,

   -- * Logical Operation
   LogicOp(..), logicOp
) where

import Control.Monad
import Data.StateVar
import Graphics.Rendering.OpenGL.GL.BlendingFactor
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.ComparisonFunction
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.Exception
import Graphics.Rendering.OpenGL.GL.Face
import Graphics.Rendering.OpenGL.GL.Framebuffer
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

rasterizerDiscard :: StateVar Capability
rasterizerDiscard = makeCapability CapRasterizerDiscard

discardingRasterizer :: IO a -> IO a
discardingRasterizer act = do
   r <- get rasterizerDiscard
   bracket_ (rasterizerDiscard $= Enabled) (rasterizerDiscard $= r) act

--------------------------------------------------------------------------------

scissor :: StateVar (Maybe (Position, Size))
scissor =
   makeStateVarMaybe
      (return CapScissorTest)
      (getInteger4 makeSB GetScissorBox)
      (\(Position x y, Size w h) -> glScissor x y w h)
   where makeSB x y w h = (Position x y, Size (fromIntegral w) (fromIntegral h))

--------------------------------------------------------------------------------

sampleAlphaToCoverage :: StateVar Capability
sampleAlphaToCoverage = makeCapability CapSampleAlphaToCoverage

sampleAlphaToOne :: StateVar Capability
sampleAlphaToOne = makeCapability CapSampleAlphaToOne

sampleCoverage :: StateVar (Maybe (GLclampf, Bool))
sampleCoverage =
   makeStateVarMaybe
      (return CapSampleCoverage)
      (liftM2 (,) (getClampf1 id GetSampleCoverageValue)
                  (getBoolean1 unmarshalGLboolean GetSampleCoverageInvert))
      (\(value, invert) -> glSampleCoverage value (marshalGLboolean invert))

--------------------------------------------------------------------------------

depthBounds :: StateVar (Maybe (GLclampd, GLclampd))
depthBounds =
   makeStateVarMaybe
      (return CapDepthBoundsTest)
      (getClampd2 (,) GetDepthBounds)
      (uncurry glDepthBoundsEXT)

--------------------------------------------------------------------------------

alphaFunc :: StateVar (Maybe (ComparisonFunction, GLclampf))
alphaFunc =
   makeStateVarMaybe
      (return CapAlphaTest)
      (liftM2 (,) (getEnum1 unmarshalComparisonFunction GetAlphaTestFunc)
                  (getClampf1 id GetAlphaTestRef))
      (uncurry (glAlphaFunc . marshalComparisonFunction))

--------------------------------------------------------------------------------

stencilTest :: StateVar Capability
stencilTest = makeCapability CapStencilTest

--------------------------------------------------------------------------------

stencilFunc :: StateVar (ComparisonFunction, GLint, GLuint)
stencilFunc =
   makeStateVar
      (get (stencilFuncSeparate Front))
      (\(func, ref, mask) ->
         glStencilFunc (marshalComparisonFunction func) ref mask)

stencilFuncSeparate :: Face -> StateVar (ComparisonFunction, GLint, GLuint)
stencilFuncSeparate face =
   makeStateVar
      (case face of
          Front -> getStencilFunc GetStencilFunc
                                  GetStencilRef
                                  GetStencilValueMask
          Back -> getStencilFunc GetStencilBackFunc
                                 GetStencilBackRef
                                 GetStencilBackValueMask
          FrontAndBack -> do recordInvalidEnum; return (Never, 0, 0))
      (\(func, ref, mask) ->
         glStencilFuncSeparate (marshalFace face)
                               (marshalComparisonFunction func) ref mask)
   where getStencilFunc func ref mask =
            liftM3 (,,) (getEnum1 unmarshalComparisonFunction func)
                        (getInteger1 id ref)
                        (getInteger1 fromIntegral mask)

--------------------------------------------------------------------------------

data StencilOp =
     OpZero
   | OpKeep
   | OpReplace
   | OpIncr
   | OpIncrWrap
   | OpDecr
   | OpDecrWrap
   | OpInvert
   deriving ( Eq, Ord, Show )

marshalStencilOp :: StencilOp -> GLenum
marshalStencilOp x = case x of
   OpZero -> gl_ZERO
   OpKeep -> gl_KEEP
   OpReplace -> gl_REPLACE
   OpIncr -> gl_INCR
   OpIncrWrap -> gl_INCR_WRAP
   OpDecr -> gl_DECR
   OpDecrWrap -> gl_DECR_WRAP
   OpInvert -> gl_INVERT

unmarshalStencilOp :: GLenum -> StencilOp
unmarshalStencilOp x
   | x == gl_ZERO = OpZero
   | x == gl_KEEP = OpKeep
   | x == gl_REPLACE = OpReplace
   | x == gl_INCR = OpIncr
   | x == gl_INCR_WRAP = OpIncrWrap
   | x == gl_DECR = OpDecr
   | x == gl_DECR_WRAP = OpDecrWrap
   | x == gl_INVERT = OpInvert
   | otherwise = error ("unmarshalStencilOp: illegal value " ++ show x)

--------------------------------------------------------------------------------

stencilOp :: StateVar (StencilOp, StencilOp, StencilOp)
stencilOp =
   makeStateVar
      (get (stencilOpSeparate Front))
      (\(sf, spdf, spdp) -> glStencilOp (marshalStencilOp sf)
                                        (marshalStencilOp spdf)
                                        (marshalStencilOp spdp))

stencilOpSeparate :: Face -> StateVar (StencilOp, StencilOp, StencilOp)
stencilOpSeparate face =
   makeStateVar
      (case face of
          Front -> getStencilOp GetStencilFail
                                GetStencilPassDepthFail
                                GetStencilPassDepthPass
          Back ->  getStencilOp GetStencilBackFail
                                GetStencilBackPassDepthFail
                                GetStencilBackPassDepthPass
          FrontAndBack -> do recordInvalidEnum
                             return (OpZero, OpZero, OpZero))
      (\(sf, spdf, spdp) -> glStencilOpSeparate (marshalFace face)
                                                (marshalStencilOp sf)
                                                (marshalStencilOp spdf)
                                                (marshalStencilOp spdp))
   where getStencilOp sf spdf spdp =
            (liftM3 (,,) (getEnum1 unmarshalStencilOp sf)
                         (getEnum1 unmarshalStencilOp spdf)
                         (getEnum1 unmarshalStencilOp spdp))


--------------------------------------------------------------------------------

activeStencilFace :: StateVar (Maybe Face)
activeStencilFace =
   makeStateVarMaybe
      (return CapStencilTestTwoSide)
      (getEnum1 unmarshalFace GetActiveStencilFace)
      (glActiveStencilFaceEXT . marshalFace)

--------------------------------------------------------------------------------

depthFunc :: StateVar (Maybe ComparisonFunction)
depthFunc =
   makeStateVarMaybe
      (return CapDepthTest)
      (getEnum1 unmarshalComparisonFunction GetDepthFunc)
      (glDepthFunc . marshalComparisonFunction)

--------------------------------------------------------------------------------

blend :: StateVar Capability
blend = makeCapability CapBlend

-- | enable or disable blending based on the buffer bound to the /i/'th drawBuffer
-- that is the buffer fmap (!! i) (get drawBuffers)
blendBuffer :: DrawBufferIndex -> StateVar Capability
blendBuffer = makeIndexedCapability ((fromIntegral gl_DRAW_BUFFER0) +) BlendI

--------------------------------------------------------------------------------

data BlendEquation =
     FuncAdd
   | FuncSubtract
   | FuncReverseSubtract
   | Min
   | Max
   | LogicOp
   deriving ( Eq, Ord, Show )

marshalBlendEquation :: BlendEquation -> GLenum
marshalBlendEquation x = case x of
   FuncAdd -> gl_FUNC_ADD
   FuncSubtract -> gl_FUNC_SUBTRACT
   FuncReverseSubtract -> gl_FUNC_REVERSE_SUBTRACT
   Min -> gl_MIN
   Max -> gl_MAX
   LogicOp -> gl_INDEX_LOGIC_OP

unmarshalBlendEquation :: GLenum -> BlendEquation
unmarshalBlendEquation x
   | x == gl_FUNC_ADD = FuncAdd
   | x == gl_FUNC_SUBTRACT = FuncSubtract
   | x == gl_FUNC_REVERSE_SUBTRACT = FuncReverseSubtract
   | x == gl_MIN = Min
   | x == gl_MAX = Max
   | x == gl_INDEX_LOGIC_OP = LogicOp
   | otherwise = error ("unmarshalBlendEquation: illegal value " ++ show x)

--------------------------------------------------------------------------------

blendEquation :: StateVar BlendEquation
blendEquation =
   makeStateVar
      (getEnum1 unmarshalBlendEquation GetBlendEquation)
      (glBlendEquation . marshalBlendEquation)

blendEquationSeparate :: StateVar (BlendEquation,BlendEquation)
blendEquationSeparate =
   makeStateVar
      (liftM2 (,) (getEnum1 unmarshalBlendEquation GetBlendEquation)
                  (getEnum1 unmarshalBlendEquation GetBlendEquationAlpha))
      (\(funcRGB, funcAlpha) ->
          glBlendEquationSeparate (marshalBlendEquation funcRGB)
                                  (marshalBlendEquation funcAlpha))

--------------------------------------------------------------------------------

blendFuncSeparate ::
   StateVar ((BlendingFactor, BlendingFactor), (BlendingFactor, BlendingFactor))
blendFuncSeparate =
   makeStateVar
      (do srcRGB   <- getEnum1 unmarshalBlendingFactor GetBlendSrcRGB
          srcAlpha <- getEnum1 unmarshalBlendingFactor GetBlendSrcAlpha
          dstRGB   <- getEnum1 unmarshalBlendingFactor GetBlendDstRGB
          dstAlpha <- getEnum1 unmarshalBlendingFactor GetBlendDstAlpha
          return ((srcRGB, srcAlpha), (dstRGB, dstAlpha)))
      (\((srcRGB, srcAlpha), (dstRGB, dstAlpha)) ->
         glBlendFuncSeparate (marshalBlendingFactor srcRGB)
                             (marshalBlendingFactor srcAlpha)
                             (marshalBlendingFactor dstRGB)
                             (marshalBlendingFactor dstAlpha))

blendFunc :: StateVar (BlendingFactor, BlendingFactor)
blendFunc =
   makeStateVar
      (liftM2 (,) (getEnum1 unmarshalBlendingFactor GetBlendSrc)
                  (getEnum1 unmarshalBlendingFactor GetBlendDst))
      (\(s, d) ->
         glBlendFunc (marshalBlendingFactor s) (marshalBlendingFactor d))

blendColor :: StateVar (Color4 GLclampf)
blendColor =
   makeStateVar
      (getClampf4 Color4 GetBlendColor)
      (\(Color4 r g b a) -> glBlendColor r g b a)

--------------------------------------------------------------------------------

dither :: StateVar Capability
dither = makeCapability CapDither

--------------------------------------------------------------------------------

data LogicOp =
     Clear
   | And
   | AndReverse
   | Copy
   | AndInverted
   | Noop
   | Xor
   | Or
   | Nor
   | Equiv
   | Invert
   | OrReverse
   | CopyInverted
   | OrInverted
   | Nand
   | Set
   deriving ( Eq, Ord, Show )

marshalLogicOp :: LogicOp -> GLenum
marshalLogicOp x = case x of
   Clear -> gl_CLEAR
   And -> gl_AND
   AndReverse -> gl_AND_REVERSE
   Copy -> gl_COPY
   AndInverted -> gl_AND_INVERTED
   Noop -> gl_NOOP
   Xor -> gl_XOR
   Or -> gl_OR
   Nor -> gl_NOR
   Equiv -> gl_EQUIV
   Invert -> gl_INVERT
   OrReverse -> gl_OR_REVERSE
   CopyInverted -> gl_COPY_INVERTED
   OrInverted -> gl_OR_INVERTED
   Nand -> gl_NAND
   Set -> gl_SET

unmarshalLogicOp :: GLenum -> LogicOp
unmarshalLogicOp x
   | x == gl_CLEAR = Clear
   | x == gl_AND = And
   | x == gl_AND_REVERSE = AndReverse
   | x == gl_COPY = Copy
   | x == gl_AND_INVERTED = AndInverted
   | x == gl_NOOP = Noop
   | x == gl_XOR = Xor
   | x == gl_OR = Or
   | x == gl_NOR = Nor
   | x == gl_EQUIV = Equiv
   | x == gl_INVERT = Invert
   | x == gl_OR_REVERSE = OrReverse
   | x == gl_COPY_INVERTED = CopyInverted
   | x == gl_OR_INVERTED = OrInverted
   | x == gl_NAND = Nand
   | x == gl_SET = Set
   | otherwise = error ("unmarshalLogicOp: illegal value " ++ show x)

--------------------------------------------------------------------------------

logicOp :: StateVar (Maybe LogicOp)
logicOp =
   makeStateVarMaybe
      (do rgba <- get rgbaMode
          return $ if rgba then CapColorLogicOp else CapIndexLogicOp)
      (getEnum1 unmarshalLogicOp GetLogicOpMode)
      (glLogicOp . marshalLogicOp)
