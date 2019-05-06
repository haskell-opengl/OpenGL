--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PerFragment
-- Copyright   :  (c) Sven Panne 2002-2019
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
import Graphics.GL

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
   OpZero -> GL_ZERO
   OpKeep -> GL_KEEP
   OpReplace -> GL_REPLACE
   OpIncr -> GL_INCR
   OpIncrWrap -> GL_INCR_WRAP
   OpDecr -> GL_DECR
   OpDecrWrap -> GL_DECR_WRAP
   OpInvert -> GL_INVERT

unmarshalStencilOp :: GLenum -> StencilOp
unmarshalStencilOp x
   | x == GL_ZERO = OpZero
   | x == GL_KEEP = OpKeep
   | x == GL_REPLACE = OpReplace
   | x == GL_INCR = OpIncr
   | x == GL_INCR_WRAP = OpIncrWrap
   | x == GL_DECR = OpDecr
   | x == GL_DECR_WRAP = OpDecrWrap
   | x == GL_INVERT = OpInvert
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
blendBuffer = makeIndexedCapability ((fromIntegral GL_DRAW_BUFFER0) +) BlendI

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
   FuncAdd -> GL_FUNC_ADD
   FuncSubtract -> GL_FUNC_SUBTRACT
   FuncReverseSubtract -> GL_FUNC_REVERSE_SUBTRACT
   Min -> GL_MIN
   Max -> GL_MAX
   LogicOp -> GL_INDEX_LOGIC_OP

unmarshalBlendEquation :: GLenum -> BlendEquation
unmarshalBlendEquation x
   | x == GL_FUNC_ADD = FuncAdd
   | x == GL_FUNC_SUBTRACT = FuncSubtract
   | x == GL_FUNC_REVERSE_SUBTRACT = FuncReverseSubtract
   | x == GL_MIN = Min
   | x == GL_MAX = Max
   | x == GL_INDEX_LOGIC_OP = LogicOp
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
   Clear -> GL_CLEAR
   And -> GL_AND
   AndReverse -> GL_AND_REVERSE
   Copy -> GL_COPY
   AndInverted -> GL_AND_INVERTED
   Noop -> GL_NOOP
   Xor -> GL_XOR
   Or -> GL_OR
   Nor -> GL_NOR
   Equiv -> GL_EQUIV
   Invert -> GL_INVERT
   OrReverse -> GL_OR_REVERSE
   CopyInverted -> GL_COPY_INVERTED
   OrInverted -> GL_OR_INVERTED
   Nand -> GL_NAND
   Set -> GL_SET

unmarshalLogicOp :: GLenum -> LogicOp
unmarshalLogicOp x
   | x == GL_CLEAR = Clear
   | x == GL_AND = And
   | x == GL_AND_REVERSE = AndReverse
   | x == GL_COPY = Copy
   | x == GL_AND_INVERTED = AndInverted
   | x == GL_NOOP = Noop
   | x == GL_XOR = Xor
   | x == GL_OR = Or
   | x == GL_NOR = Nor
   | x == GL_EQUIV = Equiv
   | x == GL_INVERT = Invert
   | x == GL_OR_REVERSE = OrReverse
   | x == GL_COPY_INVERTED = CopyInverted
   | x == GL_OR_INVERTED = OrInverted
   | x == GL_NAND = Nand
   | x == GL_SET = Set
   | otherwise = error ("unmarshalLogicOp: illegal value " ++ show x)

--------------------------------------------------------------------------------

logicOp :: StateVar (Maybe LogicOp)
logicOp =
   makeStateVarMaybe
      (do rgba <- get rgbaMode
          return $ if rgba then CapColorLogicOp else CapIndexLogicOp)
      (getEnum1 unmarshalLogicOp GetLogicOpMode)
      (glLogicOp . marshalLogicOp)
