--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PerFragment
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 4.1 (Per-Fragment Operations) of the
-- OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PerFragment (
   -- * Scissor Test
   scissor,

   -- * Multisample Fragment Operations
   sampleAlphaToCoverage,  sampleAlphaToOne, sampleCoverage,

   -- * Depth Bounds Test
   depthBounds,

   -- * Alpha Test
   ComparisonFunction(..), alphaFunc,

   -- * Stencil Test
   stencilFunc, StencilOp(..), stencilOp, stencilTestTwoSide, activeStencilFace,

   -- * Depth Buffer Test
   depthFunc,

   -- * Blending
   BlendEquationMode(..), blendEquation,
   BlendingFactor(..), blendFuncSeparate, blendFunc, blendColor,

   -- * Dithering
   dither,

   -- * Logical Operation
   LogicOp(..), logicOp
) where

import Control.Monad ( liftM2, liftM3 )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapScissorTest,CapSampleAlphaToCoverage,CapSampleAlphaToOne,
             CapSampleCoverage,CapDepthBoundsTest,CapAlphaTest,CapStencilTest,
             CapStencilTestTwoSide,CapDepthTest,CapBlend,CapDither,
             CapIndexLogicOp,CapColorLogicOp),
   makeCapability, makeStateVarMaybe )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLint, GLuint, GLsizei, GLenum, GLclampf, GLclampd, Capability )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Position(..), Size(..) )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.Face ( marshalFace, unmarshalFace )
import Graphics.Rendering.OpenGL.GL.Colors ( Face )
import Graphics.Rendering.OpenGL.GL.GLboolean (
   GLboolean, marshalGLboolean, unmarshalGLboolean )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetScissorBox,GetSampleCoverageValue,GetSampleCoverageInvert,
            GetDepthBounds,GetAlphaTestFunc,GetAlphaTestRef,GetStencilFunc,
            GetStencilRef,GetStencilValueMask,GetStencilFail,
            GetStencilPassDepthFail,GetStencilPassDepthPass,
            GetActiveStencilFace,GetDepthFunc,GetBlendEquation,GetBlendDstRGB,
            GetBlendSrcRGB,GetBlendDstAlpha,GetBlendSrcAlpha,GetBlendSrc,
            GetBlendDst,GetBlendColor,GetLogicOpMode),
   getInteger1, getInteger4, getEnum1, getFloat1, getFloat4, getDouble2,
   getBoolean1 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasGetter(get), StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Color4(..), rgbaMode )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"

--------------------------------------------------------------------------------

scissor :: StateVar (Maybe (Position, Size))
scissor =
   makeStateVarMaybe
      (return CapScissorTest)
      (getInteger4 makeSB GetScissorBox)
      (\(Position x y, Size w h) -> glScissor x y w h)
   where makeSB x y w h = (Position x y, Size (fromIntegral w) (fromIntegral h))
       
foreign import CALLCONV unsafe "glScissor" glScissor ::
   GLint -> GLint -> GLsizei -> GLsizei -> IO ()

--------------------------------------------------------------------------------

sampleAlphaToCoverage :: StateVar Capability
sampleAlphaToCoverage = makeCapability CapSampleAlphaToCoverage

sampleAlphaToOne :: StateVar Capability
sampleAlphaToOne = makeCapability CapSampleAlphaToOne

sampleCoverage :: StateVar (Maybe (GLclampf, Bool))
sampleCoverage =
   makeStateVarMaybe
      (return CapSampleCoverage)
      (liftM2 (,) (getFloat1 id GetSampleCoverageValue)
                  (getBoolean1 unmarshalGLboolean GetSampleCoverageInvert))
      (\(value, invert) -> glSampleCoverageARB value (marshalGLboolean invert))

EXTENSION_ENTRY("GL_ARB_multisample or OpenGL 1.3",glSampleCoverageARB,GLclampf -> GLboolean -> IO ())

--------------------------------------------------------------------------------

depthBounds :: StateVar (Maybe (GLclampd, GLclampd))
depthBounds =
   makeStateVarMaybe
      (return CapDepthBoundsTest)
      (getDouble2 (,) GetDepthBounds)
      (uncurry glDepthBoundsEXT)
       
EXTENSION_ENTRY("GL_EXT_depth_bounds_test",glDepthBoundsEXT,GLclampd -> GLclampd -> IO ())

--------------------------------------------------------------------------------

data ComparisonFunction =
     Never
   | Less
   | Equal
   | Lequal
   | Greater
   | Notequal
   | Gequal
   | Always
   deriving ( Eq, Ord, Show )

marshalComparisonFunction :: ComparisonFunction -> GLenum
marshalComparisonFunction x = case x of
   Never -> 0x200
   Less -> 0x201
   Equal -> 0x202
   Lequal -> 0x203
   Greater -> 0x204
   Notequal -> 0x205
   Gequal -> 0x206
   Always -> 0x207

unmarshalComparisonFunction :: GLenum -> ComparisonFunction
unmarshalComparisonFunction x
   | x == 0x200 = Never
   | x == 0x201 = Less
   | x == 0x202 = Equal
   | x == 0x203 = Lequal
   | x == 0x204 = Greater
   | x == 0x205 = Notequal
   | x == 0x206 = Gequal
   | x == 0x207 = Always
   | otherwise = error ("unmarshalComparisonFunction: illegal value " ++ show x)

--------------------------------------------------------------------------------

alphaFunc :: StateVar (Maybe (ComparisonFunction, GLclampf))
alphaFunc =
   makeStateVarMaybe
      (return CapAlphaTest)
      (liftM2 (,) (getEnum1 unmarshalComparisonFunction GetAlphaTestFunc)
                  (getFloat1 id GetAlphaTestRef))
      (uncurry (glAlphaFunc . marshalComparisonFunction))

foreign import CALLCONV unsafe "glAlphaFunc" glAlphaFunc ::
   GLenum -> GLclampf -> IO ()

--------------------------------------------------------------------------------

stencilFunc :: StateVar (Maybe (ComparisonFunction, GLint, GLuint))
stencilFunc =
   makeStateVarMaybe
      (return CapStencilTest)
      (liftM3 (,,) (getEnum1 unmarshalComparisonFunction GetStencilFunc)
                   (getInteger1 id GetStencilRef)
                   (getInteger1 fromIntegral GetStencilValueMask))
      (\(func, ref, mask) ->
         glStencilFunc (marshalComparisonFunction func) ref mask)

foreign import CALLCONV unsafe "glStencilFunc" glStencilFunc ::
   GLenum -> GLint -> GLuint -> IO ()

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
   OpZero -> 0x0
   OpKeep -> 0x1e00
   OpReplace -> 0x1e01
   OpIncr -> 0x1e02
   OpIncrWrap -> 0x8507
   OpDecr -> 0x1e03
   OpDecrWrap -> 0x8508
   OpInvert -> 0x150a

unmarshalStencilOp :: GLenum -> StencilOp
unmarshalStencilOp x
   | x == 0x0 = OpZero
   | x == 0x1e00 = OpKeep
   | x == 0x1e01 = OpReplace
   | x == 0x1e02 = OpIncr
   | x == 0x8507 = OpIncrWrap
   | x == 0x1e03 = OpDecr
   | x == 0x8508 = OpDecrWrap
   | x == 0x150a = OpInvert
   | otherwise = error ("unmarshalStencilOp: illegal value " ++ show x)

--------------------------------------------------------------------------------

stencilOp :: StateVar (StencilOp, StencilOp, StencilOp)
stencilOp =
   makeStateVar
      (liftM3 (,,) (getEnum1 unmarshalStencilOp GetStencilFail)
                   (getEnum1 unmarshalStencilOp GetStencilPassDepthFail)
                   (getEnum1 unmarshalStencilOp GetStencilPassDepthPass))
      (\(sf, spdf, spdp) -> glStencilOp (marshalStencilOp sf)
                                        (marshalStencilOp spdf)
                                        (marshalStencilOp spdp))

foreign import CALLCONV unsafe "glStencilOp" glStencilOp ::
   GLenum -> GLenum -> GLenum -> IO ()

--------------------------------------------------------------------------------

stencilTestTwoSide :: StateVar Capability
stencilTestTwoSide = makeCapability CapStencilTestTwoSide

activeStencilFace :: StateVar Face
activeStencilFace =
   makeStateVar
      (getEnum1 unmarshalFace GetActiveStencilFace)
      (glActiveStencilFaceEXT . marshalFace)

EXTENSION_ENTRY("GL_EXT_stencil_two_side",glActiveStencilFaceEXT,GLenum -> IO ())

--------------------------------------------------------------------------------

depthFunc :: StateVar (Maybe ComparisonFunction)
depthFunc =
   makeStateVarMaybe
      (return CapDepthTest)
      (getEnum1 unmarshalComparisonFunction GetDepthFunc)
      (glDepthFunc . marshalComparisonFunction)

foreign import CALLCONV unsafe "glDepthFunc" glDepthFunc :: GLenum -> IO ()

--------------------------------------------------------------------------------

data BlendEquationMode =
     FuncAdd
   | FuncSubtract
   | FuncReverseSubtract
   | Min
   | Max
   | LogicOp
   deriving ( Eq, Ord, Show )

marshalBlendEquationMode :: BlendEquationMode -> GLenum
marshalBlendEquationMode x = case x of
   FuncAdd -> 0x8006
   FuncSubtract -> 0x800a
   FuncReverseSubtract -> 0x800b
   Min -> 0x8007
   Max -> 0x8008
   LogicOp -> 0xbf1

unmarshalBlendEquationMode :: GLenum -> BlendEquationMode
unmarshalBlendEquationMode x
   | x == 0x8006 = FuncAdd
   | x == 0x800a = FuncSubtract
   | x == 0x800b = FuncReverseSubtract
   | x == 0x8007 = Min
   | x == 0x8008 = Max
   | x == 0xbf1 = LogicOp
   | otherwise = error ("unmarshalBlendEquationMode: illegal value " ++ show x)

--------------------------------------------------------------------------------

blendEquation :: StateVar (Maybe BlendEquationMode)
blendEquation =
   makeStateVarMaybe
      (return CapBlend)
      (getEnum1 unmarshalBlendEquationMode GetBlendEquation)
      (glBlendEquationEXT . marshalBlendEquationMode)

EXTENSION_ENTRY("GL_EXT_blend_minmax or GL_EXT_blend_subtract or OpenGL 1.4",glBlendEquationEXT,GLenum -> IO ())

--------------------------------------------------------------------------------

data BlendingFactor =
     Zero
   | One
   | SrcColor
   | OneMinusSrcColor
   | DstColor
   | OneMinusDstColor
   | SrcAlpha
   | OneMinusSrcAlpha
   | DstAlpha
   | OneMinusDstAlpha
   | ConstantColor
   | OneMinusConstantColor
   | ConstantAlpha
   | OneMinusConstantAlpha
   | SrcAlphaSaturate
   deriving ( Eq, Ord, Show )

marshalBlendingFactor :: BlendingFactor -> GLenum
marshalBlendingFactor x = case x of
   Zero -> 0x0
   One -> 0x1
   SrcColor -> 0x300
   OneMinusSrcColor -> 0x301
   DstColor -> 0x306
   OneMinusDstColor -> 0x307
   SrcAlpha -> 0x302
   OneMinusSrcAlpha -> 0x303
   DstAlpha -> 0x304
   OneMinusDstAlpha -> 0x305
   ConstantColor -> 0x8001
   OneMinusConstantColor -> 0x8002
   ConstantAlpha -> 0x8003
   OneMinusConstantAlpha -> 0x8004
   SrcAlphaSaturate -> 0x308

unmarshalBlendingFactor :: GLenum -> BlendingFactor
unmarshalBlendingFactor x
   | x == 0x0 = Zero
   | x == 0x1 = One
   | x == 0x300 = SrcColor
   | x == 0x301 = OneMinusSrcColor
   | x == 0x306 = DstColor
   | x == 0x307 = OneMinusDstColor
   | x == 0x302 = SrcAlpha
   | x == 0x303 = OneMinusSrcAlpha
   | x == 0x304 = DstAlpha
   | x == 0x305 = OneMinusDstAlpha
   | x == 0x8001 = ConstantColor
   | x == 0x8002 = OneMinusConstantColor
   | x == 0x8003 = ConstantAlpha
   | x == 0x8004 = OneMinusConstantAlpha
   | x == 0x308 = SrcAlphaSaturate
   | otherwise = error ("unmarshalBlendingFactor: illegal value " ++ show x)

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
         glBlendFuncSeparateEXT (marshalBlendingFactor srcRGB)
                                (marshalBlendingFactor srcAlpha)
                                (marshalBlendingFactor dstRGB)
                                (marshalBlendingFactor dstAlpha))

EXTENSION_ENTRY("GL_EXT_blend_func_separate or OpenGL 1.4",glBlendFuncSeparateEXT,GLenum -> GLenum -> GLenum -> GLenum -> IO ())

blendFunc :: StateVar (BlendingFactor, BlendingFactor)
blendFunc =
   makeStateVar
      (liftM2 (,) (getEnum1 unmarshalBlendingFactor GetBlendSrc)
                  (getEnum1 unmarshalBlendingFactor GetBlendDst))
      (\(s, d) ->
         glBlendFunc (marshalBlendingFactor s) (marshalBlendingFactor d))

foreign import CALLCONV unsafe "glBlendFunc" glBlendFunc ::
   GLenum -> GLenum -> IO ()

blendColor :: StateVar (Color4 GLclampf)
blendColor =
   makeStateVar
      (getFloat4 Color4 GetBlendColor)
      (\(Color4 r g b a) -> glBlendColorEXT r g b a)

EXTENSION_ENTRY("GL_EXT_blend_color or OpenGL 1.4",glBlendColorEXT,GLclampf -> GLclampf -> GLclampf -> GLclampf -> IO ())

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
   Clear -> 0x1500
   And -> 0x1501
   AndReverse -> 0x1502
   Copy -> 0x1503
   AndInverted -> 0x1504
   Noop -> 0x1505
   Xor -> 0x1506
   Or -> 0x1507
   Nor -> 0x1508
   Equiv -> 0x1509
   Invert -> 0x150a
   OrReverse -> 0x150b
   CopyInverted -> 0x150c
   OrInverted -> 0x150d
   Nand -> 0x150e
   Set -> 0x150f

unmarshalLogicOp :: GLenum -> LogicOp
unmarshalLogicOp x
   | x == 0x1500 = Clear
   | x == 0x1501 = And
   | x == 0x1502 = AndReverse
   | x == 0x1503 = Copy
   | x == 0x1504 = AndInverted
   | x == 0x1505 = Noop
   | x == 0x1506 = Xor
   | x == 0x1507 = Or
   | x == 0x1508 = Nor
   | x == 0x1509 = Equiv
   | x == 0x150a = Invert
   | x == 0x150b = OrReverse
   | x == 0x150c = CopyInverted
   | x == 0x150d = OrInverted
   | x == 0x150e = Nand
   | x == 0x150f = Set
   | otherwise = error ("unmarshalLogicOp: illegal value " ++ show x)

--------------------------------------------------------------------------------

logicOp :: StateVar (Maybe LogicOp)
logicOp =
   makeStateVarMaybe
      (do rgba <- get rgbaMode
          return $ if rgba then CapColorLogicOp else CapIndexLogicOp)
      (getEnum1 unmarshalLogicOp GetLogicOpMode)
      (glLogicOp . marshalLogicOp)

foreign import CALLCONV unsafe "glLogicOp" glLogicOp :: GLenum -> IO ()
