-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Capability
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for handling OpenGL capabilities, i.e.
-- boolean state variables.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Capability (
   Capability(..), marshalCapability, unmarshalCapability,
   EnableCap(..), makeCapability, makeStateVarMaybe
) where

import Control.Monad ( liftM )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLsizei, Capability(..) )
import Graphics.Rendering.OpenGL.GL.GLboolean (
   GLboolean, marshalGLboolean, unmarshalGLboolean )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   clipPlaneIndexToEnum, lightIndexToEnum )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasGetter(get), HasSetter(($=)), StateVar, makeStateVar )

--------------------------------------------------------------------------------

marshalCapability :: Capability -> GLboolean
marshalCapability = marshalGLboolean . (Enabled ==)

unmarshalCapability :: GLboolean -> Capability
unmarshalCapability x = if unmarshalGLboolean x then Enabled else Disabled

--------------------------------------------------------------------------------

data EnableCap =
     CapFog
   | CapLighting
   | CapTexture1D
   | CapTexture2D
   | CapLineStipple
   | CapPolygonStipple
   | CapCullFace
   | CapAlphaTest
   | CapBlend
   | CapIndexLogicOp
   | CapColorLogicOp
   | CapDither
   | CapStencilTest
   | CapDepthTest
   | CapClipPlane GLsizei
   | CapLight GLsizei
   | CapTextureGenS
   | CapTextureGenT
   | CapTextureGenR
   | CapTextureGenQ
   | CapMap1Vertex3
   | CapMap1Vertex4
   | CapMap1Color4
   | CapMap1Index
   | CapMap1Normal
   | CapMap1TextureCoord1
   | CapMap1TextureCoord2
   | CapMap1TextureCoord3
   | CapMap1TextureCoord4
   | CapMap2Vertex3
   | CapMap2Vertex4
   | CapMap2Color4
   | CapMap2Index
   | CapMap2Normal
   | CapMap2TextureCoord1
   | CapMap2TextureCoord2
   | CapMap2TextureCoord3
   | CapMap2TextureCoord4
   | CapPointSmooth
   | CapLineSmooth
   | CapPolygonSmooth
   | CapScissorTest
   | CapColorMaterial
   | CapNormalize
   | CapAutoNormal
   | CapPolygonOffsetPoint
   | CapPolygonOffsetLine
   | CapPolygonOffsetFill
   | CapVertexArray
   | CapNormalArray
   | CapColorArray
   | CapIndexArray
   | CapTextureCoordArray
   | CapEdgeFlagArray
   | CapFogCoordArray
   | CapSecondaryColorArray
   | CapMatrixIndexArray
   | CapConvolution1D
   | CapConvolution2D
   | CapSeparable2D
   | CapHistogram
   | CapMinmax
   | CapRescaleNormal
   | CapSharedTexturePalette
   | CapTexture3D
   | CapMultisample
   | CapSampleAlphaToCoverage
   | CapSampleAlphaToOne
   | CapSampleCoverage
   | CapColorTable
   | CapPostConvolutionColorTable
   | CapPostColorMatrixColorTable
   | CapColorSum
   | CapTextureCubeMap
   | CapWeightSumUnity
   | CapVertexBlend
   | CapWeightArray
   | CapMatrixPalette
   | CapDepthClamp
   | CapDepthBoundsTest
   | CapPrimitiveRestart -- NOTE: client state!
   | CapPointSprite
   | CapStencilTestTwoSide

marshalEnableCap :: EnableCap -> GLenum
marshalEnableCap x = case x of
   CapFog -> 0xb60
   CapLighting -> 0xb50
   CapTexture1D -> 0xde0
   CapTexture2D -> 0xde1
   CapLineStipple -> 0xb24
   CapPolygonStipple -> 0xb42
   CapCullFace -> 0xb44
   CapAlphaTest -> 0xbc0
   CapBlend -> 0xbe2
   CapIndexLogicOp -> 0xbf1
   CapColorLogicOp -> 0xbf2
   CapDither -> 0xbd0
   CapStencilTest -> 0xb90
   CapDepthTest -> 0xb71
   CapClipPlane i -> clipPlaneIndexToEnum i
   CapLight i -> lightIndexToEnum i
   CapTextureGenS -> 0xc60
   CapTextureGenT -> 0xc61
   CapTextureGenR -> 0xc62
   CapTextureGenQ -> 0xc63
   CapMap1Vertex3 -> 0xd97
   CapMap1Vertex4 -> 0xd98
   CapMap1Color4 -> 0xd90
   CapMap1Index -> 0xd91
   CapMap1Normal -> 0xd92
   CapMap1TextureCoord1 -> 0xd93
   CapMap1TextureCoord2 -> 0xd94
   CapMap1TextureCoord3 -> 0xd95
   CapMap1TextureCoord4 -> 0xd96
   CapMap2Vertex3 -> 0xdb7
   CapMap2Vertex4 -> 0xdb8
   CapMap2Color4 -> 0xdb0
   CapMap2Index -> 0xdb1
   CapMap2Normal -> 0xdb2
   CapMap2TextureCoord1 -> 0xdb3
   CapMap2TextureCoord2 -> 0xdb4
   CapMap2TextureCoord3 -> 0xdb5
   CapMap2TextureCoord4 -> 0xdb6
   CapPointSmooth -> 0xb10
   CapLineSmooth -> 0xb20
   CapPolygonSmooth -> 0xb41
   CapScissorTest -> 0xc11
   CapColorMaterial -> 0xb57
   CapNormalize -> 0xba1
   CapAutoNormal -> 0xd80
   CapPolygonOffsetPoint -> 0x2a01
   CapPolygonOffsetLine -> 0x2a02
   CapPolygonOffsetFill -> 0x8037
   CapVertexArray -> 0x8074
   CapNormalArray -> 0x8075
   CapColorArray -> 0x8076
   CapIndexArray -> 0x8077
   CapTextureCoordArray -> 0x8078
   CapEdgeFlagArray -> 0x8079
   CapFogCoordArray -> 0x8457
   CapSecondaryColorArray -> 0x845e
   CapMatrixIndexArray -> 0x8844
   CapConvolution1D -> 0x8010
   CapConvolution2D -> 0x8011
   CapSeparable2D -> 0x8012
   CapHistogram -> 0x8024
   CapMinmax -> 0x802e
   CapRescaleNormal -> 0x803a
   CapSharedTexturePalette -> 0x81fb
   CapTexture3D -> 0x806f
   CapMultisample -> 0x809d
   CapSampleAlphaToCoverage -> 0x809e
   CapSampleAlphaToOne -> 0x809f
   CapSampleCoverage -> 0x80a0
   CapColorTable -> 0x80d0
   CapPostConvolutionColorTable -> 0x80d1
   CapPostColorMatrixColorTable -> 0x80d2
   CapColorSum -> 0x8458
   CapTextureCubeMap -> 0x8513
   CapWeightSumUnity -> 0x86a6
   CapVertexBlend -> 0x86a7
   CapWeightArray -> 0x86ad
   CapMatrixPalette -> 0x8840
   CapDepthClamp -> 0x864f
   CapDepthBoundsTest -> 0x8890
   CapPrimitiveRestart -> 0x8558
   CapPointSprite -> 0x8861
   CapStencilTestTwoSide -> 0x8910

--------------------------------------------------------------------------------

makeCapability :: EnableCap -> StateVar Capability
makeCapability cap = makeStateVar (isEnabled cap) (enable cap)

--------------------------------------------------------------------------------

isEnabled :: EnableCap -> IO Capability
isEnabled = liftM unmarshalCapability . glIsEnabled . marshalEnableCap

foreign import CALLCONV unsafe "glIsEnabled" glIsEnabled ::
   GLenum -> IO GLboolean

--------------------------------------------------------------------------------

enable :: EnableCap -> Capability -> IO ()
enable cap Disabled = glDisable (marshalEnableCap cap)
enable cap Enabled  = glEnable  (marshalEnableCap cap)

foreign import CALLCONV unsafe "glEnable" glEnable :: GLenum -> IO ()

foreign import CALLCONV unsafe "glDisable" glDisable :: GLenum -> IO ()

--------------------------------------------------------------------------------

makeStateVarMaybe :: IO EnableCap -> IO a -> (a -> IO ()) -> StateVar (Maybe a)
makeStateVarMaybe getCap getAct setAct =
   makeStateVar
      (getStateVarMaybe getCap getAct)
      (setStateVarMaybe getCap setAct)

getStateVarMaybe :: IO EnableCap -> IO a -> IO (Maybe a)
getStateVarMaybe getCap act = do
   capability <- liftM makeCapability getCap
   state <- get capability
   if state == Enabled
      then liftM Just act
      else return Nothing

setStateVarMaybe :: IO EnableCap -> (a -> IO ()) -> Maybe a -> IO ()
setStateVarMaybe getCap act val = do
   capability <- liftM makeCapability getCap
   maybe (capability $= Disabled) (\x -> capability $= Enabled >> act x) val
