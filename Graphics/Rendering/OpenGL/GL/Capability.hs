-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Capability
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for handling OpenGL capabilities, i.e.
-- boolean state variables.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Capability (
   marshalCapability, unmarshalCapability,
   EnableCap(..), makeCapability, makeStateVarMaybe
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLboolean, GLenum, GLsizei, Capability(..) )
import Graphics.Rendering.OpenGL.GL.GLboolean (
   marshalGLboolean, unmarshalGLboolean )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   clipPlaneIndexToEnum, lightIndexToEnum )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasGetter(get), HasSetter(($=)), StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal ( recordInvalidEnum )

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
   | CapRasterPositionUnclipped
   | CapTextureColorTable

marshalEnableCap :: EnableCap -> Maybe GLenum
marshalEnableCap x = case x of
   CapFog -> Just 0xb60
   CapLighting -> Just 0xb50
   CapTexture1D -> Just 0xde0
   CapTexture2D -> Just 0xde1
   CapLineStipple -> Just 0xb24
   CapPolygonStipple -> Just 0xb42
   CapCullFace -> Just 0xb44
   CapAlphaTest -> Just 0xbc0
   CapBlend -> Just 0xbe2
   CapIndexLogicOp -> Just 0xbf1
   CapColorLogicOp -> Just 0xbf2
   CapDither -> Just 0xbd0
   CapStencilTest -> Just 0xb90
   CapDepthTest -> Just 0xb71
   CapClipPlane i -> clipPlaneIndexToEnum i
   CapLight i -> lightIndexToEnum i
   CapTextureGenS -> Just 0xc60
   CapTextureGenT -> Just 0xc61
   CapTextureGenR -> Just 0xc62
   CapTextureGenQ -> Just 0xc63
   CapMap1Vertex3 -> Just 0xd97
   CapMap1Vertex4 -> Just 0xd98
   CapMap1Color4 -> Just 0xd90
   CapMap1Index -> Just 0xd91
   CapMap1Normal -> Just 0xd92
   CapMap1TextureCoord1 -> Just 0xd93
   CapMap1TextureCoord2 -> Just 0xd94
   CapMap1TextureCoord3 -> Just 0xd95
   CapMap1TextureCoord4 -> Just 0xd96
   CapMap2Vertex3 -> Just 0xdb7
   CapMap2Vertex4 -> Just 0xdb8
   CapMap2Color4 -> Just 0xdb0
   CapMap2Index -> Just 0xdb1
   CapMap2Normal -> Just 0xdb2
   CapMap2TextureCoord1 -> Just 0xdb3
   CapMap2TextureCoord2 -> Just 0xdb4
   CapMap2TextureCoord3 -> Just 0xdb5
   CapMap2TextureCoord4 -> Just 0xdb6
   CapPointSmooth -> Just 0xb10
   CapLineSmooth -> Just 0xb20
   CapPolygonSmooth -> Just 0xb41
   CapScissorTest -> Just 0xc11
   CapColorMaterial -> Just 0xb57
   CapNormalize -> Just 0xba1
   CapAutoNormal -> Just 0xd80
   CapPolygonOffsetPoint -> Just 0x2a01
   CapPolygonOffsetLine -> Just 0x2a02
   CapPolygonOffsetFill -> Just 0x8037
   CapVertexArray -> Just 0x8074
   CapNormalArray -> Just 0x8075
   CapColorArray -> Just 0x8076
   CapIndexArray -> Just 0x8077
   CapTextureCoordArray -> Just 0x8078
   CapEdgeFlagArray -> Just 0x8079
   CapFogCoordArray -> Just 0x8457
   CapSecondaryColorArray -> Just 0x845e
   CapMatrixIndexArray -> Just 0x8844
   CapConvolution1D -> Just 0x8010
   CapConvolution2D -> Just 0x8011
   CapSeparable2D -> Just 0x8012
   CapHistogram -> Just 0x8024
   CapMinmax -> Just 0x802e
   CapRescaleNormal -> Just 0x803a
   CapSharedTexturePalette -> Just 0x81fb
   CapTexture3D -> Just 0x806f
   CapMultisample -> Just 0x809d
   CapSampleAlphaToCoverage -> Just 0x809e
   CapSampleAlphaToOne -> Just 0x809f
   CapSampleCoverage -> Just 0x80a0
   CapColorTable -> Just 0x80d0
   CapPostConvolutionColorTable -> Just 0x80d1
   CapPostColorMatrixColorTable -> Just 0x80d2
   CapColorSum -> Just 0x8458
   CapTextureCubeMap -> Just 0x8513
   CapWeightSumUnity -> Just 0x86a6
   CapVertexBlend -> Just 0x86a7
   CapWeightArray -> Just 0x86ad
   CapMatrixPalette -> Just 0x8840
   CapDepthClamp -> Just 0x864f
   CapDepthBoundsTest -> Just 0x8890
   CapPrimitiveRestart -> Just 0x8558
   CapPointSprite -> Just 0x8861
   CapStencilTestTwoSide -> Just 0x8910
   CapRasterPositionUnclipped -> Just 0x19262
   CapTextureColorTable -> Just 0x80bc

--------------------------------------------------------------------------------

makeCapability :: EnableCap -> StateVar Capability
makeCapability cap = makeStateVar (isEnabled cap) (enable cap)

--------------------------------------------------------------------------------

isEnabled :: EnableCap -> IO Capability
isEnabled =
   maybe (do recordInvalidEnum; return Disabled)
         (fmap unmarshalCapability . glIsEnabled) .
   marshalEnableCap

foreign import CALLCONV unsafe "glIsEnabled" glIsEnabled ::
   GLenum -> IO GLboolean

--------------------------------------------------------------------------------

enable :: EnableCap -> Capability -> IO ()
enable cap state = maybe recordInvalidEnum (f state) (marshalEnableCap cap)
   where f Disabled = glDisable
         f Enabled  = glEnable

foreign import CALLCONV unsafe "glDisable" glDisable :: GLenum -> IO ()

foreign import CALLCONV unsafe "glEnable" glEnable :: GLenum -> IO ()

--------------------------------------------------------------------------------

makeStateVarMaybe :: IO EnableCap -> IO a -> (a -> IO ()) -> StateVar (Maybe a)
makeStateVarMaybe getCap getAct setAct =
   makeStateVar
      (getStateVarMaybe getCap getAct)
      (setStateVarMaybe getCap setAct)

getStateVarMaybe :: IO EnableCap -> IO a -> IO (Maybe a)
getStateVarMaybe getCap act = do
   capability <- fmap makeCapability getCap
   state <- get capability
   if state == Enabled
      then fmap Just act
      else return Nothing

setStateVarMaybe :: IO EnableCap -> (a -> IO ()) -> Maybe a -> IO ()
setStateVarMaybe getCap act val = do
   capability <- fmap makeCapability getCap
   maybe (capability $= Disabled) (\x -> act x >> capability $= Enabled) val
