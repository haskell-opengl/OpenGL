-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Capability
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for handling OpenGL capabilities, i.e.
-- boolean state variables.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Capability (
   Capability(..), marshalCapability, unmarshalCapability,
   EnableCap(..), makeCapability, makeStateVarMaybe,

   IndexedEnableCap(..), makeIndexedCapability,
) where

import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (
   gl_ALPHA_TEST, gl_AUTO_NORMAL, gl_COLOR_ARRAY, gl_COLOR_MATERIAL,
   gl_COLOR_SUM, gl_COLOR_TABLE, gl_CONVOLUTION_1D, gl_CONVOLUTION_2D,
   gl_EDGE_FLAG_ARRAY, gl_FOG, gl_FOG_COORD_ARRAY, gl_HISTOGRAM, gl_INDEX_ARRAY,
   gl_INDEX_LOGIC_OP, gl_LIGHTING, gl_LINE_STIPPLE, gl_MAP1_COLOR_4,
   gl_MAP1_INDEX, gl_MAP1_NORMAL, gl_MAP1_TEXTURE_COORD_1,
   gl_MAP1_TEXTURE_COORD_2, gl_MAP1_TEXTURE_COORD_3, gl_MAP1_TEXTURE_COORD_4,
   gl_MAP1_VERTEX_3, gl_MAP1_VERTEX_4, gl_MAP2_COLOR_4, gl_MAP2_INDEX,
   gl_MAP2_NORMAL, gl_MAP2_TEXTURE_COORD_1, gl_MAP2_TEXTURE_COORD_2,
   gl_MAP2_TEXTURE_COORD_3, gl_MAP2_TEXTURE_COORD_4, gl_MAP2_VERTEX_3,
   gl_MAP2_VERTEX_4, gl_MINMAX, gl_NORMALIZE, gl_NORMAL_ARRAY, gl_POINT_SMOOTH,
   gl_POINT_SPRITE, gl_POLYGON_STIPPLE, gl_POST_COLOR_MATRIX_COLOR_TABLE,
   gl_POST_CONVOLUTION_COLOR_TABLE, gl_RESCALE_NORMAL, gl_SECONDARY_COLOR_ARRAY,
   gl_SEPARABLE_2D, gl_TEXTURE_COORD_ARRAY, gl_TEXTURE_GEN_Q, gl_TEXTURE_GEN_R,
   gl_TEXTURE_GEN_S, gl_TEXTURE_GEN_T, gl_VERTEX_ARRAY,
   gl_VERTEX_PROGRAM_TWO_SIDE )
import Graphics.Rendering.OpenGL.Raw.ARB.MatrixPalette ( gl_MATRIX_INDEX_ARRAY, gl_MATRIX_PALETTE )
import Graphics.Rendering.OpenGL.Raw.ARB.VertexBlend ( gl_WEIGHT_SUM_UNITY, gl_VERTEX_BLEND, gl_WEIGHT_ARRAY )
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.EXT.DepthBoundsTest ( gl_DEPTH_BOUNDS_TEST )
import Graphics.Rendering.OpenGL.Raw.EXT.SharedTexturePalette ( gl_SHARED_TEXTURE_PALETTE )
import Graphics.Rendering.OpenGL.Raw.EXT.StencilTwoSide ( gl_STENCIL_TEST_TWO_SIDE )
import Graphics.Rendering.OpenGL.Raw.NV.DepthClamp ( gl_DEPTH_CLAMP )

--------------------------------------------------------------------------------

data Capability =
     Disabled
   | Enabled
   deriving ( Eq, Ord, Show )

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
   | CapTextureRectangle
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
   | CapPrimitiveRestart
   | CapPointSprite
   | CapStencilTestTwoSide
   | CapRasterPositionUnclipped
   | CapRasterizerDiscard
   | CapTextureColorTable
   | CapVertexProgramPointSize
   | CapVertexProgramTwoSide

marshalEnableCap :: EnableCap -> Maybe GLenum
marshalEnableCap x = case x of
   CapFog -> Just gl_FOG
   CapLighting -> Just gl_LIGHTING
   CapTexture1D -> Just gl_TEXTURE_1D
   CapTexture2D -> Just gl_TEXTURE_2D
   CapTextureRectangle -> Just gl_TEXTURE_RECTANGLE
   CapLineStipple -> Just gl_LINE_STIPPLE
   CapPolygonStipple -> Just gl_POLYGON_STIPPLE
   CapCullFace -> Just gl_CULL_FACE
   CapAlphaTest -> Just gl_ALPHA_TEST
   CapBlend -> Just gl_BLEND
   CapIndexLogicOp -> Just gl_INDEX_LOGIC_OP
   CapColorLogicOp -> Just gl_COLOR_LOGIC_OP
   CapDither -> Just gl_DITHER
   CapStencilTest -> Just gl_STENCIL_TEST
   CapDepthTest -> Just gl_DEPTH_TEST
   CapClipPlane i -> clipPlaneIndexToEnum i
   CapLight i -> lightIndexToEnum i
   CapTextureGenS -> Just gl_TEXTURE_GEN_S
   CapTextureGenT -> Just gl_TEXTURE_GEN_T
   CapTextureGenR -> Just gl_TEXTURE_GEN_R
   CapTextureGenQ -> Just gl_TEXTURE_GEN_Q
   CapMap1Vertex3 -> Just gl_MAP1_VERTEX_3
   CapMap1Vertex4 -> Just gl_MAP1_VERTEX_4
   CapMap1Color4 -> Just gl_MAP1_COLOR_4
   CapMap1Index -> Just gl_MAP1_INDEX
   CapMap1Normal -> Just gl_MAP1_NORMAL
   CapMap1TextureCoord1 -> Just gl_MAP1_TEXTURE_COORD_1
   CapMap1TextureCoord2 -> Just gl_MAP1_TEXTURE_COORD_2
   CapMap1TextureCoord3 -> Just gl_MAP1_TEXTURE_COORD_3
   CapMap1TextureCoord4 -> Just gl_MAP1_TEXTURE_COORD_4
   CapMap2Vertex3 -> Just gl_MAP2_VERTEX_3
   CapMap2Vertex4 -> Just gl_MAP2_VERTEX_4
   CapMap2Color4 -> Just gl_MAP2_COLOR_4
   CapMap2Index -> Just gl_MAP2_INDEX
   CapMap2Normal -> Just gl_MAP2_NORMAL
   CapMap2TextureCoord1 -> Just gl_MAP2_TEXTURE_COORD_1
   CapMap2TextureCoord2 -> Just gl_MAP2_TEXTURE_COORD_2
   CapMap2TextureCoord3 -> Just gl_MAP2_TEXTURE_COORD_3
   CapMap2TextureCoord4 -> Just gl_MAP2_TEXTURE_COORD_4
   CapPointSmooth -> Just gl_POINT_SMOOTH
   CapLineSmooth -> Just gl_LINE_SMOOTH
   CapPolygonSmooth -> Just gl_POLYGON_SMOOTH
   CapScissorTest -> Just gl_SCISSOR_TEST
   CapColorMaterial -> Just gl_COLOR_MATERIAL
   CapNormalize -> Just gl_NORMALIZE
   CapAutoNormal -> Just gl_AUTO_NORMAL
   CapPolygonOffsetPoint -> Just gl_POLYGON_OFFSET_POINT
   CapPolygonOffsetLine -> Just gl_POLYGON_OFFSET_LINE
   CapPolygonOffsetFill -> Just gl_POLYGON_OFFSET_FILL
   CapVertexArray -> Just gl_VERTEX_ARRAY
   CapNormalArray -> Just gl_NORMAL_ARRAY
   CapColorArray -> Just gl_COLOR_ARRAY
   CapIndexArray -> Just gl_INDEX_ARRAY
   CapTextureCoordArray -> Just gl_TEXTURE_COORD_ARRAY
   CapEdgeFlagArray -> Just gl_EDGE_FLAG_ARRAY
   CapFogCoordArray -> Just gl_FOG_COORD_ARRAY
   CapSecondaryColorArray -> Just gl_SECONDARY_COLOR_ARRAY
   CapMatrixIndexArray -> Just gl_MATRIX_INDEX_ARRAY
   CapConvolution1D -> Just gl_CONVOLUTION_1D
   CapConvolution2D -> Just gl_CONVOLUTION_2D
   CapSeparable2D -> Just gl_SEPARABLE_2D
   CapHistogram -> Just gl_HISTOGRAM
   CapMinmax -> Just gl_MINMAX
   CapRescaleNormal -> Just gl_RESCALE_NORMAL
   CapSharedTexturePalette -> Just gl_SHARED_TEXTURE_PALETTE
   CapTexture3D -> Just gl_TEXTURE_3D
   CapMultisample -> Just gl_MULTISAMPLE
   CapSampleAlphaToCoverage -> Just gl_SAMPLE_ALPHA_TO_COVERAGE
   CapSampleAlphaToOne -> Just gl_SAMPLE_ALPHA_TO_ONE
   CapSampleCoverage -> Just gl_SAMPLE_COVERAGE
   CapColorTable -> Just gl_COLOR_TABLE
   CapPostConvolutionColorTable -> Just gl_POST_CONVOLUTION_COLOR_TABLE
   CapPostColorMatrixColorTable -> Just gl_POST_COLOR_MATRIX_COLOR_TABLE
   CapColorSum -> Just gl_COLOR_SUM
   CapTextureCubeMap -> Just gl_TEXTURE_CUBE_MAP
   CapWeightSumUnity -> Just gl_WEIGHT_SUM_UNITY
   CapVertexBlend -> Just gl_VERTEX_BLEND
   CapWeightArray -> Just gl_WEIGHT_ARRAY
   CapMatrixPalette -> Just gl_MATRIX_PALETTE
   CapDepthClamp -> Just gl_DEPTH_CLAMP
   CapDepthBoundsTest -> Just gl_DEPTH_BOUNDS_TEST
   CapPrimitiveRestart -> Just gl_PRIMITIVE_RESTART
   CapPointSprite -> Just gl_POINT_SPRITE
   CapStencilTestTwoSide -> Just gl_STENCIL_TEST_TWO_SIDE
   -- TODO: use RASTER_POSITION_UNCLIPPED_IBM from IBM_rasterpos_clip extension
   CapRasterPositionUnclipped -> Just 0x19262
   CapRasterizerDiscard -> Just gl_RASTERIZER_DISCARD
   -- TODO: use TEXTURE_COLOR_TABLE_SGI from SGI_texture_color_table extension
   CapTextureColorTable -> Just 0x80bc
   CapVertexProgramPointSize -> Just gl_VERTEX_PROGRAM_POINT_SIZE
   CapVertexProgramTwoSide -> Just gl_VERTEX_PROGRAM_TWO_SIDE

--------------------------------------------------------------------------------

makeCapability :: EnableCap -> StateVar Capability
makeCapability cap = makeStateVar (isEnabled cap) (enable cap)

--------------------------------------------------------------------------------

isEnabled :: EnableCap -> IO Capability
isEnabled =
   maybe (do recordInvalidEnum; return Disabled)
         (fmap unmarshalCapability . glIsEnabled) .
   marshalEnableCap

--------------------------------------------------------------------------------

enable :: EnableCap -> Capability -> IO ()
enable cap state = maybe recordInvalidEnum (f state) (marshalEnableCap cap)
   where f Disabled = glDisable
         f Enabled  = glEnable

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

--------------------------------------------------------------------------------

data IndexedEnableCap =
   BlendI

marshalIndexedEnableCap :: IndexedEnableCap -> Maybe GLenum
marshalIndexedEnableCap x = case x of
   BlendI -> Just gl_BLEND

makeIndexedCapability ::(a -> GLuint) -> IndexedEnableCap ->  a
   -> StateVar Capability
makeIndexedCapability f cap val = makeStateVar
   (isIndexedEnabled (f val) cap)
   (\state -> enableIndexed (f val) cap state)

isIndexedEnabled :: GLuint -> IndexedEnableCap -> IO Capability
isIndexedEnabled i =
   maybe (do recordInvalidEnum; return Disabled)
         (\cap -> fmap unmarshalCapability $ glIsEnabledi cap i) .
   marshalIndexedEnableCap

enableIndexed :: GLuint -> IndexedEnableCap -> Capability -> IO ()
enableIndexed i cap state =
   maybe recordInvalidEnum (f state) (marshalIndexedEnableCap cap)
      where f Enabled  = \c -> glEnablei c i
            f Disabled = \c -> glDisablei c i
