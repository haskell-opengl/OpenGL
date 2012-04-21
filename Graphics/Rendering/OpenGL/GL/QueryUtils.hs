-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.QueryUtils
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module with utilities to query OpenGL state.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.QueryUtils (
   module Graphics.Rendering.OpenGL.GL.QueryUtils.PName,
   module Graphics.Rendering.OpenGL.GL.QueryUtils.VertexAttrib,

   lightIndexToEnum,
   modelviewIndexToEnum, modelviewEnumToIndex,

   maybeNullPtr,
) where

import Foreign.Ptr
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (
   gl_LIGHT0, gl_MODELVIEW)
import Graphics.Rendering.OpenGL.Raw.ARB.VertexBlend (
   gl_MODELVIEW1, gl_MODELVIEW2, gl_MODELVIEW31 )
import Graphics.Rendering.OpenGL.Raw.Core32

import Graphics.Rendering.OpenGL.GL.QueryUtils.PName
import Graphics.Rendering.OpenGL.GL.QueryUtils.VertexAttrib

--------------------------------------------------------------------------------

--data GetPNameO =
--     GetCurrentMatrixIndex
--   | GetPointSmooth
--   | GetPointSizeRange
--   | GetPointSizeGranularity
--   | GetLineSmooth
--   | GetLineWidthRange
--   | GetLineWidthGranularity
--   | GetLineStipple
--   | GetPolygonSmooth
--   | GetPolygonStipple
--   | GetCullFace
--   | GetLighting
--   | GetFog
--   | GetDepthTest
--   | GetStencilTest
--   | GetNormalize
--   | GetAttribStackDepth
--   | GetClientAttribStackDepth
--   | GetAlphaTest
--   | GetDither
--   | GetBlend
--   | GetIndexLogicOp
--   | GetLogicOp
--   | GetColorLogicOp
--   | GetScissorTest
--   | GetIndexMode
--   | GetTextureGenS
--   | GetTextureGenT
--   | GetTextureGenR
--   | GetTextureGenQ
--   | GetMaxClipDistances
--   | GetMaxAttribStackDepth
--   | GetMaxClientAttribStackDepth
--   | GetIndexBits
--   | GetAutoNormal
--   | GetMap1Color4
--   | GetMap1Index
--   | GetMap1Normal
--   | GetMap1TextureCoord1
--   | GetMap1TextureCoord2
--   | GetMap1TextureCoord3
--   | GetMap1TextureCoord4
--   | GetMap1Vertex3
--   | GetMap1Vertex4
--   | GetMap2Color4
--   | GetMap2Index
--   | GetMap2Normal
--   | GetMap2TextureCoord1
--   | GetMap2TextureCoord2
--   | GetMap2TextureCoord3
--   | GetMap2TextureCoord4
--   | GetMap2Vertex3
--   | GetMap2Vertex4
--   | GetTexture1D
--   | GetTexture2D
--   | GetFeedbackBufferSize
--   | GetFeedbackBufferType
--   | GetSelectionBufferSize
--   | GetPolygonOffsetPoint
--   | GetPolygonOffsetLine
--   | GetPolygonOffsetFill
--   | GetVertexArray
--   | GetNormalArray
--   | GetColorArray
--   | GetIndexArray
--   | GetTextureCoordArray
--   | GetEdgeFlagArray
--   | GetFogCoordArray
--   | GetSecondaryColorArray
--   | GetMatrixIndexArray
--   | GetMatrixIndexArraySize
--   | GetMatrixIndexArrayType
--   | GetMatrixIndexArrayStride
--   | GetClipDistance GLsizei
--   | GetLight GLsizei
--   | GetTransposeModelviewMatrix
--   | GetTransposeProjectionMatrix
--   | GetTransposeTextureMatrix
--   | GetTransposeColorMatrix
--   | GetColorTable
--   | GetPostConvolutionColorTable
--   | GetPostColorMatrixColorTable
--   | GetConvolution1D
--   | GetConvolution2D
--   | GetSeparable2D
--   | GetMaxConvolutionWidth
--   | GetMaxConvolutionHeight
--   | GetHistogram
--   | GetMinmax
--   | GetColorSum
--   | GetRescaleNormal
--   | GetSharedTexturePalette
--   | GetTexture3DBinding
--   | GetTexture3D
--   | GetMultisample
--   | GetSampleAlphaToCoverage
--   | GetSampleAlphaToOne
--   | GetSampleCoverage
--   | GetTextureCubeMap
--   | GetMaxVertexUnits
--   | GetActiveVertexUnits
--   | GetWeightSumUnity
--   | GetVertexBlend
--   | GetModelview GLsizei
--   | GetCurrentWeight
--   | GetWeightArrayType
--   | GetWeightArrayStride
--   | GetWeightArraySize
--   | GetWeightArray
--   | GetMaxPaletteMatrices
--   | GetCurrentPaletteMatrix
--   | GetCurrentMatrix
--   | GetMaxVaryingComponents
--   | GetColorMaterial
--   -- GetWeightArrayBufferBinding
--   -- transform feedback stuff
--   -- FramebufferObject
--   -- RenderbufferObject
--   -- Color clamping
--   -- VertexArrayObject
--
--_marshalGetPNameO :: GetPNameO -> Maybe GLenum
--_marshalGetPNameO x = case x of
--   GetCurrentMatrixIndex -> Just gl_CURRENT_MATRIX_INDEX
--   GetPointSmooth -> Just gl_POINT_SMOOTH
--   GetPointSizeRange -> Just gl_POINT_SIZE_RANGE
--   GetPointSizeGranularity -> Just gl_POINT_SIZE_GRANULARITY
--   GetLineSmooth -> Just gl_LINE_SMOOTH
--   GetLineWidthRange -> Just gl_SMOOTH_LINE_WIDTH_RANGE
--   GetLineWidthGranularity -> Just gl_SMOOTH_LINE_WIDTH_GRANULARITY
--   GetLineStipple -> Just gl_LINE_STIPPLE
--   GetPolygonSmooth -> Just gl_POLYGON_SMOOTH
--   GetPolygonStipple -> Just gl_POLYGON_STIPPLE
--   GetCullFace -> Just gl_CULL_FACE
--   GetLighting -> Just gl_LIGHTING
--   GetFog -> Just gl_FOG
--   GetDepthTest -> Just gl_DEPTH_TEST
--   GetStencilTest -> Just gl_STENCIL_TEST
--   GetPolygonOffsetPoint -> Just gl_POLYGON_OFFSET_POINT
--   GetNormalize -> Just gl_NORMALIZE
--   GetAttribStackDepth -> Just gl_ATTRIB_STACK_DEPTH
--   GetClientAttribStackDepth -> Just gl_CLIENT_ATTRIB_STACK_DEPTH
--   GetAlphaTest -> Just gl_ALPHA_TEST
--   GetDither -> Just gl_DITHER
--   GetBlend -> Just gl_BLEND
--   GetIndexLogicOp -> Just gl_INDEX_LOGIC_OP
--   GetLogicOp -> Just gl_INDEX_LOGIC_OP
--   GetColorLogicOp -> Just gl_COLOR_LOGIC_OP
--   GetScissorTest -> Just gl_SCISSOR_TEST
--   GetIndexMode -> Just gl_INDEX_MODE
--   GetTextureGenS -> Just gl_TEXTURE_GEN_S
--   GetTextureGenT -> Just gl_TEXTURE_GEN_T
--   GetTextureGenR -> Just gl_TEXTURE_GEN_R
--   GetTextureGenQ -> Just gl_TEXTURE_GEN_Q
--   GetMaxClipDistances -> Just gl_MAX_CLIP_DISTANCES
--   GetMaxAttribStackDepth -> Just gl_MAX_ATTRIB_STACK_DEPTH
--   GetMaxClientAttribStackDepth -> Just gl_MAX_CLIENT_ATTRIB_STACK_DEPTH
--   GetIndexBits -> Just gl_INDEX_BITS
--   GetAutoNormal -> Just gl_AUTO_NORMAL
--   GetMap1Color4 -> Just gl_MAP1_COLOR_4
--   GetMap1Index -> Just gl_MAP1_INDEX
--   GetMap1Normal -> Just gl_MAP1_NORMAL
--   GetMap1TextureCoord1 -> Just gl_MAP1_TEXTURE_COORD_1
--   GetMap1TextureCoord2 -> Just gl_MAP1_TEXTURE_COORD_2
--   GetMap1TextureCoord3 -> Just gl_MAP1_TEXTURE_COORD_3
--   GetMap1TextureCoord4 -> Just gl_MAP1_TEXTURE_COORD_4
--   GetMap1Vertex3 -> Just gl_MAP1_VERTEX_3
--   GetMap1Vertex4 -> Just gl_MAP1_VERTEX_4
--   GetMap2Color4 -> Just gl_MAP2_COLOR_4
--   GetMap2Index -> Just gl_MAP2_INDEX
--   GetMap2Normal -> Just gl_MAP2_NORMAL
--   GetMap2TextureCoord1 -> Just gl_MAP2_TEXTURE_COORD_1
--   GetMap2TextureCoord2 -> Just gl_MAP2_TEXTURE_COORD_2
--   GetMap2TextureCoord3 -> Just gl_MAP2_TEXTURE_COORD_3
--   GetMap2TextureCoord4 -> Just gl_MAP2_TEXTURE_COORD_4
--   GetMap2Vertex3 -> Just gl_MAP2_VERTEX_3
--   GetMap2Vertex4 -> Just gl_MAP2_VERTEX_4
--   GetTexture1D -> Just gl_TEXTURE_1D
--   GetTexture2D -> Just gl_TEXTURE_2D
--   GetFeedbackBufferSize -> Just gl_FEEDBACK_BUFFER_SIZE
--   GetFeedbackBufferType -> Just gl_FEEDBACK_BUFFER_TYPE
--   GetSelectionBufferSize -> Just gl_SELECTION_BUFFER_SIZE
--   GetPolygonOffsetLine -> Just gl_POLYGON_OFFSET_LINE
--   GetPolygonOffsetFill -> Just gl_POLYGON_OFFSET_FILL
--   GetVertexArray -> Just gl_VERTEX_ARRAY
--   GetNormalArray -> Just gl_NORMAL_ARRAY
--   GetColorArray -> Just gl_COLOR_ARRAY
--   GetIndexArray -> Just gl_INDEX_ARRAY
--   GetTextureCoordArray -> Just gl_TEXTURE_COORD_ARRAY
--   GetEdgeFlagArray -> Just gl_EDGE_FLAG_ARRAY
--   GetFogCoordArray -> Just gl_FOG_COORD_ARRAY
--   GetSecondaryColorArray -> Just gl_SECONDARY_COLOR_ARRAY
--   GetMatrixIndexArray -> Just gl_MATRIX_INDEX_ARRAY
--   GetMatrixIndexArraySize -> Just gl_MATRIX_INDEX_ARRAY_SIZE
--   GetMatrixIndexArrayType -> Just gl_MATRIX_INDEX_ARRAY_TYPE
--   GetMatrixIndexArrayStride -> Just gl_MATRIX_INDEX_ARRAY_STRIDE
--   GetClipDistance i -> clipPlaneIndexToEnum i
--   GetLight i -> lightIndexToEnum i
--   GetTransposeModelviewMatrix -> Just gl_TRANSPOSE_MODELVIEW_MATRIX
--   GetTransposeProjectionMatrix -> Just gl_TRANSPOSE_PROJECTION_MATRIX
--   GetTransposeTextureMatrix -> Just gl_TRANSPOSE_TEXTURE_MATRIX
--   GetTransposeColorMatrix -> Just gl_TRANSPOSE_COLOR_MATRIX
--   GetColorTable -> Just gl_COLOR_TABLE
--   GetPostConvolutionColorTable -> Just gl_POST_CONVOLUTION_COLOR_TABLE
--   GetPostColorMatrixColorTable -> Just gl_POST_COLOR_MATRIX_COLOR_TABLE
--   GetConvolution1D -> Just gl_CONVOLUTION_1D
--   GetConvolution2D -> Just gl_CONVOLUTION_2D
--   GetSeparable2D -> Just gl_SEPARABLE_2D
--   GetMaxConvolutionWidth -> Just gl_MAX_CONVOLUTION_WIDTH
--   GetMaxConvolutionHeight -> Just gl_MAX_CONVOLUTION_HEIGHT
--   GetHistogram -> Just gl_HISTOGRAM
--   GetMinmax -> Just gl_MINMAX
--   GetColorSum -> Just gl_COLOR_SUM
--   GetRescaleNormal -> Just gl_RESCALE_NORMAL
--   GetSharedTexturePalette -> Just gl_SHARED_TEXTURE_PALETTE
--   GetTexture3DBinding -> Just gl_TEXTURE_BINDING_3D
--   GetTexture3D -> Just gl_TEXTURE_3D
--   GetMultisample -> Just gl_MULTISAMPLE
--   GetSampleAlphaToCoverage -> Just gl_SAMPLE_ALPHA_TO_COVERAGE
--   GetSampleAlphaToOne -> Just gl_SAMPLE_ALPHA_TO_ONE
--   GetSampleCoverage -> Just gl_SAMPLE_COVERAGE
--   GetColorMaterial -> Just gl_COLOR_MATERIAL
--   GetTextureCubeMap -> Just gl_TEXTURE_CUBE_MAP
--
--   GetMaxVertexUnits -> Just gl_MAX_VERTEX_UNITS
--   GetActiveVertexUnits -> Just gl_ACTIVE_VERTEX_UNITS
--   GetWeightSumUnity -> Just gl_WEIGHT_SUM_UNITY
--   GetVertexBlend -> Just gl_VERTEX_BLEND
--   GetModelview i -> modelviewIndexToEnum i
--   GetCurrentWeight -> Just gl_CURRENT_WEIGHT
--   GetWeightArrayType -> Just gl_WEIGHT_ARRAY_TYPE
--   GetWeightArrayStride -> Just gl_WEIGHT_ARRAY_STRIDE
--   GetWeightArraySize -> Just gl_WEIGHT_ARRAY_SIZE
--   GetWeightArray -> Just gl_WEIGHT_ARRAY
--   GetMaxPaletteMatrices -> Just gl_MAX_PALETTE_MATRICES
--   GetCurrentPaletteMatrix -> Just gl_CURRENT_PALETTE_MATRIX
--   GetCurrentMatrix -> Just gl_CURRENT_MATRIX
--   GetMaxVaryingComponents -> Just gl_MAX_VARYING_COMPONENTS

   -- GetWeightArrayBufferBinding -> Just gl_WEIGHT_ARRAY_BUFFER_BINDING
   -- transform feedback
   -- FramebufferObject
   -- RenderbufferObject
   -- Color clamping
     -- VertexArrayObject

--------------------------------------------------------------------------------

--data GetIndexedPName =
--     GetTransformFeedbackBuffer
--   | GetTransformFeedbackBufferStart
--   | GetTransformFeedbackBufferSize
--
--marshalGetIndexedPName :: GetIndexedPName -> GLenum
--marshalGetIndexedPName x = case x of
--   GetTransformFeedbackBuffer -> gl_TRANSFORM_FEEDBACK_BUFFER
--   GetTransformFeedbackBufferSize -> gl_TRANSFORM_FEEDBACK_BUFFER_SIZE
--   GetTransformFeedbackBufferStart -> gl_TRANSFORM_FEEDBACK_BUFFER_START

--------------------------------------------------------------------------------

-- 0x4000 through 0x4FFF are reserved for light numbers

lightIndexToEnum :: GLsizei -> Maybe GLenum
lightIndexToEnum i
   | 0 <= i && i <= maxLightIndex = Just (gl_LIGHT0 + fromIntegral i)
   | otherwise = Nothing

maxLightIndex :: GLsizei
maxLightIndex = 0xFFF

--------------------------------------------------------------------------------

-- 0x1700, 0x850a, and 0x8722 through 0x873f are reserved for modelview matrices

modelviewIndexToEnum :: GLsizei -> Maybe GLenum
modelviewIndexToEnum 0 = Just gl_MODELVIEW
modelviewIndexToEnum 1 = Just gl_MODELVIEW1
modelviewIndexToEnum i
   | 2 <= i && i <= 31 = Just (gl_MODELVIEW2 - 2 + fromIntegral i)
   | otherwise = Nothing

modelviewEnumToIndex :: GLenum -> Maybe GLsizei
modelviewEnumToIndex x
   | x == gl_MODELVIEW = Just 0
   | x == gl_MODELVIEW1 = Just 1
   | gl_MODELVIEW2 <= x && x <= gl_MODELVIEW31 = Just (fromIntegral (x - (gl_MODELVIEW2 - 2)))
   | otherwise = Nothing



--------------------------------------------------------------------------------

maybeNullPtr :: b -> (Ptr a -> b) -> Ptr a -> b
maybeNullPtr n f ptr | ptr == nullPtr = n
                     | otherwise      = f ptr
--------------------------------------------------------------------------------
