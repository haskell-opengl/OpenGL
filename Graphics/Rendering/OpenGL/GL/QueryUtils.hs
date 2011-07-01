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
   GetPName(..),
   GetIndexedPName(..), marshalGetIndexedPName,
   clipPlaneIndexToEnum, lightIndexToEnum,
   modelviewIndexToEnum, modelviewEnumToIndex,
   getBoolean1, getBoolean4,
   getBoolean4i, getBooleanvi,
   getInteger1, getInteger2, getInteger4, getIntegerv,
   getInteger1i, getIntegeriv,
   getEnum1,
   getSizei1, getSizei2,
   getFloat1, getFloat2, getFloat3, getFloat4, getFloatv,
   getClampf1, getClampf4,
   getDouble1, getDouble2, getDouble4, getDoublev,
   getClampd1, getClampd2,
   maybeNullPtr,
   AttribLocation(..), GetVertexAttribPName(..),
   getVertexAttribInteger1, getVertexAttribEnum1, getVertexAttribBoolean1,
   getVertexAttribFloat4, getVertexAttribIInteger4, getVertexAttribIuInteger4,
   GetVertexAttribPointerPName(..), getVertexAttribPointer
) where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (
   gl_ACCUM_ALPHA_BITS, gl_ACCUM_BLUE_BITS, gl_ACCUM_CLEAR_VALUE,
   gl_ACCUM_GREEN_BITS, gl_ACCUM_RED_BITS, gl_ALIASED_POINT_SIZE_RANGE,
   gl_ALPHA_BIAS, gl_ALPHA_BITS, gl_ALPHA_SCALE, gl_ALPHA_TEST,
   gl_ALPHA_TEST_FUNC, gl_ALPHA_TEST_REF, gl_ATTRIB_STACK_DEPTH, gl_AUTO_NORMAL,
   gl_AUX_BUFFERS, gl_BLUE_BIAS, gl_BLUE_BITS, gl_BLUE_SCALE,
   gl_CLAMP_FRAGMENT_COLOR, gl_CLAMP_VERTEX_COLOR,
   gl_CLIENT_ACTIVE_TEXTURE, gl_CLIENT_ATTRIB_STACK_DEPTH, gl_COLOR_ARRAY,
   gl_COLOR_ARRAY_BUFFER_BINDING, gl_COLOR_ARRAY_SIZE, gl_COLOR_ARRAY_STRIDE,
   gl_COLOR_ARRAY_TYPE, gl_COLOR_MATERIAL, gl_COLOR_MATERIAL_FACE,
   gl_COLOR_MATERIAL_PARAMETER, gl_COLOR_MATRIX, gl_COLOR_MATRIX_STACK_DEPTH,
   gl_COLOR_SUM, gl_COLOR_TABLE, gl_CONVOLUTION_1D, gl_CONVOLUTION_2D,
   gl_CURRENT_COLOR, gl_CURRENT_FOG_COORD, gl_CURRENT_INDEX, gl_CURRENT_NORMAL,
   gl_CURRENT_RASTER_COLOR, gl_CURRENT_RASTER_DISTANCE, gl_CURRENT_RASTER_INDEX,
   gl_CURRENT_RASTER_POSITION, gl_CURRENT_RASTER_POSITION_VALID,
   gl_CURRENT_RASTER_SECONDARY_COLOR, gl_CURRENT_RASTER_TEXTURE_COORDS,
   gl_CURRENT_SECONDARY_COLOR, gl_CURRENT_TEXTURE_COORDS, gl_DEPTH_BIAS,
   gl_DEPTH_BITS, gl_DEPTH_SCALE, gl_EDGE_FLAG, gl_EDGE_FLAG_ARRAY,
   gl_EDGE_FLAG_ARRAY_BUFFER_BINDING, gl_EDGE_FLAG_ARRAY_STRIDE,
   gl_FEEDBACK_BUFFER_SIZE, gl_FEEDBACK_BUFFER_TYPE, gl_FOG, gl_FOG_COLOR,
   gl_FOG_COORD_ARRAY, gl_FOG_COORD_ARRAY_BUFFER_BINDING,
   gl_FOG_COORD_ARRAY_STRIDE, gl_FOG_COORD_ARRAY_TYPE, gl_FOG_COORD_SRC,
   gl_FOG_DENSITY, gl_FOG_END, gl_FOG_HINT, gl_FOG_INDEX, gl_FOG_MODE,
   gl_FOG_START, gl_GENERATE_MIPMAP_HINT, gl_GREEN_BIAS, gl_GREEN_BITS,
   gl_GREEN_SCALE, gl_HISTOGRAM, gl_INDEX_ARRAY, gl_INDEX_ARRAY_BUFFER_BINDING,
   gl_INDEX_ARRAY_STRIDE, gl_INDEX_ARRAY_TYPE, gl_INDEX_BITS,
   gl_INDEX_CLEAR_VALUE, gl_INDEX_LOGIC_OP, gl_INDEX_MODE, gl_INDEX_OFFSET,
   gl_INDEX_SHIFT, gl_INDEX_WRITEMASK, gl_LIGHT0, gl_LIGHTING,
   gl_LIGHT_MODEL_AMBIENT, gl_LIGHT_MODEL_COLOR_CONTROL,
   gl_LIGHT_MODEL_LOCAL_VIEWER, gl_LIGHT_MODEL_TWO_SIDE, gl_LINE_STIPPLE,
   gl_LINE_STIPPLE_PATTERN, gl_LINE_STIPPLE_REPEAT, gl_LIST_BASE, gl_LIST_INDEX,
   gl_LIST_MODE, gl_MAP1_COLOR_4, gl_MAP1_GRID_DOMAIN, gl_MAP1_GRID_SEGMENTS,
   gl_MAP1_INDEX, gl_MAP1_NORMAL, gl_MAP1_TEXTURE_COORD_1,
   gl_MAP1_TEXTURE_COORD_2, gl_MAP1_TEXTURE_COORD_3, gl_MAP1_TEXTURE_COORD_4,
   gl_MAP1_VERTEX_3, gl_MAP1_VERTEX_4, gl_MAP2_COLOR_4, gl_MAP2_GRID_DOMAIN,
   gl_MAP2_GRID_SEGMENTS, gl_MAP2_INDEX, gl_MAP2_NORMAL,
   gl_MAP2_TEXTURE_COORD_1, gl_MAP2_TEXTURE_COORD_2, gl_MAP2_TEXTURE_COORD_3,
   gl_MAP2_TEXTURE_COORD_4, gl_MAP2_VERTEX_3, gl_MAP2_VERTEX_4, gl_MAP_COLOR,
   gl_MAP_STENCIL, gl_MATRIX_MODE, gl_MAX_ATTRIB_STACK_DEPTH,
   gl_MAX_CLIENT_ATTRIB_STACK_DEPTH, gl_MAX_COLOR_MATRIX_STACK_DEPTH,
   gl_MAX_CONVOLUTION_HEIGHT, gl_MAX_CONVOLUTION_WIDTH, gl_MAX_EVAL_ORDER,
   gl_MAX_LIGHTS, gl_MAX_LIST_NESTING, gl_MAX_MODELVIEW_STACK_DEPTH,
   gl_MAX_NAME_STACK_DEPTH, gl_MAX_PIXEL_MAP_TABLE,
   gl_MAX_PROJECTION_STACK_DEPTH, gl_MAX_TEXTURE_COORDS,
   gl_MAX_TEXTURE_STACK_DEPTH, gl_MAX_TEXTURE_UNITS, gl_MINMAX, gl_MODELVIEW,
   gl_MODELVIEW_MATRIX, gl_MODELVIEW_STACK_DEPTH, gl_NAME_STACK_DEPTH,
   gl_NORMALIZE, gl_NORMAL_ARRAY, gl_NORMAL_ARRAY_BUFFER_BINDING,
   gl_NORMAL_ARRAY_STRIDE, gl_NORMAL_ARRAY_TYPE, gl_PERSPECTIVE_CORRECTION_HINT,
   gl_PIXEL_MAP_A_TO_A_SIZE, gl_PIXEL_MAP_B_TO_B_SIZE, gl_PIXEL_MAP_G_TO_G_SIZE,
   gl_PIXEL_MAP_I_TO_A_SIZE, gl_PIXEL_MAP_I_TO_B_SIZE, gl_PIXEL_MAP_I_TO_G_SIZE,
   gl_PIXEL_MAP_I_TO_I_SIZE, gl_PIXEL_MAP_I_TO_R_SIZE, gl_PIXEL_MAP_R_TO_R_SIZE,
   gl_PIXEL_MAP_S_TO_S_SIZE, gl_POINT_DISTANCE_ATTENUATION, gl_POINT_SIZE_MAX,
   gl_POINT_SIZE_MIN, gl_POINT_SMOOTH, gl_POINT_SMOOTH_HINT, gl_POLYGON_MODE,
   gl_POLYGON_STIPPLE, gl_POST_COLOR_MATRIX_ALPHA_BIAS,
   gl_POST_COLOR_MATRIX_ALPHA_SCALE, gl_POST_COLOR_MATRIX_BLUE_BIAS,
   gl_POST_COLOR_MATRIX_BLUE_SCALE, gl_POST_COLOR_MATRIX_COLOR_TABLE,
   gl_POST_COLOR_MATRIX_GREEN_BIAS, gl_POST_COLOR_MATRIX_GREEN_SCALE,
   gl_POST_COLOR_MATRIX_RED_BIAS, gl_POST_COLOR_MATRIX_RED_SCALE,
   gl_POST_CONVOLUTION_ALPHA_BIAS, gl_POST_CONVOLUTION_ALPHA_SCALE,
   gl_POST_CONVOLUTION_BLUE_BIAS, gl_POST_CONVOLUTION_BLUE_SCALE,
   gl_POST_CONVOLUTION_COLOR_TABLE, gl_POST_CONVOLUTION_GREEN_BIAS,
   gl_POST_CONVOLUTION_GREEN_SCALE, gl_POST_CONVOLUTION_RED_BIAS,
   gl_POST_CONVOLUTION_RED_SCALE, gl_PROJECTION_MATRIX,
   gl_PROJECTION_STACK_DEPTH, gl_RED_BIAS, gl_RED_BITS, gl_RED_SCALE,
   gl_RENDER_MODE, gl_RESCALE_NORMAL, gl_RGBA_MODE, gl_SECONDARY_COLOR_ARRAY,
   gl_SECONDARY_COLOR_ARRAY_BUFFER_BINDING, gl_SECONDARY_COLOR_ARRAY_SIZE,
   gl_SECONDARY_COLOR_ARRAY_STRIDE, gl_SECONDARY_COLOR_ARRAY_TYPE,
   gl_SELECTION_BUFFER_SIZE, gl_SEPARABLE_2D, gl_SHADE_MODEL, gl_STENCIL_BITS,
   gl_TEXTURE_COORD_ARRAY, gl_TEXTURE_COORD_ARRAY_BUFFER_BINDING,
   gl_TEXTURE_COORD_ARRAY_SIZE, gl_TEXTURE_COORD_ARRAY_STRIDE,
   gl_TEXTURE_COORD_ARRAY_TYPE, gl_TEXTURE_GEN_Q, gl_TEXTURE_GEN_R,
   gl_TEXTURE_GEN_S, gl_TEXTURE_GEN_T, gl_TEXTURE_MATRIX,
   gl_TEXTURE_STACK_DEPTH, gl_TRANSPOSE_COLOR_MATRIX,
   gl_TRANSPOSE_MODELVIEW_MATRIX, gl_TRANSPOSE_PROJECTION_MATRIX,
   gl_TRANSPOSE_TEXTURE_MATRIX, gl_VERTEX_ARRAY, gl_VERTEX_ARRAY_BUFFER_BINDING,
   gl_VERTEX_ARRAY_SIZE, gl_VERTEX_ARRAY_STRIDE, gl_VERTEX_ARRAY_TYPE,
   gl_ZOOM_X, gl_ZOOM_Y )
import Graphics.Rendering.OpenGL.Raw.ARB.FragmentProgram (
   gl_CURRENT_MATRIX, gl_CURRENT_MATRIX_STACK_DEPTH )
import Graphics.Rendering.OpenGL.Raw.ARB.MatrixPalette (
   gl_CURRENT_MATRIX_INDEX, gl_CURRENT_PALETTE_MATRIX, gl_MATRIX_INDEX_ARRAY,
   gl_MATRIX_INDEX_ARRAY_SIZE, gl_MATRIX_INDEX_ARRAY_STRIDE,
   gl_MATRIX_INDEX_ARRAY_TYPE, gl_MATRIX_PALETTE,
   gl_MAX_MATRIX_PALETTE_STACK_DEPTH, gl_MAX_PALETTE_MATRICES )
import Graphics.Rendering.OpenGL.Raw.ARB.VertexBlend (
   gl_ACTIVE_VERTEX_UNITS, gl_CURRENT_WEIGHT, gl_MAX_VERTEX_UNITS,
   gl_MODELVIEW1, gl_MODELVIEW2, gl_MODELVIEW31, gl_VERTEX_BLEND,
   gl_WEIGHT_ARRAY, gl_WEIGHT_ARRAY_SIZE, gl_WEIGHT_ARRAY_STRIDE,
   gl_WEIGHT_ARRAY_TYPE, gl_WEIGHT_SUM_UNITY )
import Graphics.Rendering.OpenGL.Raw.Core32
import Graphics.Rendering.OpenGL.Raw.EXT ( gl_RGBA_SIGNED_COMPONENTS )
import Graphics.Rendering.OpenGL.Raw.EXT.Cmyka ( gl_PACK_CMYK_HINT, gl_UNPACK_CMYK_HINT )
import Graphics.Rendering.OpenGL.Raw.EXT.CompiledVertexArray ( gl_ARRAY_ELEMENT_LOCK_FIRST, gl_ARRAY_ELEMENT_LOCK_COUNT )
import Graphics.Rendering.OpenGL.Raw.EXT.DepthBoundsTest ( gl_DEPTH_BOUNDS, )
import Graphics.Rendering.OpenGL.Raw.EXT.SharedTexturePalette ( gl_SHARED_TEXTURE_PALETTE )
import Graphics.Rendering.OpenGL.Raw.EXT.StencilTwoSide ( gl_ACTIVE_STENCIL_FACE )
import Graphics.Rendering.OpenGL.Raw.EXT.TextureFilterAnisotropic ( gl_MAX_TEXTURE_MAX_ANISOTROPY )
import Graphics.Rendering.OpenGL.Raw.NV.FogDistance ( gl_FOG_DISTANCE_MODE )
import Graphics.Rendering.OpenGL.Raw.NV.LightMaxExponent (  gl_MAX_SHININESS, gl_MAX_SPOT_EXPONENT )
import Graphics.Rendering.OpenGL.Raw.NV.PrimitiveRestart ( gl_PRIMITIVE_RESTART_INDEX_NV, gl_PRIMITIVE_RESTART_NV )

--------------------------------------------------------------------------------

data GetPName =
     GetCurrentColor
   | GetCurrentIndex
   | GetCurrentNormal
   | GetCurrentTextureCoords
   | GetCurrentRasterColor
   | GetCurrentRasterSecondaryColor
   | GetCurrentRasterIndex
   | GetCurrentRasterTextureCoords
   | GetCurrentRasterPosition
   | GetCurrentRasterPositionValid
   | GetCurrentRasterDistance
   | GetCurrentMatrixIndex
   | GetPointSmooth
   | GetPointSize
   | GetPointSizeRange
   | GetPointSizeGranularity
   | GetLineSmooth
   | GetLineWidth
   | GetLineWidthRange
   | GetLineWidthGranularity
   | GetLineStipple
   | GetLineStipplePattern
   | GetLineStippleRepeat
   | GetSmoothPointSizeRange
   | GetSmoothPointSizeGranularity
   | GetSmoothLineWidthRange
   | GetSmoothLineWidthGranularity
   | GetAliasedPointSizeRange
   | GetAliasedLineWidthRange
   | GetListMode
   | GetMaxListNesting
   | GetListBase
   | GetListIndex
   | GetPolygonMode
   | GetPolygonSmooth
   | GetPolygonStipple
   | GetEdgeFlag
   | GetCullFace
   | GetCullFaceMode
   | GetFrontFace
   | GetLighting
   | GetLightModelLocalViewer
   | GetLightModelTwoSide
   | GetLightModelAmbient
   | GetShadeModel
   | GetColorMaterialFace
   | GetColorMaterialParameter
   | GetColorMaterial
   | GetFog
   | GetFogIndex
   | GetFogDensity
   | GetFogStart
   | GetFogEnd
   | GetFogMode
   | GetFogColor
   | GetFogCoordSrc
   | GetCurrentFogCoord
   | GetDepthRange
   | GetDepthTest
   | GetDepthWritemask
   | GetDepthClearValue
   | GetDepthFunc
   | GetAccumClearValue
   | GetStencilTest
   | GetStencilClearValue
   | GetStencilFunc
   | GetStencilValueMask
   | GetStencilFail
   | GetStencilPassDepthFail
   | GetStencilPassDepthPass
   | GetStencilRef
   | GetStencilWritemask
   | GetMatrixMode
   | GetNormalize
   | GetViewport
   | GetModelviewStackDepth
   | GetProjectionStackDepth
   | GetTextureStackDepth
   | GetModelviewMatrix
   | GetProjectionMatrix
   | GetTextureMatrix
   | GetAttribStackDepth
   | GetClientAttribStackDepth
   | GetAlphaTest
   | GetAlphaTestFunc
   | GetAlphaTestRef
   | GetDither
   | GetBlendDst
   | GetBlendSrc
   | GetBlend
   | GetLogicOpMode
   | GetIndexLogicOp
   | GetLogicOp
   | GetColorLogicOp
   | GetAuxBuffers
   | GetDrawBuffer
   | GetReadBuffer
   | GetScissorBox
   | GetScissorTest
   | GetIndexClearValue
   | GetIndexWritemask
   | GetColorClearValue
   | GetColorWritemask
   | GetIndexMode
   | GetRGBAMode
   | GetDoublebuffer
   | GetStereo
   | GetRenderMode
   | GetPerspectiveCorrectionHint
   | GetPointSmoothHint
   | GetLineSmoothHint
   | GetPolygonSmoothHint
   | GetFogHint
   | GetGenerateMipmapHint
   | GetTextureCompressionHint
   | GetTextureGenS
   | GetTextureGenT
   | GetTextureGenR
   | GetTextureGenQ
   | GetPixelMapIToISize
   | GetPixelMapSToSSize
   | GetPixelMapIToRSize
   | GetPixelMapIToGSize
   | GetPixelMapIToBSize
   | GetPixelMapIToASize
   | GetPixelMapRToRSize
   | GetPixelMapGToGSize
   | GetPixelMapBToBSize
   | GetPixelMapAToASize
   | GetUnpackSwapBytes
   | GetUnpackLSBFirst
   | GetUnpackRowLength
   | GetUnpackSkipRows
   | GetUnpackSkipPixels
   | GetUnpackAlignment
   | GetPackSwapBytes
   | GetPackLSBFirst
   | GetPackRowLength
   | GetPackSkipRows
   | GetPackSkipPixels
   | GetPackAlignment
   | GetMapColor
   | GetMapStencil
   | GetIndexShift
   | GetIndexOffset
   | GetRedScale
   | GetRedBias
   | GetZoomX
   | GetZoomY
   | GetGreenScale
   | GetGreenBias
   | GetBlueScale
   | GetBlueBias
   | GetAlphaScale
   | GetAlphaBias
   | GetDepthScale
   | GetDepthBias
   | GetMaxEvalOrder
   | GetMaxLights
   | GetMaxClipPlanes
   | GetMaxClipDistances
   | GetMaxTextureSize
   | GetMaxPixelMapTable
   | GetMaxAttribStackDepth
   | GetMaxModelviewStackDepth
   | GetMaxNameStackDepth
   | GetMaxProjectionStackDepth
   | GetMaxTextureStackDepth
   | GetMaxViewportDims
   | GetMaxClientAttribStackDepth
   | GetSubpixelBits
   | GetIndexBits
   | GetRedBits
   | GetGreenBits
   | GetBlueBits
   | GetAlphaBits
   | GetDepthBits
   | GetStencilBits
   | GetAccumRedBits
   | GetAccumGreenBits
   | GetAccumBlueBits
   | GetAccumAlphaBits
   | GetNameStackDepth
   | GetAutoNormal
   | GetMap1Color4
   | GetMap1Index
   | GetMap1Normal
   | GetMap1TextureCoord1
   | GetMap1TextureCoord2
   | GetMap1TextureCoord3
   | GetMap1TextureCoord4
   | GetMap1Vertex3
   | GetMap1Vertex4
   | GetMap2Color4
   | GetMap2Index
   | GetMap2Normal
   | GetMap2TextureCoord1
   | GetMap2TextureCoord2
   | GetMap2TextureCoord3
   | GetMap2TextureCoord4
   | GetMap2Vertex3
   | GetMap2Vertex4
   | GetMap1GridDomain
   | GetMap1GridSegments
   | GetMap2GridDomain
   | GetMap2GridSegments
   | GetTexture1D
   | GetTexture2D
   | GetFeedbackBufferSize
   | GetFeedbackBufferType
   | GetSelectionBufferSize
   | GetPolygonOffsetUnits
   | GetPolygonOffsetPoint
   | GetPolygonOffsetLine
   | GetPolygonOffsetFill
   | GetPolygonOffsetFactor
   | GetTextureBinding1D
   | GetTextureBinding2D
   | GetTextureBinding3D
   | GetVertexArray
   | GetNormalArray
   | GetColorArray
   | GetIndexArray
   | GetTextureCoordArray
   | GetEdgeFlagArray
   | GetFogCoordArray
   | GetSecondaryColorArray
   | GetMatrixIndexArray
   | GetVertexArraySize
   | GetVertexArrayType
   | GetVertexArrayStride
   | GetNormalArrayType
   | GetNormalArrayStride
   | GetColorArraySize
   | GetColorArrayType
   | GetColorArrayStride
   | GetIndexArrayType
   | GetIndexArrayStride
   | GetTextureCoordArraySize
   | GetTextureCoordArrayType
   | GetTextureCoordArrayStride
   | GetEdgeFlagArrayStride
   | GetFogCoordArrayType
   | GetFogCoordArrayStride
   | GetSecondaryColorArraySize
   | GetSecondaryColorArrayType
   | GetSecondaryColorArrayStride
   | GetMatrixIndexArraySize
   | GetMatrixIndexArrayType
   | GetMatrixIndexArrayStride
   | GetClipPlane GLsizei
   | GetClipDistance GLsizei
   | GetLight GLsizei
   | GetTransposeModelviewMatrix
   | GetTransposeProjectionMatrix
   | GetTransposeTextureMatrix
   | GetTransposeColorMatrix
   | GetLightModelColorControl
   | GetBlendColor
   | GetBlendEquation
   | GetBlendEquationAlpha
   | GetColorTable
   | GetPostConvolutionColorTable
   | GetPostColorMatrixColorTable
   | GetConvolution1D
   | GetConvolution2D
   | GetSeparable2D
   | GetMaxConvolutionWidth
   | GetMaxConvolutionHeight
   | GetPostConvolutionRedScale
   | GetPostConvolutionGreenScale
   | GetPostConvolutionBlueScale
   | GetPostConvolutionAlphaScale
   | GetPostConvolutionRedBias
   | GetPostConvolutionGreenBias
   | GetPostConvolutionBlueBias
   | GetPostConvolutionAlphaBias
   | GetHistogram
   | GetMinmax
   | GetColorSum
   | GetCurrentSecondaryColor
   | GetRescaleNormal
   | GetSharedTexturePalette
   | GetTexture3DBinding
   | GetPackSkipImages
   | GetPackImageHeight
   | GetUnpackSkipImages
   | GetUnpackImageHeight
   | GetTexture3D
   | GetMax3DTextureSize
   | GetMaxTextureLODBias
   | GetMaxTextureMaxAnisotropy
   | GetMultisample
   | GetSampleAlphaToCoverage
   | GetSampleAlphaToOne
   | GetSampleCoverage
   | GetSampleBuffers
   | GetSamples
   | GetSampleCoverageValue
   | GetSampleCoverageInvert
   | GetPointSizeMin
   | GetPointSizeMax
   | GetPointFadeThresholdSize
   | GetPointDistanceAttenuation
   | GetColorMatrix
   | GetColorMatrixStackDepth
   | GetMaxColorMatrixStackDepth
   | GetPostColorMatrixRedScale
   | GetPostColorMatrixGreenScale
   | GetPostColorMatrixBlueScale
   | GetPostColorMatrixAlphaScale
   | GetPostColorMatrixRedBias
   | GetPostColorMatrixGreenBias
   | GetPostColorMatrixBlueBias
   | GetPostColorMatrixAlphaBias
   | GetMaxElementsVertices
   | GetMaxElementsIndices
   | GetActiveTexture
   | GetClientActiveTexture
   | GetMaxTextureUnits
   | GetTextureCubeMap
   | GetMaxCubeMapTextureSize
   | GetMaxRectangleTextureSize
   | GetNumCompressedTextureFormats
   | GetCompressedTextureFormats
   | GetMaxVertexUnits
   | GetActiveVertexUnits
   | GetWeightSumUnity
   | GetVertexBlend
   | GetModelview GLsizei
   | GetCurrentWeight
   | GetWeightArrayType
   | GetWeightArrayStride
   | GetWeightArraySize
   | GetWeightArray
   | GetMatrixPalette
   | GetMaxMatrixPaletteStackDepth
   | GetMaxPaletteMatrices
   | GetCurrentPaletteMatrix
   | GetBlendDstRGB
   | GetBlendSrcRGB
   | GetBlendDstAlpha
   | GetBlendSrcAlpha
   | GetPackCMYKHint
   | GetUnpackCMYKHint
   | GetArrayElementLockFirst
   | GetArrayElementLockCount
   | GetMaxShininess
   | GetMaxSpotExponent
   | GetFogDistanceMode
   | GetDepthBounds
   | GetPrimitiveRestartIndex
   | GetPrimitiveRestartNV
   | GetPrimitiveRestartIndexNV
   | GetActiveStencilFace
   | GetArrayBufferBinding
   | GetElementArrayBufferBinding
   | GetVertexArrayBufferBinding
   | GetNormalArrayBufferBinding
   | GetColorArrayBufferBinding
   | GetIndexArrayBufferBinding
   | GetTextureCoordArrayBufferBinding
   | GetEdgeFlagArrayBufferBinding
   | GetSecondaryColorArrayBufferBinding
   | GetFogCoordArrayBufferBinding
   | GetTextureBindingCubeMap
   | GetTextureBindingRectangle
   | GetCurrentMatrix
   | GetCurrentMatrixStackDepth
   | GetMaxCombinedTextureImageUnits
   | GetMaxDrawBuffers
   | GetMaxFragmentUniformComponents
   | GetMaxTextureCoords
   | GetMaxTextureImageUnits
   | GetMaxVaryingComponents
   | GetMaxVaryingFloats
   | GetMaxVertexAttribs
   | GetMaxVertexTextureImageUnits
   | GetMaxVertexUniformComponents
   | GetCurrentProgram
   | GetPixelPackBufferBinding
   | GetPixelUnpackBufferBinding
   | GetDrawBufferN GLsizei
   | GetRGBASignedComponents
   | GetCopyReadBuffer
   | GetCopyWriteBuffer
   -- GetWeightArrayBufferBinding
   | GetContextProfileMask
   -- transform feedback stuff
   | GetTransformFeedbackBufferBinding
   | GetMaxTransformFeedbackSeparateAttribs
   | GetMaxTransformFeedbackSeparateComponents
   | GetMaxTransformFeedbackInterleavedComponents
   -- FramebufferObject
   | GetDrawFramebufferBinding
   | GetReadFramebufferBinding
   | GetFramebufferBinding
   -- RenderbufferObject
   | GetRenderbufferBinding
   -- Color clamping
   | GetVertexColorClamp
   | GetFragmentColorClamp
   | GetReadColorClamp

marshalGetPName :: GetPName -> Maybe GLenum
marshalGetPName x = case x of
   GetCurrentColor -> Just gl_CURRENT_COLOR
   GetCurrentIndex -> Just gl_CURRENT_INDEX
   GetCurrentNormal -> Just gl_CURRENT_NORMAL
   GetCurrentTextureCoords -> Just gl_CURRENT_TEXTURE_COORDS
   GetCurrentRasterColor -> Just gl_CURRENT_RASTER_COLOR
   GetCurrentRasterSecondaryColor -> Just gl_CURRENT_RASTER_SECONDARY_COLOR
   GetCurrentRasterIndex -> Just gl_CURRENT_RASTER_INDEX
   GetCurrentRasterTextureCoords -> Just gl_CURRENT_RASTER_TEXTURE_COORDS
   GetCurrentRasterPosition -> Just gl_CURRENT_RASTER_POSITION
   GetCurrentRasterPositionValid -> Just gl_CURRENT_RASTER_POSITION_VALID
   GetCurrentRasterDistance -> Just gl_CURRENT_RASTER_DISTANCE
   GetCurrentMatrixIndex -> Just gl_CURRENT_MATRIX_INDEX
   GetPointSmooth -> Just gl_POINT_SMOOTH
   GetPointSize -> Just gl_POINT_SIZE
   GetPointSizeRange -> Just gl_POINT_SIZE_RANGE
   GetPointSizeGranularity -> Just gl_POINT_SIZE_GRANULARITY
   GetLineSmooth -> Just gl_LINE_SMOOTH
   GetLineWidth -> Just gl_LINE_WIDTH
   GetLineWidthRange -> Just gl_SMOOTH_LINE_WIDTH_RANGE
   GetLineWidthGranularity -> Just gl_SMOOTH_LINE_WIDTH_GRANULARITY
   GetLineStipple -> Just gl_LINE_STIPPLE
   GetLineStipplePattern -> Just gl_LINE_STIPPLE_PATTERN
   GetLineStippleRepeat -> Just gl_LINE_STIPPLE_REPEAT
   GetSmoothPointSizeRange -> Just gl_POINT_SIZE_RANGE
   GetSmoothPointSizeGranularity -> Just gl_POINT_SIZE_GRANULARITY
   GetSmoothLineWidthRange -> Just gl_SMOOTH_LINE_WIDTH_RANGE
   GetSmoothLineWidthGranularity -> Just gl_SMOOTH_LINE_WIDTH_GRANULARITY
   GetAliasedPointSizeRange -> Just gl_ALIASED_POINT_SIZE_RANGE
   GetAliasedLineWidthRange -> Just gl_ALIASED_LINE_WIDTH_RANGE
   GetListMode -> Just gl_LIST_MODE
   GetMaxListNesting -> Just gl_MAX_LIST_NESTING
   GetListBase -> Just gl_LIST_BASE
   GetListIndex -> Just gl_LIST_INDEX
   GetPolygonMode -> Just gl_POLYGON_MODE
   GetPolygonSmooth -> Just gl_POLYGON_SMOOTH
   GetPolygonStipple -> Just gl_POLYGON_STIPPLE
   GetEdgeFlag -> Just gl_EDGE_FLAG
   GetCullFace -> Just gl_CULL_FACE
   GetCullFaceMode -> Just gl_CULL_FACE_MODE
   GetFrontFace -> Just gl_FRONT_FACE
   GetLighting -> Just gl_LIGHTING
   GetLightModelLocalViewer -> Just gl_LIGHT_MODEL_LOCAL_VIEWER
   GetLightModelTwoSide -> Just gl_LIGHT_MODEL_TWO_SIDE
   GetLightModelAmbient -> Just gl_LIGHT_MODEL_AMBIENT
   GetShadeModel -> Just gl_SHADE_MODEL
   GetColorMaterialFace -> Just gl_COLOR_MATERIAL_FACE
   GetColorMaterialParameter -> Just gl_COLOR_MATERIAL_PARAMETER
   GetColorMaterial -> Just gl_COLOR_MATERIAL
   GetFog -> Just gl_FOG
   GetFogIndex -> Just gl_FOG_INDEX
   GetFogDensity -> Just gl_FOG_DENSITY
   GetFogStart -> Just gl_FOG_START
   GetFogEnd -> Just gl_FOG_END
   GetFogMode -> Just gl_FOG_MODE
   GetFogColor -> Just gl_FOG_COLOR
   GetFogCoordSrc -> Just gl_FOG_COORD_SRC
   GetCurrentFogCoord -> Just gl_CURRENT_FOG_COORD
   GetDepthRange -> Just gl_DEPTH_RANGE
   GetDepthTest -> Just gl_DEPTH_TEST
   GetDepthWritemask -> Just gl_DEPTH_WRITEMASK
   GetDepthClearValue -> Just gl_DEPTH_CLEAR_VALUE
   GetDepthFunc -> Just gl_DEPTH_FUNC
   GetAccumClearValue -> Just gl_ACCUM_CLEAR_VALUE
   GetStencilTest -> Just gl_STENCIL_TEST
   GetStencilClearValue -> Just gl_STENCIL_CLEAR_VALUE
   GetStencilFunc -> Just gl_STENCIL_FUNC
   GetStencilValueMask -> Just gl_STENCIL_VALUE_MASK
   GetStencilFail -> Just gl_STENCIL_FAIL
   GetStencilPassDepthFail -> Just gl_STENCIL_PASS_DEPTH_FAIL
   GetStencilPassDepthPass -> Just gl_STENCIL_PASS_DEPTH_PASS
   GetStencilRef -> Just gl_STENCIL_REF
   GetStencilWritemask -> Just gl_STENCIL_WRITEMASK
   GetMatrixMode -> Just gl_MATRIX_MODE
   GetNormalize -> Just gl_NORMALIZE
   GetViewport -> Just gl_VIEWPORT
   GetModelviewStackDepth -> Just gl_MODELVIEW_STACK_DEPTH
   GetProjectionStackDepth -> Just gl_PROJECTION_STACK_DEPTH
   GetTextureStackDepth -> Just gl_TEXTURE_STACK_DEPTH
   GetModelviewMatrix -> Just gl_MODELVIEW_MATRIX
   GetProjectionMatrix -> Just gl_PROJECTION_MATRIX
   GetTextureMatrix -> Just gl_TEXTURE_MATRIX
   GetAttribStackDepth -> Just gl_ATTRIB_STACK_DEPTH
   GetClientAttribStackDepth -> Just gl_CLIENT_ATTRIB_STACK_DEPTH
   GetAlphaTest -> Just gl_ALPHA_TEST
   GetAlphaTestFunc -> Just gl_ALPHA_TEST_FUNC
   GetAlphaTestRef -> Just gl_ALPHA_TEST_REF
   GetDither -> Just gl_DITHER
   GetBlendDst -> Just gl_BLEND_DST
   GetBlendSrc -> Just gl_BLEND_SRC
   GetBlend -> Just gl_BLEND
   GetLogicOpMode -> Just gl_LOGIC_OP_MODE
   GetIndexLogicOp -> Just gl_INDEX_LOGIC_OP
   GetLogicOp -> Just gl_INDEX_LOGIC_OP
   GetColorLogicOp -> Just gl_COLOR_LOGIC_OP
   GetAuxBuffers -> Just gl_AUX_BUFFERS
   GetDrawBuffer -> Just gl_DRAW_BUFFER
   GetReadBuffer -> Just gl_READ_BUFFER
   GetScissorBox -> Just gl_SCISSOR_BOX
   GetScissorTest -> Just gl_SCISSOR_TEST
   GetIndexClearValue -> Just gl_INDEX_CLEAR_VALUE
   GetIndexWritemask -> Just gl_INDEX_WRITEMASK
   GetColorClearValue -> Just gl_COLOR_CLEAR_VALUE
   GetColorWritemask -> Just gl_COLOR_WRITEMASK
   GetIndexMode -> Just gl_INDEX_MODE
   GetRGBAMode -> Just gl_RGBA_MODE
   GetDoublebuffer -> Just gl_DOUBLEBUFFER
   GetStereo -> Just gl_STEREO
   GetRenderMode -> Just gl_RENDER_MODE
   GetPerspectiveCorrectionHint -> Just gl_PERSPECTIVE_CORRECTION_HINT
   GetPointSmoothHint -> Just gl_POINT_SMOOTH_HINT
   GetLineSmoothHint -> Just gl_LINE_SMOOTH_HINT
   GetPolygonSmoothHint -> Just gl_POLYGON_SMOOTH_HINT
   GetFogHint -> Just gl_FOG_HINT
   GetGenerateMipmapHint -> Just gl_GENERATE_MIPMAP_HINT
   GetTextureCompressionHint -> Just gl_TEXTURE_COMPRESSION_HINT
   GetTextureGenS -> Just gl_TEXTURE_GEN_S
   GetTextureGenT -> Just gl_TEXTURE_GEN_T
   GetTextureGenR -> Just gl_TEXTURE_GEN_R
   GetTextureGenQ -> Just gl_TEXTURE_GEN_Q
   GetPixelMapIToISize -> Just gl_PIXEL_MAP_I_TO_I_SIZE
   GetPixelMapSToSSize -> Just gl_PIXEL_MAP_S_TO_S_SIZE
   GetPixelMapIToRSize -> Just gl_PIXEL_MAP_I_TO_R_SIZE
   GetPixelMapIToGSize -> Just gl_PIXEL_MAP_I_TO_G_SIZE
   GetPixelMapIToBSize -> Just gl_PIXEL_MAP_I_TO_B_SIZE
   GetPixelMapIToASize -> Just gl_PIXEL_MAP_I_TO_A_SIZE
   GetPixelMapRToRSize -> Just gl_PIXEL_MAP_R_TO_R_SIZE
   GetPixelMapGToGSize -> Just gl_PIXEL_MAP_G_TO_G_SIZE
   GetPixelMapBToBSize -> Just gl_PIXEL_MAP_B_TO_B_SIZE
   GetPixelMapAToASize -> Just gl_PIXEL_MAP_A_TO_A_SIZE
   GetUnpackSwapBytes -> Just gl_UNPACK_SWAP_BYTES
   GetUnpackLSBFirst -> Just gl_UNPACK_LSB_FIRST
   GetUnpackRowLength -> Just gl_UNPACK_ROW_LENGTH
   GetUnpackSkipRows -> Just gl_UNPACK_SKIP_ROWS
   GetUnpackSkipPixels -> Just gl_UNPACK_SKIP_PIXELS
   GetUnpackAlignment -> Just gl_UNPACK_ALIGNMENT
   GetPackSwapBytes -> Just gl_PACK_SWAP_BYTES
   GetPackLSBFirst -> Just gl_PACK_LSB_FIRST
   GetPackRowLength -> Just gl_PACK_ROW_LENGTH
   GetPackSkipRows -> Just gl_PACK_SKIP_ROWS
   GetPackSkipPixels -> Just gl_PACK_SKIP_PIXELS
   GetPackAlignment -> Just gl_PACK_ALIGNMENT
   GetMapColor -> Just gl_MAP_COLOR
   GetMapStencil -> Just gl_MAP_STENCIL
   GetIndexShift -> Just gl_INDEX_SHIFT
   GetIndexOffset -> Just gl_INDEX_OFFSET
   GetRedScale -> Just gl_RED_SCALE
   GetRedBias -> Just gl_RED_BIAS
   GetZoomX -> Just gl_ZOOM_X
   GetZoomY -> Just gl_ZOOM_Y
   GetGreenScale -> Just gl_GREEN_SCALE
   GetGreenBias -> Just gl_GREEN_BIAS
   GetBlueScale -> Just gl_BLUE_SCALE
   GetBlueBias -> Just gl_BLUE_BIAS
   GetAlphaScale -> Just gl_ALPHA_SCALE
   GetAlphaBias -> Just gl_ALPHA_BIAS
   GetDepthScale -> Just gl_DEPTH_SCALE
   GetDepthBias -> Just gl_DEPTH_BIAS
   GetMaxEvalOrder -> Just gl_MAX_EVAL_ORDER
   GetMaxLights -> Just gl_MAX_LIGHTS
   GetMaxClipPlanes -> Just gl_MAX_CLIP_DISTANCES
   GetMaxClipDistances -> Just gl_MAX_CLIP_DISTANCES
   GetMaxTextureSize -> Just gl_MAX_TEXTURE_SIZE
   GetMaxPixelMapTable -> Just gl_MAX_PIXEL_MAP_TABLE
   GetMaxAttribStackDepth -> Just gl_MAX_ATTRIB_STACK_DEPTH
   GetMaxModelviewStackDepth -> Just gl_MAX_MODELVIEW_STACK_DEPTH
   GetMaxNameStackDepth -> Just gl_MAX_NAME_STACK_DEPTH
   GetMaxProjectionStackDepth -> Just gl_MAX_PROJECTION_STACK_DEPTH
   GetMaxTextureStackDepth -> Just gl_MAX_TEXTURE_STACK_DEPTH
   GetMaxViewportDims -> Just gl_MAX_VIEWPORT_DIMS
   GetMaxClientAttribStackDepth -> Just gl_MAX_CLIENT_ATTRIB_STACK_DEPTH
   GetSubpixelBits -> Just gl_SUBPIXEL_BITS
   GetIndexBits -> Just gl_INDEX_BITS
   GetRedBits -> Just gl_RED_BITS
   GetGreenBits -> Just gl_GREEN_BITS
   GetBlueBits -> Just gl_BLUE_BITS
   GetAlphaBits -> Just gl_ALPHA_BITS
   GetDepthBits -> Just gl_DEPTH_BITS
   GetStencilBits -> Just gl_STENCIL_BITS
   GetAccumRedBits -> Just gl_ACCUM_RED_BITS
   GetAccumGreenBits -> Just gl_ACCUM_GREEN_BITS
   GetAccumBlueBits -> Just gl_ACCUM_BLUE_BITS
   GetAccumAlphaBits -> Just gl_ACCUM_ALPHA_BITS
   GetNameStackDepth -> Just gl_NAME_STACK_DEPTH
   GetAutoNormal -> Just gl_AUTO_NORMAL
   GetMap1Color4 -> Just gl_MAP1_COLOR_4
   GetMap1Index -> Just gl_MAP1_INDEX
   GetMap1Normal -> Just gl_MAP1_NORMAL
   GetMap1TextureCoord1 -> Just gl_MAP1_TEXTURE_COORD_1
   GetMap1TextureCoord2 -> Just gl_MAP1_TEXTURE_COORD_2
   GetMap1TextureCoord3 -> Just gl_MAP1_TEXTURE_COORD_3
   GetMap1TextureCoord4 -> Just gl_MAP1_TEXTURE_COORD_4
   GetMap1Vertex3 -> Just gl_MAP1_VERTEX_3
   GetMap1Vertex4 -> Just gl_MAP1_VERTEX_4
   GetMap2Color4 -> Just gl_MAP2_COLOR_4
   GetMap2Index -> Just gl_MAP2_INDEX
   GetMap2Normal -> Just gl_MAP2_NORMAL
   GetMap2TextureCoord1 -> Just gl_MAP2_TEXTURE_COORD_1
   GetMap2TextureCoord2 -> Just gl_MAP2_TEXTURE_COORD_2
   GetMap2TextureCoord3 -> Just gl_MAP2_TEXTURE_COORD_3
   GetMap2TextureCoord4 -> Just gl_MAP2_TEXTURE_COORD_4
   GetMap2Vertex3 -> Just gl_MAP2_VERTEX_3
   GetMap2Vertex4 -> Just gl_MAP2_VERTEX_4
   GetMap1GridDomain -> Just gl_MAP1_GRID_DOMAIN
   GetMap1GridSegments -> Just gl_MAP1_GRID_SEGMENTS
   GetMap2GridDomain -> Just gl_MAP2_GRID_DOMAIN
   GetMap2GridSegments -> Just gl_MAP2_GRID_SEGMENTS
   GetTexture1D -> Just gl_TEXTURE_1D
   GetTexture2D -> Just gl_TEXTURE_2D
   GetFeedbackBufferSize -> Just gl_FEEDBACK_BUFFER_SIZE
   GetFeedbackBufferType -> Just gl_FEEDBACK_BUFFER_TYPE
   GetSelectionBufferSize -> Just gl_SELECTION_BUFFER_SIZE
   GetPolygonOffsetUnits -> Just gl_POLYGON_OFFSET_UNITS
   GetPolygonOffsetPoint -> Just gl_POLYGON_OFFSET_POINT
   GetPolygonOffsetLine -> Just gl_POLYGON_OFFSET_LINE
   GetPolygonOffsetFill -> Just gl_POLYGON_OFFSET_FILL
   GetPolygonOffsetFactor -> Just gl_POLYGON_OFFSET_FACTOR
   GetTextureBinding1D -> Just gl_TEXTURE_BINDING_1D
   GetTextureBinding2D -> Just gl_TEXTURE_BINDING_2D
   GetTextureBinding3D -> Just gl_TEXTURE_BINDING_3D
   GetVertexArray -> Just gl_VERTEX_ARRAY
   GetNormalArray -> Just gl_NORMAL_ARRAY
   GetColorArray -> Just gl_COLOR_ARRAY
   GetIndexArray -> Just gl_INDEX_ARRAY
   GetTextureCoordArray -> Just gl_TEXTURE_COORD_ARRAY
   GetEdgeFlagArray -> Just gl_EDGE_FLAG_ARRAY
   GetFogCoordArray -> Just gl_FOG_COORD_ARRAY
   GetSecondaryColorArray -> Just gl_SECONDARY_COLOR_ARRAY
   GetMatrixIndexArray -> Just gl_MATRIX_INDEX_ARRAY
   GetVertexArraySize -> Just gl_VERTEX_ARRAY_SIZE
   GetVertexArrayType -> Just gl_VERTEX_ARRAY_TYPE
   GetVertexArrayStride -> Just gl_VERTEX_ARRAY_STRIDE
   GetNormalArrayType -> Just gl_NORMAL_ARRAY_TYPE
   GetNormalArrayStride -> Just gl_NORMAL_ARRAY_STRIDE
   GetColorArraySize -> Just gl_COLOR_ARRAY_SIZE
   GetColorArrayType -> Just gl_COLOR_ARRAY_TYPE
   GetColorArrayStride -> Just gl_COLOR_ARRAY_STRIDE
   GetIndexArrayType -> Just gl_INDEX_ARRAY_TYPE
   GetIndexArrayStride -> Just gl_INDEX_ARRAY_STRIDE
   GetTextureCoordArraySize -> Just gl_TEXTURE_COORD_ARRAY_SIZE
   GetTextureCoordArrayType -> Just gl_TEXTURE_COORD_ARRAY_TYPE
   GetTextureCoordArrayStride -> Just gl_TEXTURE_COORD_ARRAY_STRIDE
   GetEdgeFlagArrayStride -> Just gl_EDGE_FLAG_ARRAY_STRIDE
   GetFogCoordArrayType -> Just gl_FOG_COORD_ARRAY_TYPE
   GetFogCoordArrayStride -> Just gl_FOG_COORD_ARRAY_STRIDE
   GetSecondaryColorArraySize -> Just gl_SECONDARY_COLOR_ARRAY_SIZE
   GetSecondaryColorArrayType -> Just gl_SECONDARY_COLOR_ARRAY_TYPE
   GetSecondaryColorArrayStride -> Just gl_SECONDARY_COLOR_ARRAY_STRIDE
   GetMatrixIndexArraySize -> Just gl_MATRIX_INDEX_ARRAY_SIZE
   GetMatrixIndexArrayType -> Just gl_MATRIX_INDEX_ARRAY_TYPE
   GetMatrixIndexArrayStride -> Just gl_MATRIX_INDEX_ARRAY_STRIDE
   GetClipPlane i -> clipPlaneIndexToEnum i
   GetClipDistance i -> clipPlaneIndexToEnum i
   GetLight i -> lightIndexToEnum i
   GetTransposeModelviewMatrix -> Just gl_TRANSPOSE_MODELVIEW_MATRIX
   GetTransposeProjectionMatrix -> Just gl_TRANSPOSE_PROJECTION_MATRIX
   GetTransposeTextureMatrix -> Just gl_TRANSPOSE_TEXTURE_MATRIX
   GetTransposeColorMatrix -> Just gl_TRANSPOSE_COLOR_MATRIX
   GetLightModelColorControl -> Just gl_LIGHT_MODEL_COLOR_CONTROL
   GetBlendColor -> Just gl_BLEND_COLOR
   GetBlendEquation -> Just gl_BLEND_EQUATION_RGB
   GetBlendEquationAlpha -> Just gl_BLEND_EQUATION_ALPHA
   GetColorTable -> Just gl_COLOR_TABLE
   GetPostConvolutionColorTable -> Just gl_POST_CONVOLUTION_COLOR_TABLE
   GetPostColorMatrixColorTable -> Just gl_POST_COLOR_MATRIX_COLOR_TABLE
   GetConvolution1D -> Just gl_CONVOLUTION_1D
   GetConvolution2D -> Just gl_CONVOLUTION_2D
   GetSeparable2D -> Just gl_SEPARABLE_2D
   GetMaxConvolutionWidth -> Just gl_MAX_CONVOLUTION_WIDTH
   GetMaxConvolutionHeight -> Just gl_MAX_CONVOLUTION_HEIGHT
   GetPostConvolutionRedScale -> Just gl_POST_CONVOLUTION_RED_SCALE
   GetPostConvolutionGreenScale -> Just gl_POST_CONVOLUTION_GREEN_SCALE
   GetPostConvolutionBlueScale -> Just gl_POST_CONVOLUTION_BLUE_SCALE
   GetPostConvolutionAlphaScale -> Just gl_POST_CONVOLUTION_ALPHA_SCALE
   GetPostConvolutionRedBias -> Just gl_POST_CONVOLUTION_RED_BIAS
   GetPostConvolutionGreenBias -> Just gl_POST_CONVOLUTION_GREEN_BIAS
   GetPostConvolutionBlueBias -> Just gl_POST_CONVOLUTION_BLUE_BIAS
   GetPostConvolutionAlphaBias -> Just gl_POST_CONVOLUTION_ALPHA_BIAS
   GetHistogram -> Just gl_HISTOGRAM
   GetMinmax -> Just gl_MINMAX
   GetColorSum -> Just gl_COLOR_SUM
   GetCurrentSecondaryColor -> Just gl_CURRENT_SECONDARY_COLOR
   GetRescaleNormal -> Just gl_RESCALE_NORMAL
   GetSharedTexturePalette -> Just gl_SHARED_TEXTURE_PALETTE
   GetTexture3DBinding -> Just gl_TEXTURE_BINDING_3D
   GetPackSkipImages -> Just gl_PACK_SKIP_IMAGES
   GetPackImageHeight -> Just gl_PACK_IMAGE_HEIGHT
   GetUnpackSkipImages -> Just gl_UNPACK_SKIP_IMAGES
   GetUnpackImageHeight -> Just gl_UNPACK_IMAGE_HEIGHT
   GetTexture3D -> Just gl_TEXTURE_3D
   GetMax3DTextureSize -> Just gl_MAX_3D_TEXTURE_SIZE
   GetMaxTextureLODBias -> Just gl_MAX_TEXTURE_LOD_BIAS
   GetMaxTextureMaxAnisotropy -> Just gl_MAX_TEXTURE_MAX_ANISOTROPY
   GetMultisample -> Just gl_MULTISAMPLE
   GetSampleAlphaToCoverage -> Just gl_SAMPLE_ALPHA_TO_COVERAGE
   GetSampleAlphaToOne -> Just gl_SAMPLE_ALPHA_TO_ONE
   GetSampleCoverage -> Just gl_SAMPLE_COVERAGE
   GetSampleBuffers -> Just gl_SAMPLE_BUFFERS
   GetSamples -> Just gl_SAMPLES
   GetSampleCoverageValue -> Just gl_SAMPLE_COVERAGE_VALUE
   GetSampleCoverageInvert -> Just gl_SAMPLE_COVERAGE_INVERT
   GetPointSizeMin -> Just gl_POINT_SIZE_MIN
   GetPointSizeMax -> Just gl_POINT_SIZE_MAX
   GetPointFadeThresholdSize -> Just gl_POINT_FADE_THRESHOLD_SIZE
   GetPointDistanceAttenuation -> Just gl_POINT_DISTANCE_ATTENUATION
   GetColorMatrix -> Just gl_COLOR_MATRIX
   GetColorMatrixStackDepth -> Just gl_COLOR_MATRIX_STACK_DEPTH
   GetMaxColorMatrixStackDepth -> Just gl_MAX_COLOR_MATRIX_STACK_DEPTH
   GetPostColorMatrixRedScale -> Just gl_POST_COLOR_MATRIX_RED_SCALE
   GetPostColorMatrixGreenScale -> Just gl_POST_COLOR_MATRIX_GREEN_SCALE
   GetPostColorMatrixBlueScale -> Just gl_POST_COLOR_MATRIX_BLUE_SCALE
   GetPostColorMatrixAlphaScale -> Just gl_POST_COLOR_MATRIX_ALPHA_SCALE
   GetPostColorMatrixRedBias -> Just gl_POST_COLOR_MATRIX_RED_BIAS
   GetPostColorMatrixGreenBias -> Just gl_POST_COLOR_MATRIX_GREEN_BIAS
   GetPostColorMatrixBlueBias -> Just gl_POST_COLOR_MATRIX_BLUE_BIAS
   GetPostColorMatrixAlphaBias -> Just gl_POST_COLOR_MATRIX_ALPHA_BIAS
   GetMaxElementsVertices -> Just gl_MAX_ELEMENTS_VERTICES
   GetMaxElementsIndices -> Just gl_MAX_ELEMENTS_INDICES
   GetActiveTexture -> Just gl_ACTIVE_TEXTURE
   GetClientActiveTexture -> Just gl_CLIENT_ACTIVE_TEXTURE
   GetMaxTextureUnits -> Just gl_MAX_TEXTURE_UNITS
   GetTextureCubeMap -> Just gl_TEXTURE_CUBE_MAP
   GetMaxCubeMapTextureSize -> Just gl_MAX_CUBE_MAP_TEXTURE_SIZE
   GetMaxRectangleTextureSize -> Just gl_MAX_RECTANGLE_TEXTURE_SIZE
   GetNumCompressedTextureFormats -> Just gl_NUM_COMPRESSED_TEXTURE_FORMATS
   GetCompressedTextureFormats -> Just gl_COMPRESSED_TEXTURE_FORMATS
   GetMaxVertexUnits -> Just gl_MAX_VERTEX_UNITS
   GetActiveVertexUnits -> Just gl_ACTIVE_VERTEX_UNITS
   GetWeightSumUnity -> Just gl_WEIGHT_SUM_UNITY
   GetVertexBlend -> Just gl_VERTEX_BLEND
   GetModelview i -> modelviewIndexToEnum i
   GetCurrentWeight -> Just gl_CURRENT_WEIGHT
   GetWeightArrayType -> Just gl_WEIGHT_ARRAY_TYPE
   GetWeightArrayStride -> Just gl_WEIGHT_ARRAY_STRIDE
   GetWeightArraySize -> Just gl_WEIGHT_ARRAY_SIZE
   GetWeightArray -> Just gl_WEIGHT_ARRAY
   GetMatrixPalette -> Just gl_MATRIX_PALETTE
   GetMaxMatrixPaletteStackDepth -> Just gl_MAX_MATRIX_PALETTE_STACK_DEPTH
   GetMaxPaletteMatrices -> Just gl_MAX_PALETTE_MATRICES
   GetCurrentPaletteMatrix -> Just gl_CURRENT_PALETTE_MATRIX
   GetBlendDstRGB -> Just gl_BLEND_DST_RGB
   GetBlendSrcRGB -> Just gl_BLEND_SRC_RGB
   GetBlendDstAlpha -> Just gl_BLEND_DST_ALPHA
   GetBlendSrcAlpha -> Just gl_BLEND_SRC_ALPHA
   GetPackCMYKHint -> Just gl_PACK_CMYK_HINT
   GetUnpackCMYKHint -> Just gl_UNPACK_CMYK_HINT
   GetArrayElementLockFirst -> Just gl_ARRAY_ELEMENT_LOCK_FIRST
   GetArrayElementLockCount -> Just gl_ARRAY_ELEMENT_LOCK_COUNT
   GetMaxShininess -> Just gl_MAX_SHININESS
   GetMaxSpotExponent -> Just gl_MAX_SPOT_EXPONENT
   GetFogDistanceMode -> Just gl_FOG_DISTANCE_MODE
   GetDepthBounds -> Just gl_DEPTH_BOUNDS
   GetPrimitiveRestartIndex -> Just gl_PRIMITIVE_RESTART_INDEX
   GetPrimitiveRestartNV -> Just gl_PRIMITIVE_RESTART_NV
   GetPrimitiveRestartIndexNV -> Just gl_PRIMITIVE_RESTART_INDEX_NV
   GetActiveStencilFace -> Just gl_ACTIVE_STENCIL_FACE
   GetArrayBufferBinding -> Just gl_ARRAY_BUFFER_BINDING
   GetElementArrayBufferBinding -> Just gl_ELEMENT_ARRAY_BUFFER_BINDING
   GetVertexArrayBufferBinding -> Just gl_VERTEX_ARRAY_BUFFER_BINDING
   GetNormalArrayBufferBinding -> Just gl_NORMAL_ARRAY_BUFFER_BINDING
   GetColorArrayBufferBinding -> Just gl_COLOR_ARRAY_BUFFER_BINDING
   GetIndexArrayBufferBinding -> Just gl_INDEX_ARRAY_BUFFER_BINDING
   GetTextureCoordArrayBufferBinding -> Just gl_TEXTURE_COORD_ARRAY_BUFFER_BINDING
   GetEdgeFlagArrayBufferBinding -> Just gl_EDGE_FLAG_ARRAY_BUFFER_BINDING
   GetSecondaryColorArrayBufferBinding -> Just gl_SECONDARY_COLOR_ARRAY_BUFFER_BINDING
   GetFogCoordArrayBufferBinding -> Just gl_FOG_COORD_ARRAY_BUFFER_BINDING
   GetTextureBindingCubeMap -> Just gl_TEXTURE_BINDING_CUBE_MAP
   GetTextureBindingRectangle -> Just gl_TEXTURE_BINDING_RECTANGLE
   GetCurrentMatrix -> Just gl_CURRENT_MATRIX
   GetCurrentMatrixStackDepth -> Just gl_CURRENT_MATRIX_STACK_DEPTH
   GetMaxCombinedTextureImageUnits -> Just gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS
   GetMaxDrawBuffers -> Just gl_MAX_DRAW_BUFFERS
   GetMaxFragmentUniformComponents -> Just gl_MAX_FRAGMENT_UNIFORM_COMPONENTS
   GetMaxTextureCoords -> Just gl_MAX_TEXTURE_COORDS
   GetMaxTextureImageUnits -> Just gl_MAX_TEXTURE_IMAGE_UNITS
   GetMaxVaryingComponents -> Just gl_MAX_VARYING_COMPONENTS
   GetMaxVaryingFloats -> Just gl_MAX_VARYING_COMPONENTS
   GetMaxVertexAttribs -> Just gl_MAX_VERTEX_ATTRIBS
   GetMaxVertexTextureImageUnits -> Just gl_MAX_VERTEX_TEXTURE_IMAGE_UNITS
   GetMaxVertexUniformComponents -> Just gl_MAX_VERTEX_UNIFORM_COMPONENTS
   GetCurrentProgram -> Just gl_CURRENT_PROGRAM
   GetPixelPackBufferBinding -> Just gl_PIXEL_PACK_BUFFER_BINDING
   GetPixelUnpackBufferBinding -> Just gl_PIXEL_UNPACK_BUFFER_BINDING
   GetDrawBufferN i -> drawBufferIndexToEnum i
   GetRGBASignedComponents -> Just gl_RGBA_SIGNED_COMPONENTS
   GetCopyReadBuffer -> Just gl_COPY_READ_BUFFER
   GetCopyWriteBuffer -> Just gl_COPY_WRITE_BUFFER
   -- GetWeightArrayBufferBinding -> Just gl_WEIGHT_ARRAY_BUFFER_BINDING
   GetContextProfileMask -> Just gl_CONTEXT_PROFILE_MASK
   -- transform feedback
   GetTransformFeedbackBufferBinding -> Just gl_TRANSFORM_FEEDBACK_BUFFER_BINDING
   GetMaxTransformFeedbackSeparateAttribs -> Just gl_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS
   GetMaxTransformFeedbackSeparateComponents -> Just gl_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS
   GetMaxTransformFeedbackInterleavedComponents -> Just gl_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS
   -- FramebufferObject
   GetDrawFramebufferBinding -> Just gl_DRAW_FRAMEBUFFER_BINDING
   GetReadFramebufferBinding -> Just gl_READ_FRAMEBUFFER_BINDING
   GetFramebufferBinding -> Just gl_FRAMEBUFFER_BINDING
   -- RenderbufferObject
   GetRenderbufferBinding -> Just gl_RENDERBUFFER_BINDING
   -- Color clamping
   GetVertexColorClamp -> Just gl_CLAMP_VERTEX_COLOR
   GetFragmentColorClamp -> Just gl_CLAMP_FRAGMENT_COLOR
   GetReadColorClamp -> Just gl_CLAMP_READ_COLOR

--------------------------------------------------------------------------------

data GetIndexedPName =
     GetTransformFeedbackBuffer
   | GetTransformFeedbackBufferStart
   | GetTransformFeedbackBufferSize

marshalGetIndexedPName :: GetIndexedPName -> GLenum
marshalGetIndexedPName x = case x of
   GetTransformFeedbackBuffer -> gl_TRANSFORM_FEEDBACK_BUFFER
   GetTransformFeedbackBufferSize -> gl_TRANSFORM_FEEDBACK_BUFFER_SIZE
   GetTransformFeedbackBufferStart -> gl_TRANSFORM_FEEDBACK_BUFFER_START

--------------------------------------------------------------------------------

-- 0x3000 through 0x3FFF are reserved for clip planes

clipPlaneIndexToEnum :: GLsizei -> Maybe GLenum
clipPlaneIndexToEnum i
   | 0 <= i && i <= maxClipPlaneIndex = Just (gl_CLIP_DISTANCE0 + fromIntegral i)
   | otherwise = Nothing

maxClipPlaneIndex :: GLsizei
maxClipPlaneIndex = 0xFFF

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

-- 0x8825 through 0x8834 are reserved for draw buffers

drawBufferIndexToEnum :: GLsizei -> Maybe GLenum
drawBufferIndexToEnum i
   | 0 <= i && i <= maxDrawBufferIndex = Just (gl_DRAW_BUFFER0 + fromIntegral i)
   | otherwise = Nothing

maxDrawBufferIndex :: GLsizei
maxDrawBufferIndex = fromIntegral (gl_DRAW_BUFFER15 - gl_DRAW_BUFFER0)

--------------------------------------------------------------------------------

getBoolean1 :: (GLboolean -> a) -> GetPName -> IO a
getBoolean1 f n = alloca $ \buf -> do
   getBooleanv n buf
   peek1 f buf

getBoolean4 :: (GLboolean -> GLboolean -> GLboolean -> GLboolean -> a)
            -> GetPName -> IO a
getBoolean4 f n = allocaArray 4 $ \buf -> do
   getBooleanv n buf
   peek4 f buf

getBoolean4i :: GLuint -> (GLboolean -> GLboolean -> GLboolean -> GLboolean -> a)
            -> GetPName -> IO a
getBoolean4i i f n = allocaArray 4 $ \buf -> do
   getBooleanvi i n buf
   peek4 f buf

getBooleanvi :: GLuint -> GetPName -> Ptr GLboolean -> IO ()
getBooleanvi i = makeGetter (\e -> glGetBooleani_v e i)

getBooleanv :: GetPName -> Ptr GLboolean -> IO ()
getBooleanv = makeGetter glGetBooleanv

{-# INLINE makeGetter #-}
makeGetter :: (GLenum -> Ptr a -> IO ()) -> GetPName -> Ptr a -> IO ()
makeGetter f = maybe (const recordInvalidEnum) f . marshalGetPName

--------------------------------------------------------------------------------

getInteger1 :: (GLint -> a) -> GetPName -> IO a
getInteger1 f n = alloca $ \buf -> do
   getIntegerv n buf
   peek1 f buf

getInteger2 :: (GLint -> GLint -> a) -> GetPName -> IO a
getInteger2 f n = allocaArray 2 $ \buf -> do
   getIntegerv n buf
   peek2 f buf

getInteger4 :: (GLint -> GLint -> GLint -> GLint -> a) -> GetPName -> IO a
getInteger4 f n = allocaArray 4 $ \buf -> do
   getIntegerv n buf
   peek4 f buf


getIntegerv :: GetPName -> Ptr GLint -> IO ()
getIntegerv = maybe (const recordInvalidEnum) glGetIntegerv . marshalGetPName

--------------------------------------------------------------------------------

getInteger1i :: (GLint -> a) -> GetIndexedPName -> GLuint -> IO a
getInteger1i f n i = alloca $ \buf -> do
   getIntegeriv n i buf
   peek1 f buf

getIntegeriv :: GetIndexedPName -> GLuint -> Ptr GLint -> IO ()
getIntegeriv p = glGetIntegeri_v (marshalGetIndexedPName p)

--------------------------------------------------------------------------------

getEnum1 :: (GLenum -> a) -> GetPName -> IO a
getEnum1 f n = alloca $ \buf -> do
   getIntegerv n buf
   peek1 f (castPtr buf)

--------------------------------------------------------------------------------

getSizei1 :: (GLsizei -> a) -> GetPName -> IO a
getSizei1 f n = alloca $ \buf -> do
   getIntegerv n buf
   peek1 f (castPtr buf)

getSizei2 :: (GLsizei -> GLsizei -> a) -> GetPName -> IO a
getSizei2 f n = allocaArray 2 $ \buf -> do
   getIntegerv n buf
   peek2 f (castPtr buf)

--------------------------------------------------------------------------------

getFloat1 :: (GLfloat -> a) -> GetPName -> IO a
getFloat1 f n = alloca $ \buf -> do
   getFloatv n buf
   peek1 f buf

getFloat2 :: (GLfloat -> GLfloat -> a) -> GetPName -> IO a
getFloat2 f n = allocaArray 2 $ \buf -> do
   getFloatv n buf
   peek2 f buf

getFloat3 :: (GLfloat -> GLfloat -> GLfloat -> a) -> GetPName -> IO a
getFloat3 f n = allocaArray 3 $ \buf -> do
   getFloatv n buf
   peek3 f buf

getFloat4 ::
   (GLfloat -> GLfloat -> GLfloat -> GLfloat -> a) -> GetPName -> IO a
getFloat4 f n = allocaArray 4 $ \buf -> do
   getFloatv n buf
   peek4 f buf

getFloatv :: GetPName -> Ptr GLfloat -> IO ()
getFloatv = maybe (const recordInvalidEnum) glGetFloatv . marshalGetPName

--------------------------------------------------------------------------------

getClampf1 :: (GLclampf -> a) -> GetPName -> IO a
getClampf1 f n = alloca $ \buf -> do
   getFloatv n buf
   peek1 f (castPtr buf)

getClampf4 ::
   (GLclampf -> GLclampf -> GLclampf -> GLclampf -> a) -> GetPName -> IO a
getClampf4 f n = allocaArray 4 $ \buf -> do
   getFloatv n buf
   peek4 f (castPtr buf)

--------------------------------------------------------------------------------

getDouble1 :: (GLdouble -> a) -> GetPName -> IO a
getDouble1 f n = alloca $ \buf -> do
   getDoublev n buf
   peek1 f buf

getDouble2 :: (GLdouble -> GLdouble -> a) -> GetPName -> IO a
getDouble2 f n = allocaArray 2 $ \buf -> do
   getDoublev n buf
   peek2 f buf

getDouble4 ::
   (GLdouble -> GLdouble -> GLdouble -> GLdouble -> a) -> GetPName -> IO a
getDouble4 f n = allocaArray 4 $ \buf -> do
   getDoublev n buf
   peek4 f buf

getDoublev :: GetPName -> Ptr GLdouble -> IO ()
getDoublev = maybe (const recordInvalidEnum) glGetDoublev . marshalGetPName

--------------------------------------------------------------------------------

getClampd1 :: (GLclampd -> a) -> GetPName -> IO a
getClampd1 f n = alloca $ \buf -> do
   getDoublev n buf
   peek1 f (castPtr buf)

getClampd2 :: (GLclampd -> GLclampd -> a) -> GetPName -> IO a
getClampd2 f n = allocaArray 2 $ \buf -> do
   getDoublev n buf
   peek2 f (castPtr buf)

--------------------------------------------------------------------------------

maybeNullPtr :: b -> (Ptr a -> b) -> Ptr a -> b
maybeNullPtr n f ptr | ptr == nullPtr = n
                     | otherwise      = f ptr
--------------------------------------------------------------------------------

newtype AttribLocation = AttribLocation GLuint
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

data GetVertexAttribPName =
     GetVertexAttribArrayEnabled
   | GetVertexAttribArraySize
   | GetVertexAttribArrayStride
   | GetVertexAttribArrayType
   | GetVertexAttribArrayNormalized
   | GetCurrentVertexAttrib
   | GetVertexAttribArrayBufferBinding
   | GetVertexAttribArrayInteger

marshalGetVertexAttribPName :: GetVertexAttribPName -> GLenum
marshalGetVertexAttribPName x = case x of
   GetVertexAttribArrayEnabled -> gl_VERTEX_ATTRIB_ARRAY_ENABLED
   GetVertexAttribArraySize -> gl_VERTEX_ATTRIB_ARRAY_SIZE
   GetVertexAttribArrayStride -> gl_VERTEX_ATTRIB_ARRAY_STRIDE
   GetVertexAttribArrayType -> gl_VERTEX_ATTRIB_ARRAY_TYPE
   GetVertexAttribArrayNormalized -> gl_VERTEX_ATTRIB_ARRAY_NORMALIZED
   GetCurrentVertexAttrib -> gl_CURRENT_VERTEX_ATTRIB
   GetVertexAttribArrayBufferBinding -> gl_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING
   GetVertexAttribArrayInteger -> gl_VERTEX_ATTRIB_ARRAY_INTEGER

--------------------------------------------------------------------------------

getVertexAttribInteger1 :: (GLint -> b) -> AttribLocation -> GetVertexAttribPName -> IO b
getVertexAttribInteger1 f (AttribLocation location) n = alloca $ \buf -> do
   glGetVertexAttribiv location (marshalGetVertexAttribPName n) buf
   peek1 f buf

getVertexAttribEnum1 :: (GLenum -> b) -> AttribLocation -> GetVertexAttribPName -> IO b
getVertexAttribEnum1 f = getVertexAttribInteger1 (f . fromIntegral)

getVertexAttribBoolean1 :: (GLboolean -> b) -> AttribLocation -> GetVertexAttribPName -> IO b
getVertexAttribBoolean1 f = getVertexAttribInteger1 (f . fromIntegral)

getVertexAttribFloat4 :: (GLfloat -> GLfloat -> GLfloat -> GLfloat -> b) -> AttribLocation -> GetVertexAttribPName -> IO b
getVertexAttribFloat4 f (AttribLocation location) n = alloca $ \buf -> do
   glGetVertexAttribfv location (marshalGetVertexAttribPName n) buf
   peek4 f buf

getVertexAttribIInteger4 :: (GLint -> GLint -> GLint -> GLint -> b) -> AttribLocation -> GetVertexAttribPName -> IO b
getVertexAttribIInteger4 f (AttribLocation location) n = alloca $ \buf -> do
   glGetVertexAttribIiv location (marshalGetVertexAttribPName n) buf
   peek4 f buf

getVertexAttribIuInteger4 :: (GLuint -> GLuint -> GLuint -> GLuint -> b) -> AttribLocation -> GetVertexAttribPName -> IO b
getVertexAttribIuInteger4 f (AttribLocation location) n = alloca $ \buf -> do
   glGetVertexAttribIuiv location (marshalGetVertexAttribPName n) buf
   peek4 f buf

--------------------------------------------------------------------------------

data GetVertexAttribPointerPName =
   VertexAttribArrayPointer

marshalGetVertexAttribPointerPName :: GetVertexAttribPointerPName -> GLenum
marshalGetVertexAttribPointerPName x = case x of
   VertexAttribArrayPointer -> gl_VERTEX_ATTRIB_ARRAY_POINTER

--------------------------------------------------------------------------------

getVertexAttribPointer :: AttribLocation -> GetVertexAttribPointerPName -> IO (Ptr a)
getVertexAttribPointer (AttribLocation location) n = alloca $ \buf -> do
   glGetVertexAttribPointerv location (marshalGetVertexAttribPointerPName n) buf
   peek buf
