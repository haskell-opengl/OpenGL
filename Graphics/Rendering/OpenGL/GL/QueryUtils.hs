-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.QueryUtils
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module with utilities to query OpenGL state.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(..),
   clipPlaneIndexToEnum, lightIndexToEnum,
   getBoolean1, getBoolean4,
   getInteger1, getInteger2, getInteger4, getIntegerv,
   getEnum1,
   getSizei1,
   getFloat1, getFloat2, getFloat3, getFloat4, getFloatv,
   getDouble1, getDouble2, getDouble4, getDoublev
) where

import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( allocaArray )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLboolean, GLenum, GLint, GLsizei, GLfloat, GLdouble )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek1, peek2, peek3, peek4 )
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal ( recordInvalidEnum )

--------------------------------------------------------------------------------

data GetPName =
     GetCurrentColor
   | GetCurrentIndex
   | GetCurrentNormal
   | GetCurrentTextureCoords
   | GetCurrentRasterColor
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
   | GetLight GLsizei
   | GetTransposeModelviewMatrix
   | GetTransposeProjectionMatrix
   | GetTransposeTextureMatrix
   | GetTransposeColorMatrix
   | GetLightModelColorControl
   | GetBlendColor
   | GetBlendEquation
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
   | GetNumCompressedTextureFormats
   | GetCompressedTextureFormats
   | GetMaxVertexUnits
   | GetActiveVertexUnits
   | GetWeightSumUnity
   | GetVertexBlend
   | GetModelview0
   | GetModelview1
   | GetModelview2
   | GetModelview3
   | GetModelview4
   | GetModelview5
   | GetModelview6
   | GetModelview7
   | GetModelview8
   | GetModelview9
   | GetModelview10
   | GetModelview11
   | GetModelview12
   | GetModelview13
   | GetModelview14
   | GetModelview15
   | GetModelview16
   | GetModelview17
   | GetModelview18
   | GetModelview19
   | GetModelview20
   | GetModelview21
   | GetModelview22
   | GetModelview23
   | GetModelview24
   | GetModelview25
   | GetModelview26
   | GetModelview27
   | GetModelview28
   | GetModelview29
   | GetModelview30
   | GetModelview31
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
   -- GetWeightArrayBufferBinding
   -- GetVertexAttribArrayBufferBinding

marshalGetPName :: GetPName -> Maybe GLenum
marshalGetPName x = case x of
   GetCurrentColor -> Just 0xb00
   GetCurrentIndex -> Just 0xb01
   GetCurrentNormal -> Just 0xb02
   GetCurrentTextureCoords -> Just 0xb03
   GetCurrentRasterColor -> Just 0xb04
   GetCurrentRasterIndex -> Just 0xb05
   GetCurrentRasterTextureCoords -> Just 0xb06
   GetCurrentRasterPosition -> Just 0xb07
   GetCurrentRasterPositionValid -> Just 0xb08
   GetCurrentRasterDistance -> Just 0xb09
   GetCurrentMatrixIndex -> Just 0x8845
   GetPointSmooth -> Just 0xb10
   GetPointSize -> Just 0xb11
   GetPointSizeRange -> Just 0xb12
   GetPointSizeGranularity -> Just 0xb13
   GetLineSmooth -> Just 0xb20
   GetLineWidth -> Just 0xb21
   GetLineWidthRange -> Just 0xb22
   GetLineWidthGranularity -> Just 0xb23
   GetLineStipple -> Just 0xb24
   GetLineStipplePattern -> Just 0xb25
   GetLineStippleRepeat -> Just 0xb26
   GetSmoothPointSizeRange -> Just 0xb12
   GetSmoothPointSizeGranularity -> Just 0xb13
   GetSmoothLineWidthRange -> Just 0xb22
   GetSmoothLineWidthGranularity -> Just 0xb23
   GetAliasedPointSizeRange -> Just 0x846d
   GetAliasedLineWidthRange -> Just 0x846e
   GetListMode -> Just 0xb30
   GetMaxListNesting -> Just 0xb31
   GetListBase -> Just 0xb32
   GetListIndex -> Just 0xb33
   GetPolygonMode -> Just 0xb40
   GetPolygonSmooth -> Just 0xb41
   GetPolygonStipple -> Just 0xb42
   GetEdgeFlag -> Just 0xb43
   GetCullFace -> Just 0xb44
   GetCullFaceMode -> Just 0xb45
   GetFrontFace -> Just 0xb46
   GetLighting -> Just 0xb50
   GetLightModelLocalViewer -> Just 0xb51
   GetLightModelTwoSide -> Just 0xb52
   GetLightModelAmbient -> Just 0xb53
   GetShadeModel -> Just 0xb54
   GetColorMaterialFace -> Just 0xb55
   GetColorMaterialParameter -> Just 0xb56
   GetColorMaterial -> Just 0xb57
   GetFog -> Just 0xb60
   GetFogIndex -> Just 0xb61
   GetFogDensity -> Just 0xb62
   GetFogStart -> Just 0xb63
   GetFogEnd -> Just 0xb64
   GetFogMode -> Just 0xb65
   GetFogColor -> Just 0xb66
   GetFogCoordSrc -> Just 0x8450
   GetCurrentFogCoord -> Just 0x8453
   GetDepthRange -> Just 0xb70
   GetDepthTest -> Just 0xb71
   GetDepthWritemask -> Just 0xb72
   GetDepthClearValue -> Just 0xb73
   GetDepthFunc -> Just 0xb74
   GetAccumClearValue -> Just 0xb80
   GetStencilTest -> Just 0xb90
   GetStencilClearValue -> Just 0xb91
   GetStencilFunc -> Just 0xb92
   GetStencilValueMask -> Just 0xb93
   GetStencilFail -> Just 0xb94
   GetStencilPassDepthFail -> Just 0xb95
   GetStencilPassDepthPass -> Just 0xb96
   GetStencilRef -> Just 0xb97
   GetStencilWritemask -> Just 0xb98
   GetMatrixMode -> Just 0xba0
   GetNormalize -> Just 0xba1
   GetViewport -> Just 0xba2
   GetModelviewStackDepth -> Just 0xba3
   GetProjectionStackDepth -> Just 0xba4
   GetTextureStackDepth -> Just 0xba5
   GetModelviewMatrix -> Just 0xba6
   GetProjectionMatrix -> Just 0xba7
   GetTextureMatrix -> Just 0xba8
   GetAttribStackDepth -> Just 0xbb0
   GetClientAttribStackDepth -> Just 0xbb1
   GetAlphaTest -> Just 0xbc0
   GetAlphaTestFunc -> Just 0xbc1
   GetAlphaTestRef -> Just 0xbc2
   GetDither -> Just 0xbd0
   GetBlendDst -> Just 0xbe0
   GetBlendSrc -> Just 0xbe1
   GetBlend -> Just 0xbe2
   GetLogicOpMode -> Just 0xbf0
   GetIndexLogicOp -> Just 0xbf1
   GetLogicOp -> Just 0xbf1
   GetColorLogicOp -> Just 0xbf2
   GetAuxBuffers -> Just 0xc00
   GetDrawBuffer -> Just 0xc01
   GetReadBuffer -> Just 0xc02
   GetScissorBox -> Just 0xc10
   GetScissorTest -> Just 0xc11
   GetIndexClearValue -> Just 0xc20
   GetIndexWritemask -> Just 0xc21
   GetColorClearValue -> Just 0xc22
   GetColorWritemask -> Just 0xc23
   GetIndexMode -> Just 0xc30
   GetRGBAMode -> Just 0xc31
   GetDoublebuffer -> Just 0xc32
   GetStereo -> Just 0xc33
   GetRenderMode -> Just 0xc40
   GetPerspectiveCorrectionHint -> Just 0xc50
   GetPointSmoothHint -> Just 0xc51
   GetLineSmoothHint -> Just 0xc52
   GetPolygonSmoothHint -> Just 0xc53
   GetFogHint -> Just 0xc54
   GetGenerateMipmapHint -> Just 0x8192
   GetTextureCompressionHint -> Just 0x84ef
   GetTextureGenS -> Just 0xc60
   GetTextureGenT -> Just 0xc61
   GetTextureGenR -> Just 0xc62
   GetTextureGenQ -> Just 0xc63
   GetPixelMapIToISize -> Just 0xcb0
   GetPixelMapSToSSize -> Just 0xcb1
   GetPixelMapIToRSize -> Just 0xcb2
   GetPixelMapIToGSize -> Just 0xcb3
   GetPixelMapIToBSize -> Just 0xcb4
   GetPixelMapIToASize -> Just 0xcb5
   GetPixelMapRToRSize -> Just 0xcb6
   GetPixelMapGToGSize -> Just 0xcb7
   GetPixelMapBToBSize -> Just 0xcb8
   GetPixelMapAToASize -> Just 0xcb9
   GetUnpackSwapBytes -> Just 0xcf0
   GetUnpackLSBFirst -> Just 0xcf1
   GetUnpackRowLength -> Just 0xcf2
   GetUnpackSkipRows -> Just 0xcf3
   GetUnpackSkipPixels -> Just 0xcf4
   GetUnpackAlignment -> Just 0xcf5
   GetPackSwapBytes -> Just 0xd00
   GetPackLSBFirst -> Just 0xd01
   GetPackRowLength -> Just 0xd02
   GetPackSkipRows -> Just 0xd03
   GetPackSkipPixels -> Just 0xd04
   GetPackAlignment -> Just 0xd05
   GetMapColor -> Just 0xd10
   GetMapStencil -> Just 0xd11
   GetIndexShift -> Just 0xd12
   GetIndexOffset -> Just 0xd13
   GetRedScale -> Just 0xd14
   GetRedBias -> Just 0xd15
   GetZoomX -> Just 0xd16
   GetZoomY -> Just 0xd17
   GetGreenScale -> Just 0xd18
   GetGreenBias -> Just 0xd19
   GetBlueScale -> Just 0xd1a
   GetBlueBias -> Just 0xd1b
   GetAlphaScale -> Just 0xd1c
   GetAlphaBias -> Just 0xd1d
   GetDepthScale -> Just 0xd1e
   GetDepthBias -> Just 0xd1f
   GetMaxEvalOrder -> Just 0xd30
   GetMaxLights -> Just 0xd31
   GetMaxClipPlanes -> Just 0xd32
   GetMaxTextureSize -> Just 0xd33
   GetMaxPixelMapTable -> Just 0xd34
   GetMaxAttribStackDepth -> Just 0xd35
   GetMaxModelviewStackDepth -> Just 0xd36
   GetMaxNameStackDepth -> Just 0xd37
   GetMaxProjectionStackDepth -> Just 0xd38
   GetMaxTextureStackDepth -> Just 0xd39
   GetMaxViewportDims -> Just 0xd3a
   GetMaxClientAttribStackDepth -> Just 0xd3b
   GetSubpixelBits -> Just 0xd50
   GetIndexBits -> Just 0xd51
   GetRedBits -> Just 0xd52
   GetGreenBits -> Just 0xd53
   GetBlueBits -> Just 0xd54
   GetAlphaBits -> Just 0xd55
   GetDepthBits -> Just 0xd56
   GetStencilBits -> Just 0xd57
   GetAccumRedBits -> Just 0xd58
   GetAccumGreenBits -> Just 0xd59
   GetAccumBlueBits -> Just 0xd5a
   GetAccumAlphaBits -> Just 0xd5b
   GetNameStackDepth -> Just 0xd70
   GetAutoNormal -> Just 0xd80
   GetMap1Color4 -> Just 0xd90
   GetMap1Index -> Just 0xd91
   GetMap1Normal -> Just 0xd92
   GetMap1TextureCoord1 -> Just 0xd93
   GetMap1TextureCoord2 -> Just 0xd94
   GetMap1TextureCoord3 -> Just 0xd95
   GetMap1TextureCoord4 -> Just 0xd96
   GetMap1Vertex3 -> Just 0xd97
   GetMap1Vertex4 -> Just 0xd98
   GetMap2Color4 -> Just 0xdb0
   GetMap2Index -> Just 0xdb1
   GetMap2Normal -> Just 0xdb2
   GetMap2TextureCoord1 -> Just 0xdb3
   GetMap2TextureCoord2 -> Just 0xdb4
   GetMap2TextureCoord3 -> Just 0xdb5
   GetMap2TextureCoord4 -> Just 0xdb6
   GetMap2Vertex3 -> Just 0xdb7
   GetMap2Vertex4 -> Just 0xdb8
   GetMap1GridDomain -> Just 0xdd0
   GetMap1GridSegments -> Just 0xdd1
   GetMap2GridDomain -> Just 0xdd2
   GetMap2GridSegments -> Just 0xdd3
   GetTexture1D -> Just 0xde0
   GetTexture2D -> Just 0xde1
   GetFeedbackBufferSize -> Just 0xdf1
   GetFeedbackBufferType -> Just 0xdf2
   GetSelectionBufferSize -> Just 0xdf4
   GetPolygonOffsetUnits -> Just 0x2a00
   GetPolygonOffsetPoint -> Just 0x2a01
   GetPolygonOffsetLine -> Just 0x2a02
   GetPolygonOffsetFill -> Just 0x8037
   GetPolygonOffsetFactor -> Just 0x8038
   GetTextureBinding1D -> Just 0x8068
   GetTextureBinding2D -> Just 0x8069
   GetTextureBinding3D -> Just 0x806a
   GetVertexArray -> Just 0x8074
   GetNormalArray -> Just 0x8075
   GetColorArray -> Just 0x8076
   GetIndexArray -> Just 0x8077
   GetTextureCoordArray -> Just 0x8078
   GetEdgeFlagArray -> Just 0x8079
   GetFogCoordArray -> Just 0x8457
   GetSecondaryColorArray -> Just 0x845e
   GetMatrixIndexArray -> Just 0x8844
   GetVertexArraySize -> Just 0x807a
   GetVertexArrayType -> Just 0x807b
   GetVertexArrayStride -> Just 0x807c
   GetNormalArrayType -> Just 0x807e
   GetNormalArrayStride -> Just 0x807f
   GetColorArraySize -> Just 0x8081
   GetColorArrayType -> Just 0x8082
   GetColorArrayStride -> Just 0x8083
   GetIndexArrayType -> Just 0x8085
   GetIndexArrayStride -> Just 0x8086
   GetTextureCoordArraySize -> Just 0x8088
   GetTextureCoordArrayType -> Just 0x8089
   GetTextureCoordArrayStride -> Just 0x808a
   GetEdgeFlagArrayStride -> Just 0x808c
   GetFogCoordArrayType -> Just 0x8454
   GetFogCoordArrayStride -> Just 0x8455
   GetSecondaryColorArraySize -> Just 0x845a
   GetSecondaryColorArrayType -> Just 0x845b
   GetSecondaryColorArrayStride -> Just 0x845c
   GetMatrixIndexArraySize -> Just 0x8846
   GetMatrixIndexArrayType -> Just 0x8847
   GetMatrixIndexArrayStride -> Just 0x8848
   GetClipPlane i -> clipPlaneIndexToEnum i
   GetLight i -> lightIndexToEnum i
   GetTransposeModelviewMatrix -> Just 0x84e3
   GetTransposeProjectionMatrix -> Just 0x84e4
   GetTransposeTextureMatrix -> Just 0x84e5
   GetTransposeColorMatrix -> Just 0x84e6
   GetLightModelColorControl -> Just 0x81f8
   GetBlendColor -> Just 0x8005
   GetBlendEquation -> Just 0x8009
   GetColorTable -> Just 0x80d0
   GetPostConvolutionColorTable -> Just 0x80d1
   GetPostColorMatrixColorTable -> Just 0x80d2
   GetConvolution1D -> Just 0x8010
   GetConvolution2D -> Just 0x8011
   GetSeparable2D -> Just 0x8012
   GetMaxConvolutionWidth -> Just 0x801a
   GetMaxConvolutionHeight -> Just 0x801b
   GetPostConvolutionRedScale -> Just 0x801c
   GetPostConvolutionGreenScale -> Just 0x801d
   GetPostConvolutionBlueScale -> Just 0x801e
   GetPostConvolutionAlphaScale -> Just 0x801f
   GetPostConvolutionRedBias -> Just 0x8020
   GetPostConvolutionGreenBias -> Just 0x8021
   GetPostConvolutionBlueBias -> Just 0x8022
   GetPostConvolutionAlphaBias -> Just 0x8023
   GetHistogram -> Just 0x8024
   GetMinmax -> Just 0x802e
   GetColorSum -> Just 0x8458
   GetCurrentSecondaryColor -> Just 0x8459
   GetRescaleNormal -> Just 0x803a
   GetSharedTexturePalette -> Just 0x81fb
   GetTexture3DBinding -> Just 0x806a
   GetPackSkipImages -> Just 0x806b
   GetPackImageHeight -> Just 0x806c
   GetUnpackSkipImages -> Just 0x806d
   GetUnpackImageHeight -> Just 0x806e
   GetTexture3D -> Just 0x806f
   GetMax3DTextureSize -> Just 0x8073
   GetMaxTextureLODBias -> Just 0x84fd
   GetMaxTextureMaxAnisotropy -> Just 0x84ff
   GetMultisample -> Just 0x809d
   GetSampleAlphaToCoverage -> Just 0x809e
   GetSampleAlphaToOne -> Just 0x809f
   GetSampleCoverage -> Just 0x80a0
   GetSampleBuffers -> Just 0x80a8
   GetSamples -> Just 0x80a9
   GetSampleCoverageValue -> Just 0x80aa
   GetSampleCoverageInvert -> Just 0x80ab
   GetPointSizeMin -> Just 0x8126
   GetPointSizeMax -> Just 0x8127
   GetPointFadeThresholdSize -> Just 0x8128
   GetPointDistanceAttenuation -> Just 0x8129
   GetColorMatrix -> Just 0x80b1
   GetColorMatrixStackDepth -> Just 0x80b2
   GetMaxColorMatrixStackDepth -> Just 0x80b3
   GetPostColorMatrixRedScale -> Just 0x80b4
   GetPostColorMatrixGreenScale -> Just 0x80b5
   GetPostColorMatrixBlueScale -> Just 0x80b6
   GetPostColorMatrixAlphaScale -> Just 0x80b7
   GetPostColorMatrixRedBias -> Just 0x80b8
   GetPostColorMatrixGreenBias -> Just 0x80b9
   GetPostColorMatrixBlueBias -> Just 0x80ba
   GetPostColorMatrixAlphaBias -> Just 0x80bb
   GetMaxElementsVertices -> Just 0x80e8
   GetMaxElementsIndices -> Just 0x80e9
   GetActiveTexture -> Just 0x84e0
   GetClientActiveTexture -> Just 0x84e1
   GetMaxTextureUnits -> Just 0x84e2
   GetTextureCubeMap -> Just 0x8513
   GetMaxCubeMapTextureSize -> Just 0x851c
   GetNumCompressedTextureFormats -> Just 0x86a2
   GetCompressedTextureFormats -> Just 0x86a3
   GetMaxVertexUnits -> Just 0x86a4
   GetActiveVertexUnits -> Just 0x86a5
   GetWeightSumUnity -> Just 0x86a6
   GetVertexBlend -> Just 0x86a7
   GetModelview0 -> Just 0x1700
   GetModelview1 -> Just 0x850a
   GetModelview2 -> Just 0x8722
   GetModelview3 -> Just 0x8723
   GetModelview4 -> Just 0x8724
   GetModelview5 -> Just 0x8725
   GetModelview6 -> Just 0x8726
   GetModelview7 -> Just 0x8727
   GetModelview8 -> Just 0x8728
   GetModelview9 -> Just 0x8729
   GetModelview10 -> Just 0x872a
   GetModelview11 -> Just 0x872b
   GetModelview12 -> Just 0x872c
   GetModelview13 -> Just 0x872d
   GetModelview14 -> Just 0x872e
   GetModelview15 -> Just 0x872f
   GetModelview16 -> Just 0x8730
   GetModelview17 -> Just 0x8731
   GetModelview18 -> Just 0x8732
   GetModelview19 -> Just 0x8733
   GetModelview20 -> Just 0x8734
   GetModelview21 -> Just 0x8735
   GetModelview22 -> Just 0x8736
   GetModelview23 -> Just 0x8737
   GetModelview24 -> Just 0x8738
   GetModelview25 -> Just 0x8739
   GetModelview26 -> Just 0x873a
   GetModelview27 -> Just 0x873b
   GetModelview28 -> Just 0x873c
   GetModelview29 -> Just 0x873d
   GetModelview30 -> Just 0x873e
   GetModelview31 -> Just 0x873f
   GetCurrentWeight -> Just 0x86a8
   GetWeightArrayType -> Just 0x86a9
   GetWeightArrayStride -> Just 0x86aa
   GetWeightArraySize -> Just 0x86ab
   GetWeightArray -> Just 0x86ad
   GetMatrixPalette -> Just 0x8840
   GetMaxMatrixPaletteStackDepth -> Just 0x8841
   GetMaxPaletteMatrices -> Just 0x8842
   GetCurrentPaletteMatrix -> Just 0x8843
   GetBlendDstRGB -> Just 0x80c8
   GetBlendSrcRGB -> Just 0x80c9
   GetBlendDstAlpha -> Just 0x80ca
   GetBlendSrcAlpha -> Just 0x80cb
   GetPackCMYKHint -> Just 0x800e
   GetUnpackCMYKHint -> Just 0x800f
   GetArrayElementLockFirst -> Just 0x81a8
   GetArrayElementLockCount -> Just 0x81a9
   GetMaxShininess -> Just 0x8504
   GetMaxSpotExponent -> Just 0x8505
   GetFogDistanceMode -> Just 0x855a
   GetDepthBounds -> Just 0x8891
   GetPrimitiveRestartIndex -> Just 0x8559
   GetActiveStencilFace -> Just 0x8911
   GetArrayBufferBinding -> Just 0x8894
   GetElementArrayBufferBinding -> Just 0x8895
   GetVertexArrayBufferBinding -> Just 0x8896
   GetNormalArrayBufferBinding -> Just 0x8897
   GetColorArrayBufferBinding -> Just 0x8898
   GetIndexArrayBufferBinding -> Just 0x8899
   GetTextureCoordArrayBufferBinding -> Just 0x889a
   GetEdgeFlagArrayBufferBinding -> Just 0x889b
   GetSecondaryColorArrayBufferBinding -> Just 0x889c
   GetFogCoordArrayBufferBinding -> Just 0x889d
   GetTextureBindingCubeMap -> Just 0x8514
   GetTextureBindingRectangle -> Just 0x84f6
   GetCurrentMatrix -> Just 0x8641
   GetCurrentMatrixStackDepth -> Just 0x8640
   -- GetWeightArrayBufferBinding -> Just 0x889e
   -- GetVertexAttribArrayBufferBinding -> Just 0x889f

--------------------------------------------------------------------------------

-- 0x3000 through 0x3FFF are reserved for clip planes

clipPlaneIndexToEnum :: GLsizei -> Maybe GLenum
clipPlaneIndexToEnum i
   | 0 <= i && i <= 0xFFF = Just (0x3000 + fromIntegral i)
   | otherwise = Nothing

--------------------------------------------------------------------------------

-- 0x4000 through 0x4FFF are reserved for light numbers

lightIndexToEnum :: GLsizei -> Maybe GLenum
lightIndexToEnum i
   | 0 <= i && i <= 0xFFF = Just (0x4000 + fromIntegral i)
   | otherwise = Nothing

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

foreign import CALLCONV unsafe "glGetBooleanv" glGetBooleanv ::
   GLenum -> Ptr GLboolean -> IO ()

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

foreign import CALLCONV unsafe "glGetIntegerv" glGetIntegerv ::
   GLenum -> Ptr GLint -> IO ()

--------------------------------------------------------------------------------

getEnum1 :: (GLenum -> a) -> GetPName -> IO a
getEnum1 f = getInteger1 (f . fromIntegral)

getSizei1 :: (GLsizei -> a) -> GetPName -> IO a
getSizei1 f = getInteger1 (f . fromIntegral)

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

foreign import CALLCONV unsafe "glGetFloatv" glGetFloatv ::
   GLenum -> Ptr GLfloat -> IO ()

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

foreign import CALLCONV unsafe "glGetDoublev" glGetDoublev ::
   GLenum -> Ptr GLdouble -> IO ()
