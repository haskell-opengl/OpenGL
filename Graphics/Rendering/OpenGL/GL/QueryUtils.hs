-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.QueryUtils
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
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
   getInteger1, getInteger2, getInteger4,
   getEnum1,
   getSizei1,
   getFloat1, getFloat2, getFloat3, getFloat4, getFloatv,
   getDouble1, getDouble2, getDouble4, getDoublev,
   GetPointervPName(..), getPointer,
   getArrayWith
) where

import Control.Monad ( liftM )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( allocaArray, peekArray )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable(peek) )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLboolean, GLenum, GLint, GLsizei, GLfloat, GLdouble )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek1, peek2, peek3, peek4 )

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
   | GetMaxTextureLodBias
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

marshalGetPName :: GetPName -> GLenum
marshalGetPName x = case x of
   GetCurrentColor -> 0xb00
   GetCurrentIndex -> 0xb01
   GetCurrentNormal -> 0xb02
   GetCurrentTextureCoords -> 0xb03
   GetCurrentRasterColor -> 0xb04
   GetCurrentRasterIndex -> 0xb05
   GetCurrentRasterTextureCoords -> 0xb06
   GetCurrentRasterPosition -> 0xb07
   GetCurrentRasterPositionValid -> 0xb08
   GetCurrentRasterDistance -> 0xb09
   GetCurrentMatrixIndex -> 0x8845
   GetPointSmooth -> 0xb10
   GetPointSize -> 0xb11
   GetPointSizeRange -> 0xb12
   GetPointSizeGranularity -> 0xb13
   GetLineSmooth -> 0xb20
   GetLineWidth -> 0xb21
   GetLineWidthRange -> 0xb22
   GetLineWidthGranularity -> 0xb23
   GetLineStipple -> 0xb24
   GetLineStipplePattern -> 0xb25
   GetLineStippleRepeat -> 0xb26
   GetSmoothPointSizeRange -> 0xb12
   GetSmoothPointSizeGranularity -> 0xb13
   GetSmoothLineWidthRange -> 0xb22
   GetSmoothLineWidthGranularity -> 0xb23
   GetAliasedPointSizeRange -> 0x846d
   GetAliasedLineWidthRange -> 0x846e
   GetListMode -> 0xb30
   GetMaxListNesting -> 0xb31
   GetListBase -> 0xb32
   GetListIndex -> 0xb33
   GetPolygonMode -> 0xb40
   GetPolygonSmooth -> 0xb41
   GetPolygonStipple -> 0xb42
   GetEdgeFlag -> 0xb43
   GetCullFace -> 0xb44
   GetCullFaceMode -> 0xb45
   GetFrontFace -> 0xb46
   GetLighting -> 0xb50
   GetLightModelLocalViewer -> 0xb51
   GetLightModelTwoSide -> 0xb52
   GetLightModelAmbient -> 0xb53
   GetShadeModel -> 0xb54
   GetColorMaterialFace -> 0xb55
   GetColorMaterialParameter -> 0xb56
   GetColorMaterial -> 0xb57
   GetFog -> 0xb60
   GetFogIndex -> 0xb61
   GetFogDensity -> 0xb62
   GetFogStart -> 0xb63
   GetFogEnd -> 0xb64
   GetFogMode -> 0xb65
   GetFogColor -> 0xb66
   GetFogCoordSrc -> 0x8450
   GetCurrentFogCoord -> 0x8453
   GetDepthRange -> 0xb70
   GetDepthTest -> 0xb71
   GetDepthWritemask -> 0xb72
   GetDepthClearValue -> 0xb73
   GetDepthFunc -> 0xb74
   GetAccumClearValue -> 0xb80
   GetStencilTest -> 0xb90
   GetStencilClearValue -> 0xb91
   GetStencilFunc -> 0xb92
   GetStencilValueMask -> 0xb93
   GetStencilFail -> 0xb94
   GetStencilPassDepthFail -> 0xb95
   GetStencilPassDepthPass -> 0xb96
   GetStencilRef -> 0xb97
   GetStencilWritemask -> 0xb98
   GetMatrixMode -> 0xba0
   GetNormalize -> 0xba1
   GetViewport -> 0xba2
   GetModelviewStackDepth -> 0xba3
   GetProjectionStackDepth -> 0xba4
   GetTextureStackDepth -> 0xba5
   GetModelviewMatrix -> 0xba6
   GetProjectionMatrix -> 0xba7
   GetTextureMatrix -> 0xba8
   GetAttribStackDepth -> 0xbb0
   GetClientAttribStackDepth -> 0xbb1
   GetAlphaTest -> 0xbc0
   GetAlphaTestFunc -> 0xbc1
   GetAlphaTestRef -> 0xbc2
   GetDither -> 0xbd0
   GetBlendDst -> 0xbe0
   GetBlendSrc -> 0xbe1
   GetBlend -> 0xbe2
   GetLogicOpMode -> 0xbf0
   GetIndexLogicOp -> 0xbf1
   GetLogicOp -> 0xbf1
   GetColorLogicOp -> 0xbf2
   GetAuxBuffers -> 0xc00
   GetDrawBuffer -> 0xc01
   GetReadBuffer -> 0xc02
   GetScissorBox -> 0xc10
   GetScissorTest -> 0xc11
   GetIndexClearValue -> 0xc20
   GetIndexWritemask -> 0xc21
   GetColorClearValue -> 0xc22
   GetColorWritemask -> 0xc23
   GetIndexMode -> 0xc30
   GetRGBAMode -> 0xc31
   GetDoublebuffer -> 0xc32
   GetStereo -> 0xc33
   GetRenderMode -> 0xc40
   GetPerspectiveCorrectionHint -> 0xc50
   GetPointSmoothHint -> 0xc51
   GetLineSmoothHint -> 0xc52
   GetPolygonSmoothHint -> 0xc53
   GetFogHint -> 0xc54
   GetGenerateMipmapHint -> 0x8192
   GetTextureCompressionHint -> 0x84ef
   GetTextureGenS -> 0xc60
   GetTextureGenT -> 0xc61
   GetTextureGenR -> 0xc62
   GetTextureGenQ -> 0xc63
   GetPixelMapIToISize -> 0xcb0
   GetPixelMapSToSSize -> 0xcb1
   GetPixelMapIToRSize -> 0xcb2
   GetPixelMapIToGSize -> 0xcb3
   GetPixelMapIToBSize -> 0xcb4
   GetPixelMapIToASize -> 0xcb5
   GetPixelMapRToRSize -> 0xcb6
   GetPixelMapGToGSize -> 0xcb7
   GetPixelMapBToBSize -> 0xcb8
   GetPixelMapAToASize -> 0xcb9
   GetUnpackSwapBytes -> 0xcf0
   GetUnpackLSBFirst -> 0xcf1
   GetUnpackRowLength -> 0xcf2
   GetUnpackSkipRows -> 0xcf3
   GetUnpackSkipPixels -> 0xcf4
   GetUnpackAlignment -> 0xcf5
   GetPackSwapBytes -> 0xd00
   GetPackLSBFirst -> 0xd01
   GetPackRowLength -> 0xd02
   GetPackSkipRows -> 0xd03
   GetPackSkipPixels -> 0xd04
   GetPackAlignment -> 0xd05
   GetMapColor -> 0xd10
   GetMapStencil -> 0xd11
   GetIndexShift -> 0xd12
   GetIndexOffset -> 0xd13
   GetRedScale -> 0xd14
   GetRedBias -> 0xd15
   GetZoomX -> 0xd16
   GetZoomY -> 0xd17
   GetGreenScale -> 0xd18
   GetGreenBias -> 0xd19
   GetBlueScale -> 0xd1a
   GetBlueBias -> 0xd1b
   GetAlphaScale -> 0xd1c
   GetAlphaBias -> 0xd1d
   GetDepthScale -> 0xd1e
   GetDepthBias -> 0xd1f
   GetMaxEvalOrder -> 0xd30
   GetMaxLights -> 0xd31
   GetMaxClipPlanes -> 0xd32
   GetMaxTextureSize -> 0xd33
   GetMaxPixelMapTable -> 0xd34
   GetMaxAttribStackDepth -> 0xd35
   GetMaxModelviewStackDepth -> 0xd36
   GetMaxNameStackDepth -> 0xd37
   GetMaxProjectionStackDepth -> 0xd38
   GetMaxTextureStackDepth -> 0xd39
   GetMaxViewportDims -> 0xd3a
   GetMaxClientAttribStackDepth -> 0xd3b
   GetSubpixelBits -> 0xd50
   GetIndexBits -> 0xd51
   GetRedBits -> 0xd52
   GetGreenBits -> 0xd53
   GetBlueBits -> 0xd54
   GetAlphaBits -> 0xd55
   GetDepthBits -> 0xd56
   GetStencilBits -> 0xd57
   GetAccumRedBits -> 0xd58
   GetAccumGreenBits -> 0xd59
   GetAccumBlueBits -> 0xd5a
   GetAccumAlphaBits -> 0xd5b
   GetNameStackDepth -> 0xd70
   GetAutoNormal -> 0xd80
   GetMap1Color4 -> 0xd90
   GetMap1Index -> 0xd91
   GetMap1Normal -> 0xd92
   GetMap1TextureCoord1 -> 0xd93
   GetMap1TextureCoord2 -> 0xd94
   GetMap1TextureCoord3 -> 0xd95
   GetMap1TextureCoord4 -> 0xd96
   GetMap1Vertex3 -> 0xd97
   GetMap1Vertex4 -> 0xd98
   GetMap2Color4 -> 0xdb0
   GetMap2Index -> 0xdb1
   GetMap2Normal -> 0xdb2
   GetMap2TextureCoord1 -> 0xdb3
   GetMap2TextureCoord2 -> 0xdb4
   GetMap2TextureCoord3 -> 0xdb5
   GetMap2TextureCoord4 -> 0xdb6
   GetMap2Vertex3 -> 0xdb7
   GetMap2Vertex4 -> 0xdb8
   GetMap1GridDomain -> 0xdd0
   GetMap1GridSegments -> 0xdd1
   GetMap2GridDomain -> 0xdd2
   GetMap2GridSegments -> 0xdd3
   GetTexture1D -> 0xde0
   GetTexture2D -> 0xde1
   GetFeedbackBufferSize -> 0xdf1
   GetFeedbackBufferType -> 0xdf2
   GetSelectionBufferSize -> 0xdf4
   GetPolygonOffsetUnits -> 0x2a00
   GetPolygonOffsetPoint -> 0x2a01
   GetPolygonOffsetLine -> 0x2a02
   GetPolygonOffsetFill -> 0x8037
   GetPolygonOffsetFactor -> 0x8038
   GetTextureBinding1D -> 0x8068
   GetTextureBinding2D -> 0x8069
   GetTextureBinding3D -> 0x806a
   GetVertexArray -> 0x8074
   GetNormalArray -> 0x8075
   GetColorArray -> 0x8076
   GetIndexArray -> 0x8077
   GetTextureCoordArray -> 0x8078
   GetEdgeFlagArray -> 0x8079
   GetFogCoordArray -> 0x8457
   GetSecondaryColorArray -> 0x845e
   GetMatrixIndexArray -> 0x8844
   GetVertexArraySize -> 0x807a
   GetVertexArrayType -> 0x807b
   GetVertexArrayStride -> 0x807c
   GetNormalArrayType -> 0x807e
   GetNormalArrayStride -> 0x807f
   GetColorArraySize -> 0x8081
   GetColorArrayType -> 0x8082
   GetColorArrayStride -> 0x8083
   GetIndexArrayType -> 0x8085
   GetIndexArrayStride -> 0x8086
   GetTextureCoordArraySize -> 0x8088
   GetTextureCoordArrayType -> 0x8089
   GetTextureCoordArrayStride -> 0x808a
   GetEdgeFlagArrayStride -> 0x808c
   GetFogCoordArrayType -> 0x8454
   GetFogCoordArrayStride -> 0x8455
   GetSecondaryColorArraySize -> 0x845a
   GetSecondaryColorArrayType -> 0x845b
   GetSecondaryColorArrayStride -> 0x845c
   GetMatrixIndexArraySize -> 0x8846
   GetMatrixIndexArrayType -> 0x8847
   GetMatrixIndexArrayStride -> 0x8848
   GetClipPlane i -> clipPlaneIndexToEnum i
   GetLight i -> lightIndexToEnum i
   GetTransposeModelviewMatrix -> 0x84e3
   GetTransposeProjectionMatrix -> 0x84e4
   GetTransposeTextureMatrix -> 0x84e5
   GetTransposeColorMatrix -> 0x84e6
   GetLightModelColorControl -> 0x81f8
   GetBlendColor -> 0x8005
   GetBlendEquation -> 0x8009
   GetColorTable -> 0x80d0
   GetPostConvolutionColorTable -> 0x80d1
   GetPostColorMatrixColorTable -> 0x80d2
   GetConvolution1D -> 0x8010
   GetConvolution2D -> 0x8011
   GetSeparable2D -> 0x8012
   GetMaxConvolutionWidth -> 0x801a
   GetMaxConvolutionHeight -> 0x801b
   GetPostConvolutionRedScale -> 0x801c
   GetPostConvolutionGreenScale -> 0x801d
   GetPostConvolutionBlueScale -> 0x801e
   GetPostConvolutionAlphaScale -> 0x801f
   GetPostConvolutionRedBias -> 0x8020
   GetPostConvolutionGreenBias -> 0x8021
   GetPostConvolutionBlueBias -> 0x8022
   GetPostConvolutionAlphaBias -> 0x8023
   GetHistogram -> 0x8024
   GetMinmax -> 0x802e
   GetColorSum -> 0x8458
   GetCurrentSecondaryColor -> 0x8459
   GetRescaleNormal -> 0x803a
   GetSharedTexturePalette -> 0x81fb
   GetTexture3DBinding -> 0x806a
   GetPackSkipImages -> 0x806b
   GetPackImageHeight -> 0x806c
   GetUnpackSkipImages -> 0x806d
   GetUnpackImageHeight -> 0x806e
   GetTexture3D -> 0x806f
   GetMax3DTextureSize -> 0x8073
   GetMaxTextureLodBias -> 0x84fd
   GetMaxTextureMaxAnisotropy -> 0x84ff
   GetMultisample -> 0x809d
   GetSampleAlphaToCoverage -> 0x809e
   GetSampleAlphaToOne -> 0x809f
   GetSampleCoverage -> 0x80a0
   GetSampleBuffers -> 0x80a8
   GetSamples -> 0x80a9
   GetSampleCoverageValue -> 0x80aa
   GetSampleCoverageInvert -> 0x80ab
   GetPointSizeMin -> 0x8126
   GetPointSizeMax -> 0x8127
   GetPointFadeThresholdSize -> 0x8128
   GetPointDistanceAttenuation -> 0x8129
   GetColorMatrix -> 0x80b1
   GetColorMatrixStackDepth -> 0x80b2
   GetMaxColorMatrixStackDepth -> 0x80b3
   GetPostColorMatrixRedScale -> 0x80b4
   GetPostColorMatrixGreenScale -> 0x80b5
   GetPostColorMatrixBlueScale -> 0x80b6
   GetPostColorMatrixAlphaScale -> 0x80b7
   GetPostColorMatrixRedBias -> 0x80b8
   GetPostColorMatrixGreenBias -> 0x80b9
   GetPostColorMatrixBlueBias -> 0x80ba
   GetPostColorMatrixAlphaBias -> 0x80bb
   GetMaxElementsVertices -> 0x80e8
   GetMaxElementsIndices -> 0x80e9
   GetActiveTexture -> 0x84e0
   GetClientActiveTexture -> 0x84e1
   GetMaxTextureUnits -> 0x84e2
   GetTextureCubeMap -> 0x8513
   GetMaxCubeMapTextureSize -> 0x851c
   GetNumCompressedTextureFormats -> 0x86a2
   GetCompressedTextureFormats -> 0x86a3
   GetMaxVertexUnits -> 0x86a4
   GetActiveVertexUnits -> 0x86a5
   GetWeightSumUnity -> 0x86a6
   GetVertexBlend -> 0x86a7
   GetModelview0 -> 0x1700
   GetModelview1 -> 0x850a
   GetModelview2 -> 0x8722
   GetModelview3 -> 0x8723
   GetModelview4 -> 0x8724
   GetModelview5 -> 0x8725
   GetModelview6 -> 0x8726
   GetModelview7 -> 0x8727
   GetModelview8 -> 0x8728
   GetModelview9 -> 0x8729
   GetModelview10 -> 0x872a
   GetModelview11 -> 0x872b
   GetModelview12 -> 0x872c
   GetModelview13 -> 0x872d
   GetModelview14 -> 0x872e
   GetModelview15 -> 0x872f
   GetModelview16 -> 0x8730
   GetModelview17 -> 0x8731
   GetModelview18 -> 0x8732
   GetModelview19 -> 0x8733
   GetModelview20 -> 0x8734
   GetModelview21 -> 0x8735
   GetModelview22 -> 0x8736
   GetModelview23 -> 0x8737
   GetModelview24 -> 0x8738
   GetModelview25 -> 0x8739
   GetModelview26 -> 0x873a
   GetModelview27 -> 0x873b
   GetModelview28 -> 0x873c
   GetModelview29 -> 0x873d
   GetModelview30 -> 0x873e
   GetModelview31 -> 0x873f
   GetCurrentWeight -> 0x86a8
   GetWeightArrayType -> 0x86a9
   GetWeightArrayStride -> 0x86aa
   GetWeightArraySize -> 0x86ab
   GetWeightArray -> 0x86ad
   GetMatrixPalette -> 0x8840
   GetMaxMatrixPaletteStackDepth -> 0x8841
   GetMaxPaletteMatrices -> 0x8842
   GetCurrentPaletteMatrix -> 0x8843
   GetBlendDstRGB -> 0x80c8
   GetBlendSrcRGB -> 0x80c9
   GetBlendDstAlpha -> 0x80ca
   GetBlendSrcAlpha -> 0x80cb
   GetPackCMYKHint -> 0x800e
   GetUnpackCMYKHint -> 0x800f
   GetArrayElementLockFirst -> 0x81a8
   GetArrayElementLockCount -> 0x81a9
   GetMaxShininess -> 0x8504
   GetMaxSpotExponent -> 0x8505
   GetFogDistanceMode -> 0x855a
   GetDepthBounds -> 0x8891
   GetPrimitiveRestartIndex -> 0x8559
   GetActiveStencilFace -> 0x8911

--------------------------------------------------------------------------------

-- 0x3000 through 0x3FFF are reserved for clip planes

clipPlaneIndexToEnum :: GLsizei -> GLenum
clipPlaneIndexToEnum i
   | 0 <= i && i <= 0xFFF = 0x3000 + fromIntegral i
   | otherwise = error ("clipPlaneIndexToEnum : illegal clip plane " ++ show i)

--------------------------------------------------------------------------------

-- 0x4000 through 0x4FFF are reserved for light numbers

lightIndexToEnum :: GLsizei -> GLenum
lightIndexToEnum i
   | 0 <= i && i <= 0xFFF = 0x4000 + fromIntegral i
   | otherwise = error ("clipPlaneIndexToEnum : illegal clip plane " ++ show i)

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
getBooleanv = glGetBooleanv . marshalGetPName

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
getIntegerv = glGetIntegerv . marshalGetPName

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
getFloatv = glGetFloatv . marshalGetPName

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
getDoublev = glGetDoublev . marshalGetPName

foreign import CALLCONV unsafe "glGetDoublev" glGetDoublev ::
   GLenum -> Ptr GLdouble -> IO ()

--------------------------------------------------------------------------------

data GetPointervPName =
     VertexArrayPointer
   | NormalArrayPointer
   | ColorArrayPointer
   | IndexArrayPointer
   | TextureCoordArrayPointer
   | EdgeFlagArrayPointer
   | FogCoordArrayPointer
   | SecondaryColorArrayPointer
   | FeedbackBufferPointer
   | SelectionBufferPointer
   | WeightArrayPointer
   | MatrixIndexArrayPointer

marshalGetPointervPName :: GetPointervPName -> GLenum
marshalGetPointervPName x = case x of
   VertexArrayPointer -> 0x808e
   NormalArrayPointer -> 0x808f
   ColorArrayPointer -> 0x8090
   IndexArrayPointer -> 0x8091
   TextureCoordArrayPointer -> 0x8092
   EdgeFlagArrayPointer -> 0x8093
   FogCoordArrayPointer -> 0x8456
   SecondaryColorArrayPointer -> 0x845d
   FeedbackBufferPointer -> 0xdf0
   SelectionBufferPointer -> 0xdf3
   WeightArrayPointer -> 0x86ac
   MatrixIndexArrayPointer -> 0x8849

--------------------------------------------------------------------------------

getPointer :: GetPointervPName -> IO (Ptr a)
getPointer n = alloca $ \buf -> do
   glGetPointerv (marshalGetPointervPName n) buf
   peek buf

foreign import CALLCONV unsafe "glGetPointerv" glGetPointerv ::
   GLenum -> Ptr (Ptr a) -> IO ()

--------------------------------------------------------------------------------

-- Something like this in the Foreign libs would be handy...
getArrayWith :: Storable a => ([a] -> b) -> Int -> (Ptr a -> IO c) -> IO b
getArrayWith f size act =
   allocaArray size $ \buf -> do
      act buf
      liftM f $ peekArray size buf
