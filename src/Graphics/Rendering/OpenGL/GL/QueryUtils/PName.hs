{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.QueryUtils.PName
-- Copyright   :  (c) Sven Panne 2002-2019, Lars Corbijn 2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>, Jason Dagit <dagitj@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.QueryUtils.PName (
    GetPName(..),
    GetPName1I(..), GetPName1F(..), GetIPName1I(..),
    GetPName2I(..), GetPName2F(..),
    GetPName3I(..), GetPName3F(..),
    GetPName4I(..), GetPName4F(..), GetIPName4I(..),
    GetPNameNI(..),

    PName1I(..), PName1F(..), IPName1I(..),
    PName2I(..), PName2F(..),
    PName3F(..),
    PName4I(..), PName4F(..), PName4ISemiIndexed(..),
    PNameNI(..),

    GetPNameMatrix(..),
    PNameMatrix(..),

    clipPlaneIndexToEnum,

    GetPointervPName(..), getPointer
) where

import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( allocaArray, peekArray )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr, nullPtr, castPtr )
import Foreign.Storable ( Storable(peek) )
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.GL

-----------------------------------------------------------------------------

class GetPName p where
    marshalGetPName :: p -> Maybe GLenum

-----------------------------------------------------------------------------

getBooleanv :: GetPName p => p-> Ptr GLboolean -> IO ()
getBooleanv = makeGetter glGetBooleanv

getIntegerv :: GetPName p => p -> Ptr GLint -> IO ()
getIntegerv = makeGetter glGetIntegerv

getInteger64v :: GetPName p => p -> Ptr GLint64 -> IO ()
getInteger64v = makeGetter glGetInteger64v

getFloatv :: GetPName p => p -> Ptr GLfloat -> IO ()
getFloatv = makeGetter glGetFloatv

getDoublev :: GetPName p => p -> Ptr GLdouble -> IO ()
getDoublev = makeGetter glGetDoublev

-----------------------------------------------------------------------------

getBooleaniv :: GetPName p => p -> GLuint -> Ptr GLboolean -> IO ()
getBooleaniv p i = makeGetter (\e -> glGetBooleani_v e i) p

getIntegeriv :: GetPName p => p -> GLuint -> Ptr GLint -> IO ()
getIntegeriv p i =  makeGetter (\e -> glGetIntegeri_v e i) p

getInteger64iv :: GetPName p => p -> GLuint -> Ptr GLint64 -> IO ()
getInteger64iv p i =  makeGetter (\e -> glGetInteger64i_v e i) p

-----------------------------------------------------------------------------

{-# INLINE makeGetter #-}
makeGetter :: GetPName p => (GLenum -> Ptr a -> IO ()) -> p -> Ptr a -> IO ()
makeGetter f = maybe (const recordInvalidEnum) f . marshalGetPName

-----------------------------------------------------------------------------

class GetPName p => GetPName1I p where
    getBoolean1 :: (GLboolean -> a) -> p -> IO a
    getBoolean1 = get1 getBooleanv

    getInteger1 :: (GLint -> a) -> p -> IO a
    getInteger1 = get1 getIntegerv

    getEnum1 :: (GLenum -> a) -> p -> IO a
    getEnum1 = get1 getIntegerv

    getSizei1 :: (GLsizei -> a) -> p -> IO a
    getSizei1 = get1 getIntegerv

    getInteger64 :: (GLint64 -> a) -> p -> IO a
    getInteger64 = get1 getInteger64v

class GetPName p => GetPName1F p where
    getFloat1 :: (GLfloat -> a) -> p -> IO a
    getFloat1 = get1 getFloatv

    getClampf1 :: (GLclampf -> a) -> p -> IO a
    getClampf1 = get1 getFloatv

    getDouble1 :: (GLdouble -> a) -> p -> IO a
    getDouble1 = get1 getDoublev

    getClampd1 :: (GLclampd -> a) -> p -> IO a
    getClampd1 = get1 getDoublev

-- | Helper function for the get*1 functions.
get1 :: (Storable b, Storable c)
    => (p -> Ptr c -> IO ())
    -> (b -> a) -- ^ Conversion from the casted value to the return value
    -> p -> IO a
get1 g f n = alloca $ \buf -> do
    g n buf
    peek1 f (castPtr buf)

class GetPName p => GetIPName1I p where
    getBoolean1i :: (GLboolean -> a) -> p -> GLuint -> IO a
    getBoolean1i = get1i getBooleaniv

    getInteger1i :: (GLint -> a) -> p -> GLuint -> IO a
    getInteger1i = get1i getIntegeriv

    getEnum1i :: (GLenum -> a) -> p -> GLuint -> IO a
    getEnum1i = get1i getIntegeriv

    getSizei1i :: (GLsizei -> a) -> p -> GLuint -> IO a
    getSizei1i = get1i getIntegeriv

    getInteger641i :: (GLint64 -> a) -> p -> GLuint -> IO a
    getInteger641i = get1i getInteger64iv

-- Indexed helper
get1i :: (Storable b, Storable c)
    => (p -> GLuint -> Ptr c -> IO ())
    -> (b -> a) -- ^ Conversion from the casted value to the return value
    -> p -> GLuint -> IO a
get1i g f n i = alloca $ \buf -> do
    g n i buf
    peek1 f (castPtr buf)

-----------------------------------------------------------------------------

class GetPName p => GetPName2I p where
    getBoolean2 :: (GLboolean -> GLboolean -> a) -> p -> IO a
    getBoolean2 = get2 getBooleanv

    getInteger2 :: (GLint -> GLint -> a) -> p -> IO a
    getInteger2 = get2 getIntegerv

    getEnum2 :: (GLenum -> GLenum -> a) -> p -> IO a
    getEnum2 = get2 getIntegerv

    getSizei2 :: (GLsizei -> GLsizei -> a) -> p -> IO a
    getSizei2 = get2 getIntegerv

class GetPName p => GetPName2F p where
    getFloat2 :: (GLfloat -> GLfloat -> a) -> p -> IO a
    getFloat2 = get2 getFloatv

    getClampf2 :: (GLclampf -> GLclampf -> a) -> p -> IO a
    getClampf2 = get2 getFloatv

    getDouble2 :: (GLdouble -> GLdouble -> a) -> p -> IO a
    getDouble2 = get2 getDoublev

    getClampd2 :: (GLclampd -> GLclampd -> a) -> p -> IO a
    getClampd2 = get2 getDoublev

-- | Helper function for the get*2 functions.
get2 :: (Storable b, Storable c)
    => (p -> Ptr c -> IO ())
    -> (b -> b -> a) -- ^ Conversion from the casted value to the return value
    -> p -> IO a
get2 g f n = allocaArray 2 $ \buf -> do
    g n buf
    peek2 f (castPtr buf)

-----------------------------------------------------------------------------

class GetPName p => GetPName3I p where
    getBoolean3 :: (GLboolean -> GLboolean -> GLboolean -> a) -> p -> IO a
    getBoolean3 = get3 getBooleanv

    getInteger3 :: (GLint -> GLint -> GLint -> a) -> p -> IO a
    getInteger3 = get3 getIntegerv

    getEnum3 :: (GLenum -> GLenum -> GLenum -> a) -> p -> IO a
    getEnum3 = get3 getIntegerv

    getSizei3 :: (GLsizei -> GLsizei -> GLsizei -> a) -> p -> IO a
    getSizei3 = get3 getIntegerv

class GetPName p => GetPName3F p where
    getFloat3 :: (GLfloat -> GLfloat -> GLfloat -> a) -> p -> IO a
    getFloat3 = get3 getFloatv

    getClampf3 :: (GLclampf -> GLclampf -> GLclampf -> a) -> p -> IO a
    getClampf3 = get3 getFloatv

    getDouble3 :: (GLdouble -> GLdouble -> GLdouble -> a) -> p -> IO a
    getDouble3 = get3 getDoublev

    getClampd3 :: (GLclampd -> GLclampd -> GLclampd -> a) -> p -> IO a
    getClampd3 = get3 getDoublev

-- | Helper function for the get*3 functions.
get3 :: (Storable b, Storable c)
    => (p -> Ptr c -> IO ())
    -> (b -> b -> b -> a) -- ^ Conversion from the casted value to the return value
    -> p -> IO a
get3 g f n = allocaArray 3 $ \buf -> do
    g n buf
    peek3 f (castPtr buf)

-----------------------------------------------------------------------------

class GetPName p => GetPName4I p where
    getBoolean4 :: (GLboolean -> GLboolean -> GLboolean -> GLboolean -> a) -> p -> IO a
    getBoolean4 = get4 getBooleanv

    getInteger4 :: (GLint -> GLint -> GLint -> GLint -> a) -> p -> IO a
    getInteger4 = get4 getIntegerv

    getEnum4 :: (GLenum -> GLenum -> GLenum -> GLenum -> a) -> p -> IO a
    getEnum4 = get4 getIntegerv

    getSizei4 :: (GLsizei -> GLsizei -> GLsizei -> GLsizei -> a) -> p -> IO a
    getSizei4 = get4 getIntegerv

class GetPName p => GetPName4F p where
    getFloat4 :: (GLfloat -> GLfloat -> GLfloat -> GLfloat -> a) -> p -> IO a
    getFloat4 = get4 getFloatv

    getClampf4 :: (GLclampf -> GLclampf -> GLclampf -> GLclampf -> a) -> p -> IO a
    getClampf4 = get4 getFloatv

    getDouble4 :: (GLdouble -> GLdouble -> GLdouble -> GLdouble -> a) -> p -> IO a
    getDouble4 = get4 getDoublev

    getClampd4 :: (GLclampd -> GLclampd -> GLclampd -> GLclampd -> a) -> p -> IO a
    getClampd4 = get4 getDoublev

-- | Helper function for the get*4 functions.
get4 :: (Storable b, Storable c)
    => (p -> Ptr c -> IO ())
    -> (b -> b -> b -> b -> a) -- ^ Conversion from the casted value to the return value
    -> p -> IO a
get4 g f n = allocaArray 4 $ \buf -> do
    g n buf
    peek4 f (castPtr buf)

class GetPName p => GetIPName4I p where
    getBoolean4i :: (GLboolean -> GLboolean -> GLboolean -> GLboolean -> a) -> p -> GLuint -> IO a
    getBoolean4i = get4i getBooleaniv

    getInteger4i :: (GLint -> GLint -> GLint -> GLint -> a) -> p -> GLuint -> IO a
    getInteger4i = get4i getIntegeriv

    getEnum4i :: (GLenum -> GLenum -> GLenum -> GLenum -> a) -> p -> GLuint -> IO a
    getEnum4i = get4i getIntegeriv

    getSizei4i :: (GLsizei -> GLsizei -> GLsizei -> GLsizei -> a) -> p -> GLuint -> IO a
    getSizei4i = get4i getIntegeriv

-- | Helper function for the get*4 functions.
get4i :: (Storable b, Storable c)
    => (p -> GLuint -> Ptr c -> IO ())
    -> (b -> b -> b -> b -> a) -- ^ Conversion from the casted value to the return value
    -> p -> GLuint -> IO a
get4i g f n i = allocaArray 4 $ \buf -> do
    g n i buf
    peek4 f (castPtr buf)

class GetPName p => GetPNameNI p where
   getEnumN :: (GLenum -> a) -> p -> Int -> IO [a]
   getEnumN f p n =
      allocaArray n $ \buf -> do
         getIntegerv p buf
         (map (f . fromIntegral)) `fmap` peekArray n buf

-----------------------------------------------------------------------------

class GetPName p => GetPNameMatrix p where
    getMatrixf :: p -> Ptr GLfloat -> IO ()
    getMatrixf = getFloatv
    getMatrixd :: p -> Ptr GLdouble -> IO ()
    getMatrixd = getDoublev

-----------------------------------------------------------------------------

data PName1I
    = GetEdgeFlag           -- ^ bool
    | GetRGBAMode           -- ^ enum
    | GetCurrentIndex       -- ^ int
    | GetMaxTextureUnits    -- ^ enum
    -- displaylist
    | GetListIndex          -- ^ enum
    | GetListMode           -- ^ enum
    | GetMaxListNesting     -- ^ sizei
    | GetListBase           -- ^ enum
    -- rendermode
    | GetRenderMode         -- ^ enum
    -- framebufferbinding
    | GetDrawFramebufferBinding -- ^ int
    | GetReadFramebufferBinding -- ^ int
    | GetFramebufferBinding     -- ^ int
    -- renderbufferbinding
    | GetRenderbufferBinding    -- ^ int
    -- hint
    | GetPerspectiveCorrectionHint -- ^ enum
    | GetPointSmoothHint        -- ^ enum
    | GetLineSmoothHint         -- ^ enum
    | GetPolygonSmoothHint      -- ^ enum
    | GetFogHint                -- ^ enum
    | GetGenerateMipmapHint     -- ^ enum
    | GetTextureCompressionHint -- ^ enum
    | GetPackCMYKHint           -- ^ enum
    | GetUnpackCMYKHint         -- ^ enum
    -- vertexarray
    | GetVertexArrayBinding     -- ^ int
    -- Selction?
    | GetMaxNameStackDepth      -- ^ int
    | GetNameStackDepth         -- ^ int
    -- ContextProfile
    | GetContextProfileMask     -- ^ enum
    -- pixelStorage
    | GetPackSwapBytes      -- ^ bool
    | GetUnpackSwapBytes    -- ^ bool
    | GetPackLSBFirst       -- ^ bool
    | GetUnpackLSBFirst     -- ^ bool
    | GetPackRowLength      -- ^ int
    | GetUnpackRowLength    -- ^ int
    | GetPackSkipRows       -- ^ int
    | GetUnpackSkipRows     -- ^ int
    | GetPackSkipPixels     -- ^ int
    | GetUnpackSkipPixels   -- ^ int
    | GetPackAlignment      -- ^ int
    | GetUnpackAlignment    -- ^ int
    | GetPackImageHeight    -- ^ int
    | GetUnpackImageHeight  -- ^ int
    | GetPackSkipImages     -- ^ int
    | GetUnpackSkipImages   -- ^ int
    -- pixel map
    | GetPixelMapIToISize   -- ^ int
    | GetPixelMapSToSSize   -- ^ int
    | GetPixelMapIToRSize   -- ^ int
    | GetPixelMapIToGSize   -- ^ int
    | GetPixelMapIToBSize   -- ^ int
    | GetPixelMapIToASize   -- ^ int
    | GetPixelMapRToRSize   -- ^ int
    | GetPixelMapGToGSize   -- ^ int
    | GetPixelMapBToBSize   -- ^ int
    | GetPixelMapAToASize   -- ^ int
    | GetMaxPixelMapTable   -- ^ sizei
    -- shader limits
    | GetMaxVertexTextureImageUnits     -- ^ sizei
    | GetMaxTextureImageUnits           -- ^ sizei
    | GetMaxCombinedTextureImageUnits   -- ^ sizei
    | GetMaxTextureCoords               -- ^ sizei
    | GetMaxVertexUniformComponents     -- ^ sizei
    | GetMaxFragmentUniformComponents   -- ^ sizei
    | GetMaxVertexAttribs               -- ^ sizei
    | GetMaxVaryingFloats               -- ^ sizei
    -- tessellation
    | GetPatchVertices                  -- ^ sizei
    | GetMaxPatchVertices               -- ^ sizei
    | GetMaxTessGenLevel                -- ^ sizei
    -- coordtrans
    | GetMatrixMode                 -- ^ enum
    | GetModelviewStackDepth        -- ^ sizei
    | GetProjectionStackDepth       -- ^ sizei
    | GetTextureStackDepth          -- ^ sizei
    | GetColorMatrixStackDepth      -- ^ sizei
    | GetMaxModelviewStackDepth     -- ^ sizei
    | GetMaxProjectionStackDepth    -- ^ sizei
    | GetMaxTextureStackDepth       -- ^ sizei
    | GetMaxColorMatrixStackDepth   -- ^ sizei
    | GetMaxMatrixPaletteStackDepth -- ^ sizei
    | GetCurrentMatrixStackDepth    -- ^ sizei
    | GetActiveTexture              -- ^ enum
    -- VertexArrays
    | GetVertexArraySize    -- ^ int
    | GetVertexArrayType    -- ^ enum
    | GetVertexArrayStride  -- ^ int
    | GetNormalArrayType    -- ^ enum
    | GetNormalArrayStride  -- ^ int
    | GetColorArraySize     -- ^ int
    | GetColorArrayType     -- ^ enum
    | GetColorArrayStride   -- ^ int
    | GetIndexArrayType     -- ^ enum
    | GetIndexArrayStride   -- ^ int
    | GetTextureCoordArraySize      -- ^ int
    | GetTextureCoordArrayType      -- ^ enum
    | GetTextureCoordArrayStride    -- ^ int
    | GetEdgeFlagArrayStride        -- ^ int
    | GetFogCoordArrayType          -- ^ enum
    | GetFogCoordArrayStride        -- ^ int
    | GetSecondaryColorArraySize    -- ^ int
    | GetSecondaryColorArrayType    -- ^ enum
    | GetSecondaryColorArrayStride  -- ^ int
    | GetArrayElementLockCount      -- ^ int
    | GetArrayElementLockFirst      -- ^ int
    | GetClientActiveTexture        -- ^ enum
    | GetMaxElementsVertices        -- ^ sizei
    | GetMaxElementsIndices         -- ^ sizei
    | GetPrimitiveRestartIndex      -- ^ int
    | GetPrimitiveRestartNV         -- ^ bool
    | GetPrimitiveRestartIndexNV    -- ^ int
    -- bufferObjects
    | GetArrayBufferBinding         -- ^ int
    | GetAtomicCounterBufferBinding -- ^ int
    | GetCopyReadBufferBinding      -- ^ int
    | GetCopyWriteBufferBinding     -- ^ int
    | GetDispatchIndirectBufferBinding -- ^ int
    | GetDrawIndirectBufferBinding  -- ^ int
    | GetElementArrayBufferBinding  -- ^ int
    | GetPixelPackBufferBinding     -- ^ int
    | GetPixelUnpackBufferBinding   -- ^ int
    | GetQueryBufferBinding         -- ^ int
    | GetShaderStorageBufferBinding -- ^ int
    | GetTransformFeedbackBufferBinding -- ^ int
    | GetUniformBufferBinding       -- ^ int

    | GetVertexArrayBufferBinding   -- ^ int
    | GetNormalArrayBufferBinding   -- ^ int
    | GetColorArrayBufferBinding    -- ^ int
    | GetIndexArrayBufferBinding    -- ^ int
    | GetTextureCoordArrayBufferBinding -- ^ int
    | GetEdgeFlagArrayBufferBinding -- ^ int
    | GetSecondaryColorArrayBufferBinding   -- ^ int
    | GetFogCoordArrayBufferBinding -- ^ int
    -- clipping
    | GetMaxClipPlanes          -- ^ sizei
    -- Colors
    | GetMaxLights              -- ^ sizei
    | GetFrontFace              -- ^ enum
    | GetLightModelLocalViewer  -- ^ bool
    | GetLightModelTwoSide      -- ^ bool
    | GetLightModelColorControl -- ^ enum
    | GetColorMaterialFace      -- ^ enum
    | GetColorMaterialParameter -- ^ enum
    | GetShadeModel             -- ^ enum
    | GetFragmentColorClamp     -- ^ enum
    | GetVertexColorClamp       -- ^ enum
    | GetReadColorClamp         -- ^ enum
    -- Evaluators
    | GetMaxEvalOrder       -- ^ int
    | GetMap1GridSegments   -- ^ int
    -- Fog
    | GetFogMode            -- ^ int => enum
    | GetFogIndex           -- ^ int
    | GetFogCoordSrc        -- ^ int
    | GetFogDistanceMode    -- ^ int => enum
    -- Framebuffer
    | GetAuxBuffers         -- ^ sizei
    | GetDoublebuffer       -- ^ bool
    | GetStereo             -- ^ bool
    | GetRedBits            -- ^ sizei
    | GetGreenBits          -- ^ sizei
    | GetBlueBits           -- ^ sizei
    | GetAlphaBits          -- ^ sizei
    | GetStencilBits        -- ^ sizei
    | GetDepthBits          -- ^ sizei
    | GetAccumRedBits       -- ^ sizei
    | GetAccumGreenBits     -- ^ sizei
    | GetAccumBlueBits      -- ^ sizei
    | GetAccumAlphaBits     -- ^ sizei
    | GetDrawBuffer         -- ^ enum
    | GetDrawBufferN GLsizei -- enum
    | GetMaxDrawBuffers     -- ^ sizei
    | GetIndexWritemask     -- ^ int
    | GetDepthWritemask     -- ^ bool
    | GetStencilWritemask   -- ^ bool
    | GetStencilBackWritemask   -- ^ bool
    | GetStencilClearValue  -- ^ int
    -- Program
    | GetCurrentProgram     -- ^ int
    -- Transformfeedback
    | GetMaxTransformFeedbackSeparateAttribs        -- ^ int
    | GetMaxTransformFeedbackInterleavedComponents  -- ^ int
    | GetMaxTransformFeedbackSeparateComponents     -- ^ int
    | GetCurrentRasterIndex         -- ^ int
    | GetCurrentRasterPositionValid -- ^ bool
    -- LineSegment
    | GetLineStippleRepeat          -- ^ int
    | GetLineStipplePattern         -- ^ int
    -- PerFragment
    | GetSampleCoverageInvert       -- ^ bool
    | GetAlphaTestFunc              -- ^ enum
    | GetStencilFunc                -- ^ enum
    | GetStencilBackFunc            -- ^ enum
    | GetStencilValueMask           -- ^ int
    | GetStencilBackValueMask       -- ^ int
    | GetStencilRef                 -- ^ int
    | GetStencilBackRef             -- ^ int
    | GetStencilFail                -- ^ enum
    | GetStencilBackFail            -- ^ enum
    | GetStencilPassDepthFail       -- ^ enum
    | GetStencilBackPassDepthFail   -- ^ enum
    | GetStencilPassDepthPass       -- ^ enum
    | GetStencilBackPassDepthPass   -- ^ enum
    | GetActiveStencilFace          -- ^ enum
    | GetLogicOpMode                -- ^ enum
    | GetBlendDst                   -- ^ enum
    | GetBlendSrc                   -- ^ enum
    | GetBlendSrcRGB                -- ^ enum
    | GetBlendSrcAlpha              -- ^ enum
    | GetBlendDstRGB                -- ^ enum
    | GetBlendDstAlpha              -- ^ enum
    | GetBlendEquation              -- ^ enum
    | GetBlendEquationAlpha         -- ^ enum
    | GetDepthFunc                  -- ^ enum
    | GetMapColor                   -- ^ bool
    | GetMapStencil                 -- ^ bool
    | GetIndexShift                 -- ^ int
    | GetIndexOffset                -- ^ int
    -- Polygons                     -- ^ enum
    | GetCullFaceMode
    -- TextureSpecification
    | GetNumCompressedTextureFormats    -- ^ int
    | GetMaxTextureSize                 -- ^ int
    | GetMax3DTextureSize               -- ^ int
    | GetMaxCubeMapTextureSize          -- ^ int
    | GetMaxRectangleTextureSize        -- ^ int
    | GetMaxArrayTextureLayers          -- ^ int
    | GetMaxSampleMaskWords             -- ^ int
    | GetMaxColorTextureSamples         -- ^ int
    | GetMaxDepthTextureSamples         -- ^ int
    | GetMaxIntegerSamples              -- ^ int
    -- ReadCopyPixels
    | GetReadBuffer                 -- ^ enum
    -- Texture Objects
    | GetTextureBinding1D           -- ^ int\/enum
    | GetTextureBinding2D           -- ^ int\/enum
    | GetTextureBinding3D           -- ^ int\/enum
    | GetTextureBinding1DArray      -- ^ int\/enum
    | GetTextureBinding2DArray      -- ^ int\/enum
    | GetTextureBindingCubeMapArray -- ^ int\/enum
    | GetTextureBindingRectangle    -- ^ int\/enum
    | GetTextureBindingBuffer       -- ^ int\/enum
    | GetTextureBindingCubeMap      -- ^ int\/enum
    | GetTextureBinding2DMultisample -- ^ int\/enum
    | GetTextureBinding2DMultisampleArray -- ^ int\/enum
    -- Antialiasing
    | GetSubpixelBits               -- ^ sizei
    | GetSamples                    -- ^ sizei
    | GetSampleBuffers              -- ^ sizei
    -- Sync Objects
    | GetMaxServerWaitTimeout       -- ^ int
    -- Query Objects
    | GetMaxVertexStreams           -- ^ int
    -- GL Time
    | GetTimestamp                  -- ^ int
    -- Shader
    | GetShaderCompiler             -- ^ bool
    | GetNumShaderBinaryFormats     -- ^ int
    | GetNumProgramBinaryFormats    -- ^ int
    -- Debug Output
    | GetMaxDebugMessageLength        -- ^ int
    | GetMaxDebugLoggedMessages       -- ^ int
    | GetDebugLoggedMessages          -- ^ int
    | GetDebugNextLoggedMessageLength -- ^ int
    | GetMaxDebugGroupStackDepth      -- ^ int
    | GetMaxLabelLength               -- ^ int
    -- Extensions
    | GetNumExtensions     -- ^ uint

instance GetPName1I PName1I where

instance GetPName PName1I where
    marshalGetPName pn = case pn of
        GetEdgeFlag -> Just GL_EDGE_FLAG
        GetRGBAMode -> Just GL_RGBA_MODE
        GetCurrentIndex -> Just GL_CURRENT_INDEX
        GetMaxTextureUnits -> Just GL_MAX_TEXTURE_UNITS
        -- displaylist
        GetListIndex -> Just GL_LIST_INDEX
        GetListMode -> Just GL_LIST_MODE
        GetMaxListNesting -> Just GL_MAX_LIST_NESTING
        GetListBase -> Just GL_LIST_BASE
        -- rendermode
        GetRenderMode -> Just GL_RENDER_MODE
        -- framebufferbinding
        GetDrawFramebufferBinding -> Just GL_DRAW_FRAMEBUFFER_BINDING
        GetReadFramebufferBinding -> Just GL_READ_FRAMEBUFFER_BINDING
        GetFramebufferBinding -> Just GL_FRAMEBUFFER_BINDING
        -- renderbufferbinding
        GetRenderbufferBinding -> Just GL_RENDERBUFFER_BINDING
        -- hint
        GetPerspectiveCorrectionHint -> Just GL_PERSPECTIVE_CORRECTION_HINT
        GetPointSmoothHint -> Just GL_POINT_SMOOTH_HINT
        GetLineSmoothHint -> Just GL_LINE_SMOOTH_HINT
        GetPolygonSmoothHint -> Just GL_POLYGON_SMOOTH_HINT
        GetFogHint -> Just GL_FOG_HINT
        GetGenerateMipmapHint -> Just GL_GENERATE_MIPMAP_HINT
        GetTextureCompressionHint -> Just GL_TEXTURE_COMPRESSION_HINT
        GetPackCMYKHint -> Just GL_PACK_CMYK_HINT_EXT
        GetUnpackCMYKHint -> Just GL_UNPACK_CMYK_HINT_EXT
        GetVertexArrayBinding -> Just GL_VERTEX_ARRAY_BINDING
        -- Selection ?
        GetMaxNameStackDepth -> Just GL_MAX_NAME_STACK_DEPTH
        GetNameStackDepth -> Just GL_NAME_STACK_DEPTH
        -- ContextProfile
        GetContextProfileMask -> Just GL_CONTEXT_PROFILE_MASK
        --pixel storage
        GetPackSwapBytes -> Just GL_PACK_SWAP_BYTES
        GetUnpackSwapBytes -> Just GL_UNPACK_SWAP_BYTES
        GetPackLSBFirst -> Just GL_PACK_LSB_FIRST
        GetUnpackLSBFirst -> Just GL_UNPACK_LSB_FIRST
        GetPackRowLength -> Just GL_PACK_ROW_LENGTH
        GetUnpackRowLength -> Just GL_UNPACK_ROW_LENGTH
        GetPackSkipRows -> Just GL_PACK_SKIP_ROWS
        GetUnpackSkipRows -> Just GL_UNPACK_SKIP_ROWS
        GetPackSkipPixels -> Just GL_PACK_SKIP_PIXELS
        GetUnpackSkipPixels -> Just GL_UNPACK_SKIP_PIXELS
        GetPackAlignment -> Just GL_PACK_ALIGNMENT
        GetUnpackAlignment -> Just GL_UNPACK_ALIGNMENT
        GetPackSkipImages -> Just GL_PACK_SKIP_IMAGES
        GetUnpackSkipImages -> Just GL_UNPACK_SKIP_IMAGES
        GetPackImageHeight -> Just GL_PACK_IMAGE_HEIGHT
        GetUnpackImageHeight -> Just GL_UNPACK_IMAGE_HEIGHT
        -- pixelmap
        GetPixelMapIToISize -> Just GL_PIXEL_MAP_I_TO_I_SIZE
        GetPixelMapSToSSize -> Just GL_PIXEL_MAP_S_TO_S_SIZE
        GetPixelMapIToRSize -> Just GL_PIXEL_MAP_I_TO_R_SIZE
        GetPixelMapIToGSize -> Just GL_PIXEL_MAP_I_TO_G_SIZE
        GetPixelMapIToBSize -> Just GL_PIXEL_MAP_I_TO_B_SIZE
        GetPixelMapIToASize -> Just GL_PIXEL_MAP_I_TO_A_SIZE
        GetPixelMapRToRSize -> Just GL_PIXEL_MAP_R_TO_R_SIZE
        GetPixelMapGToGSize -> Just GL_PIXEL_MAP_G_TO_G_SIZE
        GetPixelMapBToBSize -> Just GL_PIXEL_MAP_B_TO_B_SIZE
        GetPixelMapAToASize -> Just GL_PIXEL_MAP_A_TO_A_SIZE
        GetMaxPixelMapTable -> Just GL_MAX_PIXEL_MAP_TABLE
        -- shader limits
        GetMaxVertexTextureImageUnits -> Just GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS
        GetMaxTextureImageUnits -> Just GL_MAX_TEXTURE_IMAGE_UNITS
        GetMaxCombinedTextureImageUnits -> Just GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
        GetMaxTextureCoords -> Just GL_MAX_TEXTURE_COORDS
        GetMaxVertexUniformComponents -> Just GL_MAX_VERTEX_UNIFORM_COMPONENTS
        GetMaxFragmentUniformComponents -> Just GL_MAX_FRAGMENT_UNIFORM_COMPONENTS
        GetMaxVaryingFloats -> Just GL_MAX_VARYING_COMPONENTS
        GetMaxVertexAttribs -> Just GL_MAX_VERTEX_ATTRIBS
        -- tessellation
        GetPatchVertices -> Just GL_PATCH_VERTICES
        GetMaxPatchVertices -> Just GL_MAX_PATCH_VERTICES
        GetMaxTessGenLevel -> Just GL_MAX_TESS_GEN_LEVEL
        -- coordtrans
        GetMatrixMode -> Just GL_MATRIX_MODE
        GetModelviewStackDepth -> Just GL_MODELVIEW_STACK_DEPTH
        GetProjectionStackDepth -> Just GL_PROJECTION_STACK_DEPTH
        GetTextureStackDepth -> Just GL_TEXTURE_STACK_DEPTH
        GetColorMatrixStackDepth -> Just GL_COLOR_MATRIX_STACK_DEPTH
        GetMaxModelviewStackDepth -> Just GL_MAX_MODELVIEW_STACK_DEPTH
        GetMaxProjectionStackDepth -> Just GL_MAX_PROJECTION_STACK_DEPTH
        GetMaxTextureStackDepth -> Just GL_MAX_TEXTURE_STACK_DEPTH
        GetMaxColorMatrixStackDepth -> Just GL_MAX_COLOR_MATRIX_STACK_DEPTH
        GetMaxMatrixPaletteStackDepth -> Just GL_MAX_MATRIX_PALETTE_STACK_DEPTH_ARB
        GetCurrentMatrixStackDepth -> Just GL_CURRENT_MATRIX_STACK_DEPTH_ARB
        GetActiveTexture -> Just GL_ACTIVE_TEXTURE
        -- vertexarrays
        GetVertexArraySize -> Just GL_VERTEX_ARRAY_SIZE
        GetVertexArrayType -> Just GL_VERTEX_ARRAY_TYPE
        GetVertexArrayStride -> Just GL_VERTEX_ARRAY_STRIDE
        GetNormalArrayType -> Just GL_NORMAL_ARRAY_TYPE
        GetNormalArrayStride -> Just GL_NORMAL_ARRAY_STRIDE
        GetColorArraySize -> Just GL_COLOR_ARRAY_SIZE
        GetColorArrayType -> Just GL_COLOR_ARRAY_TYPE
        GetColorArrayStride -> Just GL_COLOR_ARRAY_STRIDE
        GetIndexArrayType -> Just GL_INDEX_ARRAY_TYPE
        GetIndexArrayStride -> Just GL_INDEX_ARRAY_STRIDE
        GetTextureCoordArraySize -> Just GL_TEXTURE_COORD_ARRAY_SIZE
        GetTextureCoordArrayType -> Just GL_TEXTURE_COORD_ARRAY_TYPE
        GetTextureCoordArrayStride -> Just GL_TEXTURE_COORD_ARRAY_STRIDE
        GetEdgeFlagArrayStride -> Just GL_EDGE_FLAG_ARRAY_STRIDE
        GetFogCoordArrayType -> Just GL_FOG_COORD_ARRAY_TYPE
        GetFogCoordArrayStride -> Just GL_FOG_COORD_ARRAY_STRIDE
        GetSecondaryColorArraySize -> Just GL_SECONDARY_COLOR_ARRAY_SIZE
        GetSecondaryColorArrayType -> Just GL_SECONDARY_COLOR_ARRAY_TYPE
        GetSecondaryColorArrayStride -> Just GL_SECONDARY_COLOR_ARRAY_STRIDE
        GetArrayElementLockCount -> Just GL_ARRAY_ELEMENT_LOCK_COUNT_EXT
        GetArrayElementLockFirst -> Just GL_ARRAY_ELEMENT_LOCK_FIRST_EXT
        GetClientActiveTexture -> Just GL_CLIENT_ACTIVE_TEXTURE
        GetMaxElementsVertices -> Just GL_MAX_ELEMENTS_VERTICES
        GetMaxElementsIndices -> Just GL_MAX_ELEMENTS_INDICES
        GetPrimitiveRestartIndex -> Just GL_PRIMITIVE_RESTART_INDEX
        GetPrimitiveRestartNV -> Just GL_PRIMITIVE_RESTART_NV
        GetPrimitiveRestartIndexNV -> Just GL_PRIMITIVE_RESTART_INDEX_NV
        -- bufferobjects
        GetArrayBufferBinding -> Just GL_ARRAY_BUFFER_BINDING
        GetAtomicCounterBufferBinding -> Just GL_ATOMIC_COUNTER_BUFFER_BINDING
        GetCopyReadBufferBinding -> Just GL_COPY_READ_BUFFER_BINDING
        GetCopyWriteBufferBinding -> Just GL_COPY_WRITE_BUFFER_BINDING
        GetDispatchIndirectBufferBinding -> Just GL_DISPATCH_INDIRECT_BUFFER_BINDING
        GetDrawIndirectBufferBinding -> Just GL_DRAW_INDIRECT_BUFFER_BINDING
        GetElementArrayBufferBinding -> Just GL_ELEMENT_ARRAY_BUFFER_BINDING
        GetPixelPackBufferBinding -> Just GL_PIXEL_PACK_BUFFER_BINDING
        GetPixelUnpackBufferBinding -> Just GL_PIXEL_UNPACK_BUFFER_BINDING
        GetQueryBufferBinding -> Just GL_QUERY_BUFFER_BINDING
        GetShaderStorageBufferBinding -> Just GL_SHADER_STORAGE_BUFFER_BINDING
        GetTransformFeedbackBufferBinding -> Just GL_TRANSFORM_FEEDBACK_BUFFER_BINDING
        GetUniformBufferBinding -> Just GL_UNIFORM_BUFFER_BINDING

        GetVertexArrayBufferBinding -> Just GL_VERTEX_ARRAY_BUFFER_BINDING
        GetNormalArrayBufferBinding -> Just GL_NORMAL_ARRAY_BUFFER_BINDING
        GetColorArrayBufferBinding -> Just GL_COLOR_ARRAY_BUFFER_BINDING
        GetIndexArrayBufferBinding -> Just GL_INDEX_ARRAY_BUFFER_BINDING
        GetTextureCoordArrayBufferBinding -> Just GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING
        GetEdgeFlagArrayBufferBinding -> Just GL_EDGE_FLAG_ARRAY_BUFFER_BINDING
        GetSecondaryColorArrayBufferBinding -> Just GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING
        GetFogCoordArrayBufferBinding -> Just GL_FOG_COORD_ARRAY_BUFFER_BINDING
        -- clipping
        GetMaxClipPlanes -> Just GL_MAX_CLIP_DISTANCES
        -- Colors
        GetMaxLights -> Just GL_MAX_LIGHTS
        GetFrontFace -> Just GL_FRONT_FACE
        GetLightModelLocalViewer -> Just GL_LIGHT_MODEL_LOCAL_VIEWER
        GetLightModelTwoSide -> Just GL_LIGHT_MODEL_TWO_SIDE
        GetLightModelColorControl -> Just GL_LIGHT_MODEL_COLOR_CONTROL
        GetColorMaterialFace -> Just GL_COLOR_MATERIAL_FACE
        GetColorMaterialParameter -> Just GL_COLOR_MATERIAL_PARAMETER
        GetShadeModel -> Just GL_SHADE_MODEL
        GetVertexColorClamp -> Just GL_CLAMP_VERTEX_COLOR
        GetFragmentColorClamp -> Just GL_CLAMP_FRAGMENT_COLOR
        GetReadColorClamp -> Just GL_CLAMP_READ_COLOR
        -- Evaluators
        GetMaxEvalOrder -> Just GL_MAX_EVAL_ORDER
        GetMap1GridSegments -> Just GL_MAP1_GRID_SEGMENTS
        -- Fog
        GetFogMode -> Just GL_FOG_MODE
        GetFogIndex -> Just GL_FOG_INDEX
        GetFogCoordSrc -> Just GL_FOG_COORD_SRC
        GetFogDistanceMode -> Just GL_FOG_DISTANCE_MODE_NV
        -- Framebuffer
        GetAuxBuffers -> Just GL_AUX_BUFFERS
        GetDoublebuffer -> Just GL_DOUBLEBUFFER
        GetStereo -> Just GL_STEREO
        GetRedBits -> Just GL_RED_BITS
        GetGreenBits -> Just GL_GREEN_BITS
        GetBlueBits -> Just GL_BLUE_BITS
        GetAlphaBits -> Just GL_ALPHA_BITS
        GetDepthBits -> Just GL_DEPTH_BITS
        GetStencilBits -> Just GL_STENCIL_BITS
        GetAccumRedBits -> Just GL_ACCUM_RED_BITS
        GetAccumGreenBits -> Just GL_ACCUM_GREEN_BITS
        GetAccumBlueBits -> Just GL_ACCUM_BLUE_BITS
        GetAccumAlphaBits -> Just GL_ACCUM_ALPHA_BITS
        GetDrawBuffer -> Just GL_DRAW_BUFFER
        GetDrawBufferN i -> drawBufferIndexToEnum i
        GetMaxDrawBuffers -> Just GL_MAX_DRAW_BUFFERS
        GetIndexWritemask -> Just GL_INDEX_WRITEMASK
        GetDepthWritemask -> Just GL_DEPTH_WRITEMASK
        GetStencilWritemask -> Just GL_STENCIL_WRITEMASK
        GetStencilBackWritemask -> Just GL_STENCIL_BACK_WRITEMASK
        GetStencilClearValue -> Just GL_STENCIL_CLEAR_VALUE
        -- Program
        GetCurrentProgram -> Just GL_CURRENT_PROGRAM
        -- Transformfeedback
        GetMaxTransformFeedbackSeparateAttribs -> Just GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS
        GetMaxTransformFeedbackSeparateComponents -> Just GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS
        GetMaxTransformFeedbackInterleavedComponents -> Just GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS
        -- RasterPos
        GetCurrentRasterIndex -> Just GL_CURRENT_RASTER_INDEX
        GetCurrentRasterPositionValid -> Just GL_CURRENT_RASTER_POSITION_VALID
        -- LineSegment
        GetLineStipplePattern -> Just GL_LINE_STIPPLE_PATTERN
        GetLineStippleRepeat -> Just GL_LINE_STIPPLE_REPEAT
        -- PerFragment
        GetSampleCoverageInvert -> Just GL_SAMPLE_COVERAGE_INVERT
        GetAlphaTestFunc -> Just GL_ALPHA_TEST_FUNC
        GetStencilFunc -> Just GL_STENCIL_FUNC
        GetStencilBackFunc -> Just GL_STENCIL_BACK_FUNC
        GetStencilValueMask -> Just GL_STENCIL_VALUE_MASK
        GetStencilBackValueMask -> Just GL_STENCIL_BACK_VALUE_MASK
        GetStencilRef -> Just GL_STENCIL_REF
        GetStencilBackRef -> Just GL_STENCIL_BACK_REF
        GetStencilFail -> Just GL_STENCIL_FAIL
        GetStencilBackFail -> Just GL_STENCIL_BACK_FAIL
        GetStencilPassDepthFail -> Just GL_STENCIL_PASS_DEPTH_FAIL
        GetStencilBackPassDepthFail -> Just GL_STENCIL_BACK_PASS_DEPTH_FAIL
        GetStencilPassDepthPass -> Just GL_STENCIL_PASS_DEPTH_PASS
        GetStencilBackPassDepthPass -> Just GL_STENCIL_BACK_PASS_DEPTH_PASS
        GetActiveStencilFace -> Just GL_ACTIVE_STENCIL_FACE_EXT
        GetLogicOpMode -> Just GL_LOGIC_OP_MODE
        GetBlendDst -> Just GL_BLEND_DST
        GetBlendSrc -> Just GL_BLEND_SRC
        GetBlendDstRGB -> Just GL_BLEND_DST_RGB
        GetBlendSrcRGB -> Just GL_BLEND_SRC_RGB
        GetBlendDstAlpha -> Just GL_BLEND_DST_ALPHA
        GetBlendSrcAlpha -> Just GL_BLEND_SRC_ALPHA
        GetBlendEquation -> Just GL_BLEND_EQUATION_RGB
        GetBlendEquationAlpha -> Just GL_BLEND_EQUATION_ALPHA
        GetDepthFunc -> Just GL_DEPTH_FUNC
        GetMapColor -> Just GL_MAP_COLOR
        GetMapStencil -> Just GL_MAP_STENCIL
        GetIndexShift -> Just GL_INDEX_SHIFT
        GetIndexOffset -> Just GL_INDEX_OFFSET
        -- Polygons
        GetCullFaceMode -> Just GL_CULL_FACE_MODE
        -- Texture specification
        GetNumCompressedTextureFormats -> Just GL_NUM_COMPRESSED_TEXTURE_FORMATS
        GetMaxTextureSize -> Just GL_MAX_TEXTURE_SIZE
        GetMax3DTextureSize -> Just GL_MAX_3D_TEXTURE_SIZE
        GetMaxCubeMapTextureSize -> Just GL_MAX_CUBE_MAP_TEXTURE_SIZE
        GetMaxRectangleTextureSize -> Just GL_MAX_RECTANGLE_TEXTURE_SIZE
        GetMaxArrayTextureLayers -> Just GL_MAX_ARRAY_TEXTURE_LAYERS
        GetMaxSampleMaskWords -> Just GL_MAX_SAMPLE_MASK_WORDS
        GetMaxColorTextureSamples -> Just GL_MAX_COLOR_TEXTURE_SAMPLES
        GetMaxDepthTextureSamples -> Just GL_MAX_DEPTH_TEXTURE_SAMPLES
        GetMaxIntegerSamples -> Just GL_MAX_INTEGER_SAMPLES
        -- ReadCopyPixels
        GetReadBuffer -> Just GL_READ_BUFFER
        -- Texture Objects
        GetTextureBinding1D -> Just GL_TEXTURE_BINDING_1D
        GetTextureBinding2D -> Just GL_TEXTURE_BINDING_2D
        GetTextureBinding3D -> Just GL_TEXTURE_BINDING_3D
        GetTextureBinding1DArray -> Just GL_TEXTURE_BINDING_1D_ARRAY
        GetTextureBinding2DArray -> Just GL_TEXTURE_BINDING_2D_ARRAY
        GetTextureBindingCubeMapArray -> Just GL_TEXTURE_BINDING_CUBE_MAP_ARRAY
        GetTextureBindingRectangle -> Just GL_TEXTURE_BINDING_RECTANGLE
        GetTextureBindingBuffer -> Just GL_TEXTURE_BINDING_BUFFER
        GetTextureBindingCubeMap -> Just GL_TEXTURE_BINDING_CUBE_MAP
        GetTextureBinding2DMultisample -> Just GL_TEXTURE_BINDING_2D_MULTISAMPLE
        GetTextureBinding2DMultisampleArray -> Just GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY
        -- Antialiasing
        GetSubpixelBits -> Just GL_SUBPIXEL_BITS
        GetSampleBuffers -> Just GL_SAMPLE_BUFFERS
        GetSamples -> Just GL_SAMPLES
         -- Sync Objects
        GetMaxServerWaitTimeout -> Just GL_MAX_SERVER_WAIT_TIMEOUT
        -- Query Objects
        GetMaxVertexStreams -> Just GL_MAX_VERTEX_STREAMS
        -- GL Time
        GetTimestamp -> Just GL_TIMESTAMP
        -- Shader
        GetShaderCompiler -> Just GL_SHADER_COMPILER
        GetNumShaderBinaryFormats -> Just GL_NUM_SHADER_BINARY_FORMATS
        GetNumProgramBinaryFormats -> Just GL_NUM_PROGRAM_BINARY_FORMATS
        -- Debug Output
        GetMaxDebugMessageLength -> Just GL_MAX_DEBUG_MESSAGE_LENGTH
        GetMaxDebugLoggedMessages -> Just GL_MAX_DEBUG_LOGGED_MESSAGES
        GetDebugLoggedMessages -> Just GL_DEBUG_LOGGED_MESSAGES
        GetDebugNextLoggedMessageLength -> Just GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH
        GetMaxDebugGroupStackDepth -> Just GL_MAX_DEBUG_GROUP_STACK_DEPTH
        GetMaxLabelLength -> Just GL_MAX_LABEL_LENGTH
        GetNumExtensions -> Just GL_NUM_EXTENSIONS

-- 0x8825 through 0x8834 are reserved for draw buffers

drawBufferIndexToEnum :: GLsizei -> Maybe GLenum
drawBufferIndexToEnum i
   | 0 <= i && i <= maxDrawBufferIndex = Just (GL_DRAW_BUFFER0 + fromIntegral i)
   | otherwise = Nothing

maxDrawBufferIndex :: GLsizei
maxDrawBufferIndex = fromIntegral (GL_DRAW_BUFFER15 - GL_DRAW_BUFFER0)


-----------------------------------------------------------------------------

data PName1F
    = GetCurrentFogCoord    -- ^ Float1
    -- Rasterization
    | GetZoomX              -- ^ Float
    | GetZoomY              -- ^ Float
    -- Colors
    | GetMaxShininess       -- ^ Float
    | GetMaxSpotExponent    -- ^ Float
    -- Fog
    | GetFogStart           -- ^ float
    | GetFogEnd             -- ^ float
    | GetFogDensity         -- ^ float
    -- Framebuffer
    | GetDepthClearValue    -- ^ clampf
    | GetIndexClearValue    -- ^ float
    -- RasterPos
    | GetCurrentRasterDistance -- ^ float
    -- Point
    | GetPointSizeMin               -- ^ float
    | GetPointSizeMax               -- ^ float
    | GetPointFadeThresholdSize     -- ^ float
    | GetSmoothPointSizeGranularity -- ^ float
    | GetPointSize                  -- ^ float
    -- LineSegment
    | GetLineWidth                  -- ^ float
    | GetSmoothLineWidthGranularity -- ^ float
    -- PerFragment
    | GetSampleCoverageValue        -- ^ clampf
    | GetAlphaTestRef               -- ^ clampf
    -- PixelTransfer
    | GetRedScale                   -- ^ float
    | GetGreenScale                 -- ^ float
    | GetBlueScale                  -- ^ float
    | GetAlphaScale                 -- ^ float
    | GetPostConvolutionRedScale    -- ^ float
    | GetPostConvolutionGreenScale  -- ^ float
    | GetPostConvolutionBlueScale   -- ^ float
    | GetPostConvolutionAlphaScale  -- ^ float
    | GetPostColorMatrixRedScale    -- ^ float
    | GetPostColorMatrixGreenScale  -- ^ float
    | GetPostColorMatrixBlueScale   -- ^ float
    | GetPostColorMatrixAlphaScale  -- ^ float
    | GetRedBias                    -- ^ float
    | GetGreenBias                  -- ^ float
    | GetBlueBias                   -- ^ float
    | GetAlphaBias                  -- ^ float
    | GetPostConvolutionRedBias     -- ^ float
    | GetPostConvolutionGreenBias   -- ^ float
    | GetPostConvolutionBlueBias    -- ^ float
    | GetPostConvolutionAlphaBias   -- ^ float
    | GetPostColorMatrixRedBias     -- ^ float
    | GetPostColorMatrixGreenBias   -- ^ float
    | GetPostColorMatrixBlueBias    -- ^ float
    | GetPostColorMatrixAlphaBias   -- ^ float
    | GetDepthScale                 -- ^ float
    | GetDepthBias                  -- ^ float
    -- Polygons
    | GetPolygonOffsetFactor        -- ^ float
    | GetPolygonOffsetUnits         -- ^ float
    -- Texture parameters
    | GetMaxTextureMaxAnisotropy    -- ^ float
    | GetMaxTextureLODBias          -- ^ float

instance GetPName1F PName1F where

instance GetPName PName1F where
    marshalGetPName pn = case pn of
        GetCurrentFogCoord -> Just GL_CURRENT_FOG_COORD
        -- Rasterization
        GetZoomX -> Just GL_ZOOM_X
        GetZoomY -> Just GL_ZOOM_Y
        -- Colors
        GetMaxShininess -> Just GL_MAX_SHININESS_NV
        GetMaxSpotExponent -> Just GL_MAX_SPOT_EXPONENT_NV
        -- Fog
        GetFogStart -> Just GL_FOG_START
        GetFogEnd -> Just GL_FOG_END
        GetFogDensity -> Just GL_FOG_DENSITY
        -- Framebuffer
        GetDepthClearValue -> Just GL_DEPTH_CLEAR_VALUE
        GetIndexClearValue -> Just GL_INDEX_CLEAR_VALUE
        -- RasterPos
        GetCurrentRasterDistance -> Just GL_CURRENT_RASTER_DISTANCE
        -- Point
        GetPointSizeMin -> Just GL_POINT_SIZE_MIN
        GetPointSizeMax -> Just GL_POINT_SIZE_MAX
        GetPointFadeThresholdSize -> Just GL_POINT_FADE_THRESHOLD_SIZE
        GetSmoothPointSizeGranularity -> Just GL_POINT_SIZE_GRANULARITY
        GetPointSize -> Just GL_POINT_SIZE
        -- LineSegment
        GetSmoothLineWidthGranularity -> Just GL_SMOOTH_LINE_WIDTH_GRANULARITY
        GetLineWidth -> Just GL_LINE_WIDTH
        -- PerFragment
        GetSampleCoverageValue -> Just GL_SAMPLE_COVERAGE_VALUE
        GetAlphaTestRef -> Just GL_ALPHA_TEST_REF
        -- PixelTransfer
        GetRedScale -> Just GL_RED_SCALE
        GetRedBias -> Just GL_RED_BIAS
        GetGreenScale -> Just GL_GREEN_SCALE
        GetGreenBias -> Just GL_GREEN_BIAS
        GetBlueScale -> Just GL_BLUE_SCALE
        GetBlueBias -> Just GL_BLUE_BIAS
        GetAlphaScale -> Just GL_ALPHA_SCALE
        GetAlphaBias -> Just GL_ALPHA_BIAS
        GetPostConvolutionRedScale -> Just GL_POST_CONVOLUTION_RED_SCALE
        GetPostConvolutionGreenScale -> Just GL_POST_CONVOLUTION_GREEN_SCALE
        GetPostConvolutionBlueScale -> Just GL_POST_CONVOLUTION_BLUE_SCALE
        GetPostConvolutionAlphaScale -> Just GL_POST_CONVOLUTION_ALPHA_SCALE
        GetPostConvolutionRedBias -> Just GL_POST_CONVOLUTION_RED_BIAS
        GetPostConvolutionGreenBias -> Just GL_POST_CONVOLUTION_GREEN_BIAS
        GetPostConvolutionBlueBias -> Just GL_POST_CONVOLUTION_BLUE_BIAS
        GetPostConvolutionAlphaBias -> Just GL_POST_CONVOLUTION_ALPHA_BIAS
        GetPostColorMatrixRedScale -> Just GL_POST_COLOR_MATRIX_RED_SCALE
        GetPostColorMatrixGreenScale -> Just GL_POST_COLOR_MATRIX_GREEN_SCALE
        GetPostColorMatrixBlueScale -> Just GL_POST_COLOR_MATRIX_BLUE_SCALE
        GetPostColorMatrixAlphaScale -> Just GL_POST_COLOR_MATRIX_ALPHA_SCALE
        GetPostColorMatrixRedBias -> Just GL_POST_COLOR_MATRIX_RED_BIAS
        GetPostColorMatrixGreenBias -> Just GL_POST_COLOR_MATRIX_GREEN_BIAS
        GetPostColorMatrixBlueBias -> Just GL_POST_COLOR_MATRIX_BLUE_BIAS
        GetPostColorMatrixAlphaBias -> Just GL_POST_COLOR_MATRIX_ALPHA_BIAS
        GetDepthScale -> Just GL_DEPTH_SCALE
        GetDepthBias -> Just GL_DEPTH_BIAS
        -- Polygons
        GetPolygonOffsetFactor -> Just GL_POLYGON_OFFSET_FACTOR
        GetPolygonOffsetUnits -> Just GL_POLYGON_OFFSET_UNITS
        -- Texture parameters
        GetMaxTextureMaxAnisotropy -> Just GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT
        GetMaxTextureLODBias -> Just GL_MAX_TEXTURE_LOD_BIAS

-----------------------------------------------------------------------------

data IPName1I =
      GetAtomicCounterBuffer
    | GetAtomicCounterBufferStart
    | GetAtomicCounterBufferSize
    | GetShaderStorageBuffer
    | GetShaderStorageBufferStart
    | GetShaderStorageBufferSize
    | GetTransformFeedbackBuffer
    | GetTransformFeedbackBufferStart
    | GetTransformFeedbackBufferSize
    | GetUniformBuffer
    | GetUniformBufferStart
    | GetUniformBufferSize

instance GetIPName1I IPName1I where

instance GetPName IPName1I where
    marshalGetPName pn = case pn of
        GetAtomicCounterBuffer -> Just GL_ATOMIC_COUNTER_BUFFER
        GetAtomicCounterBufferStart -> Just GL_ATOMIC_COUNTER_BUFFER_START
        GetAtomicCounterBufferSize -> Just GL_ATOMIC_COUNTER_BUFFER_SIZE
        GetShaderStorageBuffer -> Just GL_SHADER_STORAGE_BUFFER
        GetShaderStorageBufferStart -> Just GL_SHADER_STORAGE_BUFFER_START
        GetShaderStorageBufferSize -> Just GL_SHADER_STORAGE_BUFFER_SIZE
        GetTransformFeedbackBuffer -> Just GL_TRANSFORM_FEEDBACK_BUFFER
        GetTransformFeedbackBufferStart -> Just GL_TRANSFORM_FEEDBACK_BUFFER_START
        GetTransformFeedbackBufferSize -> Just GL_TRANSFORM_FEEDBACK_BUFFER_SIZE
        GetUniformBuffer -> Just GL_UNIFORM_BUFFER
        GetUniformBufferStart -> Just GL_UNIFORM_BUFFER_START
        GetUniformBufferSize -> Just GL_UNIFORM_BUFFER_SIZE

-----------------------------------------------------------------------------

data PName2I
    -- coordtrans
    = GetMaxViewportDims -- ^ sizei
    -- Evaluators
    | GetMap2GridSegments
    -- Polygons
    | GetPolygonMode

instance GetPName2I PName2I where

instance GetPName PName2I where
    marshalGetPName pn = case pn of
        -- coordtrans
        GetMaxViewportDims -> Just GL_MAX_VIEWPORT_DIMS
        -- Evaluators
        GetMap2GridSegments -> Just GL_MAP2_GRID_SEGMENTS
        -- Polygons
        GetPolygonMode -> Just GL_POLYGON_MODE

-----------------------------------------------------------------------------

data PName2F
    -- coordtrans
    = GetDepthRange -- ^ clamp
    -- Evaluators
    | GetMap1GridDomain -- ^ float2?
    -- Point
    | GetAliasedPointSizeRange  -- ^ float
    | GetSmoothPointSizeRange   -- ^ float
    -- LineSegments
    | GetAliasedLineWidthRange  -- ^ float
    | GetSmoothLineWidthRange   -- ^ float
    -- PerFragment
    | GetDepthBounds            -- ^ clampd
    -- Tessellation
    | GetPatchDefaultInnerLevel -- ^ float

instance GetPName2F PName2F where

instance GetPName PName2F where
    marshalGetPName pn = case pn of
        -- coord trans
        GetDepthRange -> Just GL_DEPTH_RANGE
        -- Evaluators
        GetMap1GridDomain -> Just GL_MAP1_GRID_DOMAIN
        -- Point
        GetAliasedPointSizeRange -> Just GL_ALIASED_POINT_SIZE_RANGE
        GetSmoothPointSizeRange -> Just GL_POINT_SIZE_RANGE
        -- LineSegments
        GetAliasedLineWidthRange -> Just GL_ALIASED_LINE_WIDTH_RANGE
        GetSmoothLineWidthRange -> Just GL_SMOOTH_LINE_WIDTH_RANGE
        -- PerFragment
        GetDepthBounds -> Just GL_DEPTH_BOUNDS_EXT
        -- Tessellation
        GetPatchDefaultInnerLevel -> Just GL_PATCH_DEFAULT_INNER_LEVEL

-----------------------------------------------------------------------------

data PName3F
    = GetCurrentNormal -- ^ Float3
    -- Point
    | GetPointDistanceAttenuation -- ^ float

instance GetPName3F PName3F where

instance GetPName PName3F where
    marshalGetPName pn = case pn of
        GetCurrentNormal -> Just GL_CURRENT_NORMAL
        -- Point
        GetPointDistanceAttenuation -> Just GL_POINT_DISTANCE_ATTENUATION

-----------------------------------------------------------------------------

data PName4I
    -- coordtrans
    = GetViewport               -- ^ int
    -- Framebuffer
    | GetRGBASignedComponents   -- ^ int
    -- PerFragment
    | GetScissorBox             -- ^ int


instance GetPName4I PName4I where

instance GetPName PName4I where
    marshalGetPName pn = case pn of
        -- coordtrans
        GetViewport -> Just GL_VIEWPORT
        -- Framebuffer
        GetRGBASignedComponents -> Just GL_RGBA_SIGNED_COMPONENTS_EXT
        -- PerFragement
        GetScissorBox -> Just GL_SCISSOR_BOX


-- | Both indexed and unindexed
data PName4ISemiIndexed
    = GetColorWritemask         -- ^ bool

instance GetPName4I  PName4ISemiIndexed where

instance GetIPName4I PName4ISemiIndexed where

instance GetPName PName4ISemiIndexed where
    marshalGetPName pn = case pn of
        GetColorWritemask -> Just GL_COLOR_WRITEMASK

-----------------------------------------------------------------------------

data PName4F
    = GetCurrentColor -- ^ ?
    | GetCurrentTextureCoords   -- ^ Float
    | GetCurrentSecondaryColor  -- ^ Float
    -- Colors
    | GetLightModelAmbient  -- ^ float
    -- Evaluators
    | GetMap2GridDomain     -- ^ float?
    -- Fog
    | GetFogColor           -- ^ clampf
    -- Framebuffer
    | GetColorClearValue    -- ^ clampf
    | GetAccumClearValue    -- ^ float
    -- RasterPos
    | GetCurrentRasterColor             -- ^ float
    | GetCurrentRasterSecondaryColor    -- ^ float
    | GetCurrentRasterTextureCoords     -- ^ float
    | GetCurrentRasterPosition          -- ^ float
    -- PerFragment
    | GetBlendColor                     -- ^ clampf
    -- Tessellation
    | GetPatchDefaultOuterLevel         -- ^ float

instance GetPName4F PName4F where

instance GetPName PName4F where
    marshalGetPName pn = case pn of
        GetCurrentColor -> Just GL_CURRENT_COLOR
        GetCurrentTextureCoords -> Just GL_CURRENT_TEXTURE_COORDS
        GetCurrentSecondaryColor -> Just GL_CURRENT_SECONDARY_COLOR
        -- Colors
        GetLightModelAmbient -> Just GL_LIGHT_MODEL_AMBIENT
        -- Evaluators
        GetMap2GridDomain -> Just GL_MAP2_GRID_DOMAIN
        -- Fog
        GetFogColor -> Just GL_FOG_COLOR
        -- Framebuffer
        GetColorClearValue -> Just GL_COLOR_CLEAR_VALUE
        GetAccumClearValue -> Just GL_ACCUM_CLEAR_VALUE
        -- RasterPos
        GetCurrentRasterColor -> Just GL_CURRENT_RASTER_COLOR
        GetCurrentRasterSecondaryColor -> Just GL_CURRENT_RASTER_SECONDARY_COLOR
        GetCurrentRasterTextureCoords -> Just GL_CURRENT_RASTER_TEXTURE_COORDS
        GetCurrentRasterPosition -> Just GL_CURRENT_RASTER_POSITION
        -- PerFragment
        GetBlendColor -> Just GL_BLEND_COLOR
        -- Tessellation
        GetPatchDefaultOuterLevel -> Just GL_PATCH_DEFAULT_OUTER_LEVEL

-- 0x3000 through 0x3FFF are reserved for clip planes
clipPlaneIndexToEnum :: GLsizei -> Maybe GLenum
clipPlaneIndexToEnum i
   | 0 <= i && i <= maxClipPlaneIndex = Just (GL_CLIP_DISTANCE0 + fromIntegral i)
   | otherwise = Nothing

maxClipPlaneIndex :: GLsizei
maxClipPlaneIndex = 0xFFF

-----------------------------------------------------------------------------

data PNameNI
    = GetCompressedTextureFormats
    | GetShaderBinaryFormats
    | GetProgramBinaryFormats

instance GetPNameNI PNameNI where

instance GetPName   PNameNI where
   marshalGetPName pn = case pn of
      GetCompressedTextureFormats -> Just GL_COMPRESSED_TEXTURE_FORMATS
      GetShaderBinaryFormats -> Just GL_SHADER_BINARY_FORMATS
      GetProgramBinaryFormats -> Just GL_PROGRAM_BINARY_FORMATS

-----------------------------------------------------------------------------

data PNameMatrix
    -- coordtrans
    = GetModelviewMatrix
    | GetProjectionMatrix
    | GetTextureMatrix
    | GetColorMatrix
    | GetMatrixPalette

instance GetPNameMatrix PNameMatrix where

instance GetPName PNameMatrix where
    marshalGetPName pn = case pn of
        -- coordtrans
        GetModelviewMatrix -> Just GL_MODELVIEW_MATRIX
        GetProjectionMatrix -> Just GL_PROJECTION_MATRIX
        GetTextureMatrix -> Just GL_TEXTURE_MATRIX
        GetColorMatrix -> Just GL_COLOR_MATRIX
        GetMatrixPalette -> Just GL_MATRIX_PALETTE_ARB

--------------------------------------------------------------------------------

data GetPointervPName =
   -- core profile
     DebugCallbackFunction
   | DebugCallbackUserParam
   -- compatibility profile
   | SelectionBufferPointer
   | FeedbackBufferPointer
   | VertexArrayPointer
   | NormalArrayPointer
   | ColorArrayPointer
   | SecondaryColorArrayPointer
   | IndexArrayPointer
   | TextureCoordArrayPointer
   | FogCoordArrayPointer
   | EdgeFlagArrayPointer
   -- GL_ARB_vertex_blend
   | WeightArrayPointer
   -- GL_ARB_matrix_palette
   | MatrixIndexArrayPointer

marshalGetPointervPName :: GetPointervPName -> GLenum
marshalGetPointervPName x = case x of
   DebugCallbackFunction -> GL_DEBUG_CALLBACK_FUNCTION
   DebugCallbackUserParam -> GL_DEBUG_CALLBACK_USER_PARAM
   SelectionBufferPointer -> GL_SELECTION_BUFFER_POINTER
   FeedbackBufferPointer -> GL_FEEDBACK_BUFFER_POINTER
   VertexArrayPointer -> GL_VERTEX_ARRAY_POINTER
   NormalArrayPointer -> GL_NORMAL_ARRAY_POINTER
   ColorArrayPointer -> GL_COLOR_ARRAY_POINTER
   SecondaryColorArrayPointer -> GL_SECONDARY_COLOR_ARRAY_POINTER
   IndexArrayPointer -> GL_INDEX_ARRAY_POINTER
   TextureCoordArrayPointer -> GL_TEXTURE_COORD_ARRAY_POINTER
   FogCoordArrayPointer -> GL_FOG_COORD_ARRAY_POINTER
   EdgeFlagArrayPointer -> GL_EDGE_FLAG_ARRAY_POINTER
   WeightArrayPointer -> GL_WEIGHT_ARRAY_POINTER_ARB
   MatrixIndexArrayPointer -> GL_MATRIX_INDEX_ARRAY_POINTER_ARB

--------------------------------------------------------------------------------

getPointer :: GetPointervPName -> IO (Ptr a)
getPointer n = with nullPtr $ \buf -> do
   glGetPointerv (marshalGetPointervPName n) buf
   peek buf
