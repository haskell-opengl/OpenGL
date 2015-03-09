--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to a part of section 3.6.1 (Pixel Storage Modes) of
-- the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable (
   ColorTableStage(..), colorTableStage,
   Proxy(..), ColorTable(..), PixelInternalFormat(..),
   colorTable, getColorTable, copyColorTable, colorSubTable, copyColorSubTable,
   colorTableScale, colorTableBias, colorTableFormat, colorTableWidth,
   colorTableRGBASizes, colorTableLuminanceSize, colorTableIntesitySize,
) where

import Data.StateVar
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.PixelData
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

data ColorTableStage =
     ColorTableStage
   | PostConvolutionColorTableStage
   | PostColorMatrixColorTableStage
   | TextureColorTableStage
   deriving ( Eq, Ord, Show )

colorTableStageToColorTable :: ColorTableStage -> ColorTable
colorTableStageToColorTable x = case x of
   ColorTableStage -> ColorTable
   PostConvolutionColorTableStage -> PostConvolutionColorTable
   PostColorMatrixColorTableStage -> PostColorMatrixColorTable
   TextureColorTableStage -> TextureColorTable

colorTableStageToEnableCap :: ColorTableStage -> EnableCap
colorTableStageToEnableCap x = case x of
   ColorTableStage -> CapColorTable
   PostConvolutionColorTableStage -> CapPostConvolutionColorTable
   PostColorMatrixColorTableStage -> CapPostColorMatrixColorTable
   TextureColorTableStage -> CapTextureColorTable

--------------------------------------------------------------------------------

colorTableStage :: ColorTableStage -> StateVar Capability
colorTableStage = makeCapability . colorTableStageToEnableCap

--------------------------------------------------------------------------------

data ColorTable =
     ColorTable
   | PostConvolutionColorTable
   | PostColorMatrixColorTable
   | Texture1DColorTable
   | Texture2DColorTable
   | Texture3DColorTable
   | TextureCubeMapColorTable
   | TextureColorTable
   | SharedTexturePalette
   deriving ( Eq, Ord, Show )

marshalColorTable :: ColorTable -> GLenum
marshalColorTable x = case x of
   ColorTable -> gl_COLOR_TABLE
   PostConvolutionColorTable -> gl_POST_CONVOLUTION_COLOR_TABLE
   PostColorMatrixColorTable -> gl_POST_COLOR_MATRIX_COLOR_TABLE
   Texture1DColorTable -> gl_TEXTURE_1D
   Texture2DColorTable -> gl_TEXTURE_2D
   Texture3DColorTable -> gl_TEXTURE_3D
   TextureCubeMapColorTable -> gl_TEXTURE_CUBE_MAP
   TextureColorTable -> gl_TEXTURE_COLOR_TABLE_SGI
   SharedTexturePalette -> gl_SHARED_TEXTURE_PALETTE_EXT

--------------------------------------------------------------------------------

data Proxy =
     NoProxy
   | Proxy
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

marshalProxyColorTable :: Proxy -> ColorTable -> Maybe GLenum
marshalProxyColorTable NoProxy x = Just (marshalColorTable x)
marshalProxyColorTable Proxy   x = case x of
   ColorTable -> Just gl_PROXY_COLOR_TABLE
   PostConvolutionColorTable -> Just gl_PROXY_POST_CONVOLUTION_COLOR_TABLE
   PostColorMatrixColorTable -> Just gl_PROXY_POST_COLOR_MATRIX_COLOR_TABLE
   Texture1DColorTable -> Just gl_PROXY_TEXTURE_1D
   Texture2DColorTable -> Just gl_PROXY_TEXTURE_2D
   Texture3DColorTable -> Just gl_PROXY_TEXTURE_3D
   TextureCubeMapColorTable -> Just gl_PROXY_TEXTURE_CUBE_MAP
   TextureColorTable -> Just gl_TEXTURE_COLOR_TABLE_SGI
   SharedTexturePalette -> Nothing

--------------------------------------------------------------------------------

colorTable ::
   Proxy -> ColorTable -> PixelInternalFormat -> GLsizei -> PixelData a -> IO ()
colorTable proxy ct int w pd =
   maybe recordInvalidEnum
         (\target -> withPixelData pd $
            glColorTable target (marshalPixelInternalFormat' int) w)
         (marshalProxyColorTable proxy ct)

--------------------------------------------------------------------------------

getColorTable :: ColorTable -> PixelData a -> IO ()
getColorTable ct pd =
   withPixelData pd $ glGetColorTable (marshalColorTable ct)

--------------------------------------------------------------------------------

copyColorTable :: ColorTable -> PixelInternalFormat -> Position -> GLsizei -> IO ()
copyColorTable ct int (Position x y) =
   glCopyColorTable (marshalColorTable ct) (marshalPixelInternalFormat' int) x y

--------------------------------------------------------------------------------

colorSubTable :: ColorTable -> GLsizei -> GLsizei -> PixelData a -> IO ()
colorSubTable ct start count pd =
   withPixelData pd $ glColorSubTable (marshalColorTable ct) start count

--------------------------------------------------------------------------------

copyColorSubTable :: ColorTable -> GLsizei -> Position -> GLsizei -> IO ()
copyColorSubTable ct start (Position x y) =
   glCopyColorSubTable (marshalColorTable ct) start x y

--------------------------------------------------------------------------------

data ColorTablePName =
     ColorTableScale
   | ColorTableBias
   | ColorTableFormat
   | ColorTableWidth
   | ColorTableRedSize
   | ColorTableGreenSize
   | ColorTableBlueSize
   | ColorTableAlphaSize
   | ColorTableLuminanceSize
   | ColorTableIntensitySize

marshalColorTablePName :: ColorTablePName -> GLenum
marshalColorTablePName x = case x of
   ColorTableScale -> gl_COLOR_TABLE_SCALE
   ColorTableBias -> gl_COLOR_TABLE_BIAS
   ColorTableFormat -> gl_COLOR_TABLE_FORMAT
   ColorTableWidth -> gl_COLOR_TABLE_WIDTH
   ColorTableRedSize -> gl_COLOR_TABLE_RED_SIZE
   ColorTableGreenSize -> gl_COLOR_TABLE_GREEN_SIZE
   ColorTableBlueSize -> gl_COLOR_TABLE_BLUE_SIZE
   ColorTableAlphaSize -> gl_COLOR_TABLE_ALPHA_SIZE
   ColorTableLuminanceSize -> gl_COLOR_TABLE_LUMINANCE_SIZE
   ColorTableIntensitySize -> gl_COLOR_TABLE_INTENSITY_SIZE

--------------------------------------------------------------------------------

colorTableScale :: ColorTableStage -> StateVar (Color4 GLfloat)
colorTableScale = colorTableScaleBias ColorTableScale

colorTableBias :: ColorTableStage -> StateVar (Color4 GLfloat)
colorTableBias = colorTableScaleBias ColorTableBias

colorTableScaleBias ::
   ColorTablePName -> ColorTableStage -> StateVar (Color4 GLfloat)
colorTableScaleBias p s =
   makeStateVar (getColorTableParameterC4f ct p) (colorTableParameterC4f ct p)
   where ct = colorTableStageToColorTable s

getColorTableParameterC4f ::
   ColorTable -> ColorTablePName -> IO (Color4 GLfloat)
getColorTableParameterC4f ct p =
   alloca $ \buf -> do
      glGetColorTableParameterfv
         (marshalColorTable ct)
         (marshalColorTablePName p)
         (castPtr buf)
      peek buf

colorTableParameterC4f ::
   ColorTable -> ColorTablePName -> Color4 GLfloat -> IO ()
colorTableParameterC4f ct p c =
   with c $ \ptr ->
      glColorTableParameterfv (marshalColorTable ct) (marshalColorTablePName p) (castPtr ptr)

--------------------------------------------------------------------------------

colorTableFormat :: ColorTable -> GettableStateVar PixelInternalFormat
colorTableFormat ct =
   makeGettableStateVar $
      getColorTableParameteri unmarshalPixelInternalFormat ct ColorTableFormat

getColorTableParameteri :: (GLint -> a) -> ColorTable -> ColorTablePName -> IO a
getColorTableParameteri f ct p =
   with 0 $ \buf -> do
      glGetColorTableParameteriv
         (marshalColorTable ct)
         (marshalColorTablePName p)
         buf
      peek1 f buf

--------------------------------------------------------------------------------

colorTableWidth :: ColorTable -> GettableStateVar GLsizei
colorTableWidth ct =
   makeGettableStateVar $
      getColorTableParameteri fromIntegral ct ColorTableWidth

--------------------------------------------------------------------------------

colorTableRGBASizes :: ColorTable -> GettableStateVar (Color4 GLsizei)
colorTableRGBASizes ct =
   makeGettableStateVar $ do
      r <- getColorTableParameteri fromIntegral ct ColorTableRedSize
      g <- getColorTableParameteri fromIntegral ct ColorTableGreenSize
      b <- getColorTableParameteri fromIntegral ct ColorTableBlueSize
      a <- getColorTableParameteri fromIntegral ct ColorTableAlphaSize
      return $ Color4 r g b a

colorTableLuminanceSize :: ColorTable -> GettableStateVar GLsizei
colorTableLuminanceSize ct =
   makeGettableStateVar $
      getColorTableParameteri fromIntegral ct ColorTableLuminanceSize

colorTableIntesitySize :: ColorTable -> GettableStateVar GLsizei
colorTableIntesitySize ct =
   makeGettableStateVar $
      getColorTableParameteri fromIntegral ct ColorTableIntensitySize
