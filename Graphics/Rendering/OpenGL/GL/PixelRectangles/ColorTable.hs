--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to a part of section 3.6.1 (Pixel Storage Modes) of
-- the OpenGL 1.5 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable (
   ColorTableStage(..), colorTableStage,
   Proxy(..), ColorTable(..), PixelInternalFormat(..),
   colorTable, getColorTable, copyColorTable, colorSubTable, copyColorSubTable,
   colorTableScale, colorTableBias, colorTableFormat, colorTableWidth,
   colorTableRGBASizes, colorTableLuminanceSize, colorTableIntesitySize,
) where

import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable(..) )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapColorTable,CapPostConvolutionColorTable,
             CapPostColorMatrixColorTable,CapTextureColorTable),
   makeCapability )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLint, GLsizei, GLfloat, Capability )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Position(..) )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek1 )
import Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization (
    PixelData(..) )
import Graphics.Rendering.OpenGL.GL.PixelData ( withPixelData )
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat (
   PixelInternalFormat(..), marshalPixelInternalFormat',
   unmarshalPixelInternalFormat )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Color4(..) )
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal (
   recordInvalidEnum )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"

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
   ColorTable -> 0x80d0
   PostConvolutionColorTable -> 0x80d1
   PostColorMatrixColorTable -> 0x80d2
   Texture1DColorTable -> 0xde0
   Texture2DColorTable -> 0xde1
   Texture3DColorTable -> 0x806f
   TextureCubeMapColorTable -> 0x8513
   TextureColorTable -> 0x80bc
   SharedTexturePalette -> 0x81fb

--------------------------------------------------------------------------------

data Proxy =
     NoProxy
   | Proxy
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

marshalProxyColorTable :: Proxy -> ColorTable -> Maybe GLenum
marshalProxyColorTable NoProxy x = Just (marshalColorTable x)
marshalProxyColorTable Proxy   x = case x of
   ColorTable -> Just 0x80d3
   PostConvolutionColorTable -> Just 0x80d4
   PostColorMatrixColorTable -> Just 0x80d5
   Texture1DColorTable -> Just 0x8063
   Texture2DColorTable -> Just 0x8064
   Texture3DColorTable -> Just 0x8070
   TextureCubeMapColorTable -> Just 0x851b
   TextureColorTable -> Just 0x80bd
   SharedTexturePalette -> Nothing

--------------------------------------------------------------------------------

colorTable ::
   Proxy -> ColorTable -> PixelInternalFormat -> GLsizei -> PixelData a -> IO ()
colorTable proxy ct int w pd =
   maybe recordInvalidEnum
         (\target -> withPixelData pd $
            glColorTable target (marshalPixelInternalFormat' int) w)
         (marshalProxyColorTable proxy ct)

EXTENSION_ENTRY("GL_ARB_imaging",glColorTable,GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())

--------------------------------------------------------------------------------

getColorTable :: ColorTable -> PixelData a -> IO ()
getColorTable ct pd =
   withPixelData pd $ glGetColorTable (marshalColorTable ct)

EXTENSION_ENTRY("GL_ARB_imaging",glGetColorTable,GLenum -> GLenum -> GLenum -> Ptr a -> IO ())

--------------------------------------------------------------------------------

copyColorTable :: ColorTable -> PixelInternalFormat -> Position -> GLsizei -> IO ()
copyColorTable ct int (Position x y) =
   glCopyColorTable (marshalColorTable ct) (marshalPixelInternalFormat' int) x y

EXTENSION_ENTRY("GL_ARB_imaging",glCopyColorTable,GLenum -> GLenum -> GLint -> GLint -> GLsizei -> IO ())

--------------------------------------------------------------------------------

colorSubTable :: ColorTable -> GLsizei -> GLsizei -> PixelData a -> IO ()
colorSubTable ct start count pd =
   withPixelData pd $ glColorSubTable (marshalColorTable ct) start count

EXTENSION_ENTRY("GL_ARB_imaging",glColorSubTable,GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())

--------------------------------------------------------------------------------

copyColorSubTable :: ColorTable -> GLsizei -> Position -> GLsizei -> IO ()
copyColorSubTable ct start (Position x y) =
   glCopyColorSubTable (marshalColorTable ct) start x y

EXTENSION_ENTRY("GL_ARB_imaging",glCopyColorSubTable,GLenum -> GLsizei -> GLint -> GLint -> GLsizei -> IO ())

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
   ColorTableScale -> 0x80d6
   ColorTableBias -> 0x80d7
   ColorTableFormat -> 0x80d8
   ColorTableWidth -> 0x80d9
   ColorTableRedSize -> 0x80da
   ColorTableGreenSize -> 0x80db
   ColorTableBlueSize -> 0x80dc
   ColorTableAlphaSize -> 0x80dd
   ColorTableLuminanceSize -> 0x80de
   ColorTableIntensitySize -> 0x80df

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
         buf
      peek buf

EXTENSION_ENTRY("GL_ARB_imaging",glGetColorTableParameterfv,GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ())

colorTableParameterC4f ::
   ColorTable -> ColorTablePName -> Color4 GLfloat -> IO ()
colorTableParameterC4f ct p c =
   with c $
      glColorTableParameterfv (marshalColorTable ct) (marshalColorTablePName p)

EXTENSION_ENTRY("GL_ARB_imaging",glColorTableParameterfv,GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ())

--------------------------------------------------------------------------------

colorTableFormat :: ColorTable -> GettableStateVar PixelInternalFormat
colorTableFormat ct =
   makeGettableStateVar $
      getColorTableParameteri unmarshalPixelInternalFormat ct ColorTableFormat

getColorTableParameteri :: (GLint -> a) -> ColorTable -> ColorTablePName -> IO a
getColorTableParameteri f ct p =
   alloca $ \buf -> do
      glGetColorTableParameteriv
         (marshalColorTable ct)
         (marshalColorTablePName p)
         buf
      peek1 f buf

EXTENSION_ENTRY("GL_ARB_imaging",glGetColorTableParameteriv,GLenum -> GLenum -> Ptr GLint -> IO ())

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
