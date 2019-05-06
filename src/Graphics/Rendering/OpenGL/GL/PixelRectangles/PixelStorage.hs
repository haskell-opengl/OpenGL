--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelRectangles.PixelStorage
-- Copyright   :  (c) Sven Panne 2002-2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 3.6.1 (Pixel Storage Modes) of the
-- OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PixelRectangles.PixelStorage (
   PixelStoreDirection(..), swapBytes, lsbFirst, rowLength, skipRows,
   skipPixels, rowAlignment, imageHeight, skipImages
) where

import Data.StateVar
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.GL

--------------------------------------------------------------------------------

data PixelStoreDirection =
     Pack
   | Unpack
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

data PixelStore =
     UnpackSwapBytes
   | UnpackLSBFirst
   | UnpackRowLength
   | UnpackSkipRows
   | UnpackSkipPixels
   | UnpackAlignment
   | PackSwapBytes
   | PackLSBFirst
   | PackRowLength
   | PackSkipRows
   | PackSkipPixels
   | PackAlignment
   | PackSkipImages
   | PackImageHeight
   | UnpackSkipImages
   | UnpackImageHeight

marshalPixelStore :: PixelStore -> GLenum
marshalPixelStore x = case x of
   UnpackSwapBytes -> GL_UNPACK_SWAP_BYTES
   UnpackLSBFirst -> GL_UNPACK_LSB_FIRST
   UnpackRowLength -> GL_UNPACK_ROW_LENGTH
   UnpackSkipRows -> GL_UNPACK_SKIP_ROWS
   UnpackSkipPixels -> GL_UNPACK_SKIP_PIXELS
   UnpackAlignment -> GL_UNPACK_ALIGNMENT
   PackSwapBytes -> GL_PACK_SWAP_BYTES
   PackLSBFirst -> GL_PACK_LSB_FIRST
   PackRowLength -> GL_PACK_ROW_LENGTH
   PackSkipRows -> GL_PACK_SKIP_ROWS
   PackSkipPixels -> GL_PACK_SKIP_PIXELS
   PackAlignment -> GL_PACK_ALIGNMENT
   PackSkipImages -> GL_PACK_SKIP_IMAGES
   PackImageHeight -> GL_PACK_IMAGE_HEIGHT
   UnpackSkipImages -> GL_UNPACK_SKIP_IMAGES
   UnpackImageHeight -> GL_UNPACK_IMAGE_HEIGHT

--------------------------------------------------------------------------------

swapBytes :: PixelStoreDirection -> StateVar Bool
swapBytes Pack   = pixelStoreb GetPackSwapBytes PackSwapBytes
swapBytes Unpack = pixelStoreb GetUnpackSwapBytes UnpackSwapBytes

lsbFirst :: PixelStoreDirection -> StateVar Bool
lsbFirst Pack   = pixelStoreb GetPackLSBFirst PackLSBFirst
lsbFirst Unpack = pixelStoreb GetUnpackLSBFirst UnpackLSBFirst

rowLength :: PixelStoreDirection -> StateVar GLint
rowLength Pack   = pixelStorei GetPackRowLength PackRowLength
rowLength Unpack = pixelStorei GetUnpackRowLength UnpackRowLength

skipRows :: PixelStoreDirection -> StateVar GLint
skipRows Pack   = pixelStorei GetPackSkipRows PackSkipRows
skipRows Unpack = pixelStorei GetUnpackSkipRows UnpackSkipRows

skipPixels :: PixelStoreDirection -> StateVar GLint
skipPixels Pack   = pixelStorei GetPackSkipPixels PackSkipPixels
skipPixels Unpack = pixelStorei GetUnpackSkipPixels UnpackSkipPixels

rowAlignment :: PixelStoreDirection -> StateVar GLint
rowAlignment Pack   = pixelStorei GetPackAlignment PackAlignment
rowAlignment Unpack = pixelStorei GetUnpackAlignment UnpackAlignment

imageHeight :: PixelStoreDirection -> StateVar GLint
imageHeight Pack   = pixelStorei GetPackImageHeight PackImageHeight
imageHeight Unpack = pixelStorei GetUnpackImageHeight UnpackImageHeight

skipImages :: PixelStoreDirection -> StateVar GLint
skipImages Pack   = pixelStorei GetPackSkipImages PackSkipImages
skipImages Unpack = pixelStorei GetUnpackSkipImages UnpackSkipImages

--------------------------------------------------------------------------------

pixelStoreb :: PName1I -> PixelStore -> StateVar Bool
pixelStoreb pn ps =
   makeStateVar
      (getBoolean1 unmarshalGLboolean pn)
      (glPixelStorei (marshalPixelStore ps) . marshalGLboolean)

pixelStorei :: PName1I -> PixelStore -> StateVar GLint
pixelStorei pn ps =
   makeStateVar
      (getInteger1 id pn)
      (glPixelStorei (marshalPixelStore ps))
