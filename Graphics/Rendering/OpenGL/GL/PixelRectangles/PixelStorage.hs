--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelRectangles.PixelStorage
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 3.6.1 (Pixel Storage Modes) of the
-- OpenGL 1.5 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PixelRectangles.PixelStorage (
   PixelStoreDirection(..), swapBytes, lsbFirst, rowLength, skipRows,
   skipPixels, rowAlignment, imageHeight, skipImages
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLint )
import Graphics.Rendering.OpenGL.GL.GLboolean (
   marshalGLboolean, unmarshalGLboolean )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetUnpackSwapBytes,GetUnpackLSBFirst,GetUnpackRowLength,
            GetUnpackSkipRows,GetUnpackSkipPixels,GetUnpackAlignment,
            GetUnpackImageHeight,GetUnpackSkipImages,GetPackSwapBytes,
            GetPackLSBFirst,GetPackRowLength,GetPackSkipRows,GetPackSkipPixels,
            GetPackAlignment,GetPackImageHeight,GetPackSkipImages),
   getBoolean1, getInteger1 )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar, makeStateVar )

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
   UnpackSwapBytes -> 0xcf0
   UnpackLSBFirst -> 0xcf1
   UnpackRowLength -> 0xcf2
   UnpackSkipRows -> 0xcf3
   UnpackSkipPixels -> 0xcf4
   UnpackAlignment -> 0xcf5
   PackSwapBytes -> 0xd00
   PackLSBFirst -> 0xd01
   PackRowLength -> 0xd02
   PackSkipRows -> 0xd03
   PackSkipPixels -> 0xd04
   PackAlignment -> 0xd05
   PackSkipImages -> 0x806b
   PackImageHeight -> 0x806c
   UnpackSkipImages -> 0x806d
   UnpackImageHeight -> 0x806e

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

pixelStoreb :: GetPName -> PixelStore -> StateVar Bool
pixelStoreb pn ps =
   makeStateVar
      (getBoolean1 unmarshalGLboolean pn)
      (glPixelStorei (marshalPixelStore ps) . marshalGLboolean)

pixelStorei :: GetPName -> PixelStore -> StateVar GLint
pixelStorei pn ps =
   makeStateVar
      (getInteger1 id pn)
      (glPixelStorei (marshalPixelStore ps))

foreign import CALLCONV unsafe "glPixelStorei" glPixelStorei ::
   GLenum -> GLint -> IO ()
