--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelRectangles
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 3.6 (Pixel Rectangles) of the OpenGL 1.4
-- specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PixelRectangles (
   PixelStore(..), unpackSwapBytes, unpackLSBFirst, unpackRowLength,
   unpackSkipRows, unpackSkipPixels, unpackAlignment, unpackImageHeight,
   unpackSkipImages, packSwapBytes, packLSBFirst, packRowLength, packSkipRows,
   packSkipPixels, packAlignment, packImageHeight, packSkipImages
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
   deriving ( Eq, Ord, Show )

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

unpackSwapBytes :: StateVar Bool
unpackSwapBytes = pixelStoreb GetUnpackSwapBytes UnpackSwapBytes

unpackLSBFirst :: StateVar Bool
unpackLSBFirst = pixelStoreb GetUnpackLSBFirst UnpackLSBFirst

unpackRowLength :: StateVar GLint
unpackRowLength = pixelStorei GetUnpackRowLength UnpackRowLength

unpackSkipRows :: StateVar GLint
unpackSkipRows = pixelStorei GetUnpackSkipRows UnpackSkipRows

unpackSkipPixels :: StateVar GLint
unpackSkipPixels = pixelStorei GetUnpackSkipPixels UnpackSkipPixels

unpackAlignment :: StateVar GLint
unpackAlignment = pixelStorei GetUnpackAlignment UnpackAlignment

unpackImageHeight :: StateVar GLint
unpackImageHeight = pixelStorei GetUnpackImageHeight UnpackImageHeight

unpackSkipImages :: StateVar GLint
unpackSkipImages = pixelStorei GetUnpackSkipImages UnpackSkipImages

packSwapBytes :: StateVar Bool
packSwapBytes = pixelStoreb GetPackSwapBytes PackSwapBytes

packLSBFirst :: StateVar Bool
packLSBFirst = pixelStoreb GetPackLSBFirst PackLSBFirst

packRowLength :: StateVar GLint
packRowLength = pixelStorei GetPackRowLength PackRowLength

packSkipRows :: StateVar GLint
packSkipRows = pixelStorei GetPackSkipRows PackSkipRows

packSkipPixels :: StateVar GLint
packSkipPixels = pixelStorei GetPackSkipPixels PackSkipPixels

packAlignment :: StateVar GLint
packAlignment = pixelStorei GetPackAlignment PackAlignment

packImageHeight :: StateVar GLint
packImageHeight = pixelStorei GetPackImageHeight PackImageHeight

packSkipImages :: StateVar GLint
packSkipImages = pixelStorei GetPackSkipImages PackSkipImages

--------------------------------------------------------------------------------

pixelStoreb :: GetPName -> PixelStore -> StateVar Bool
pixelStoreb pn ps =
   makeStateVar
      (getBoolean1 unmarshalGLboolean pn)
      (glPixelStorei (marshalPixelStore ps) . (fromIntegral . marshalGLboolean))

pixelStorei :: GetPName -> PixelStore -> StateVar GLint
pixelStorei pn ps =
   makeStateVar
      (getInteger1 id pn)
      (glPixelStorei (marshalPixelStore ps))

foreign import CALLCONV unsafe "glPixelStorei" glPixelStorei ::
   GLenum -> GLint -> IO ()
