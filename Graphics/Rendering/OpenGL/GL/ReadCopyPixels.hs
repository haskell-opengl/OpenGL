--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.ReadCopyPixels
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 4.3 (Drawing, Reading, and Copying Pixels)
-- of the OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.ReadCopyPixels (
   -- * Reading Pixels
   readPixels, ReadBufferMode(..), readBuffer,

   -- * Copying Pixels
   PixelCopyType(..), copyPixels
) where

import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLint, GLsizei )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Position(..), Size(..) )
import Graphics.Rendering.OpenGL.GL.PixelTypes (
   PixelFormat(..), marshalPixelFormat, PixelType(..), marshalPixelType )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetReadBuffer), getEnum1 )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar, makeStateVar )

--------------------------------------------------------------------------------

readPixels :: Position -> Size -> PixelFormat -> PixelType -> Ptr a -> IO ()
readPixels (Position x y) (Size w h) f t =
   glReadPixels x y w h (marshalPixelFormat f) (marshalPixelType t)

foreign import CALLCONV unsafe "glReadPixels" glReadPixels ::
   GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ()

--------------------------------------------------------------------------------

data ReadBufferMode =
     ReadBufferFrontLeft
   | ReadBufferFrontRight
   | ReadBufferBackLeft
   | ReadBufferBackRight
   | ReadBufferFront
   | ReadBufferBack
   | ReadBufferLeft
   | ReadBufferRight
   | ReadBufferAux GLsizei
   deriving ( Eq, Ord, Show )

marshalReadBufferMode :: ReadBufferMode -> GLenum
marshalReadBufferMode x = case x of
   ReadBufferFrontLeft -> 0x400
   ReadBufferFrontRight -> 0x401
   ReadBufferBackLeft -> 0x402
   ReadBufferBackRight -> 0x403
   ReadBufferFront -> 0x404
   ReadBufferBack -> 0x405
   ReadBufferLeft -> 0x406
   ReadBufferRight -> 0x407
   ReadBufferAux i
      | i <= 246  -> 0x409 + fromIntegral i
      | otherwise -> error ("marshalReadBufferMode: illegal value " ++ show i)

unmarshalReadBufferMode :: GLenum -> ReadBufferMode
unmarshalReadBufferMode x
   | x == 0x400 = ReadBufferFrontLeft
   | x == 0x401 = ReadBufferFrontRight
   | x == 0x402 = ReadBufferBackLeft
   | x == 0x403 = ReadBufferBackRight
   | x == 0x404 = ReadBufferFront
   | x == 0x405 = ReadBufferBack
   | x == 0x406 = ReadBufferLeft
   | x == 0x407 = ReadBufferRight
   | 0x409 <= x && x <= 0x4ff = ReadBufferAux (fromIntegral x - 0x409)
   | otherwise = error ("unmarshalReadBufferMode: illegal value " ++ show x)

--------------------------------------------------------------------------------

readBuffer :: StateVar ReadBufferMode
readBuffer = makeStateVar (getEnum1 unmarshalReadBufferMode GetReadBuffer)
                          (glReadBuffer . marshalReadBufferMode)

foreign import CALLCONV unsafe "glReadBuffer" glReadBuffer :: GLenum -> IO ()

--------------------------------------------------------------------------------

data PixelCopyType =
     CopyColor
   | CopyDepth
   | CopyStencil
   deriving ( Eq, Ord, Show )

marshalPixelCopyType :: PixelCopyType -> GLenum
marshalPixelCopyType x = case x of
   CopyColor -> 0x1800
   CopyDepth -> 0x1801
   CopyStencil -> 0x1802

--------------------------------------------------------------------------------

copyPixels :: Position -> Size -> PixelCopyType -> IO ()
copyPixels (Position x y) (Size w h) t =
   glCopyPixels x y w h (marshalPixelCopyType t)

foreign import CALLCONV unsafe "glCopyPixels" glCopyPixels ::
   GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> IO ()
