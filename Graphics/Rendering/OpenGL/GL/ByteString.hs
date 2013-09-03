{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.ByteString
-- Copyright   :  (c) Sven Panne 2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for interfacing with ByteStrings.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.ByteString (
   B.ByteString, createByteString, withByteString
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Foreign.Ptr
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

createByteString :: Integral a => a -> (Ptr b -> IO ()) -> IO B.ByteString
createByteString size act = BI.create (fromIntegral size) (act . castPtr)

withByteString :: B.ByteString -> (Ptr a -> GLsizei -> IO b) -> IO b
withByteString bs act =
   BU.unsafeUseAsCStringLen bs $ \(ptr, size) ->
      act (castPtr ptr) (fromIntegral size)
