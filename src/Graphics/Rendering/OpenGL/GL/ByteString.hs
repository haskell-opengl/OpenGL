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
   B.ByteString, stringQuery, createAndTrimByteString,
   withByteString, withGLstring,
   packUtf8, unpackUtf8
) where

import Data.StateVar
import Foreign.Ptr
import Graphics.Rendering.OpenGL.Raw
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

--------------------------------------------------------------------------------

stringQuery :: (a -> GettableStateVar GLsizei)
            -> (a -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
            -> a
            -> IO B.ByteString
stringQuery lengthVar getStr obj = do
   len <- get (lengthVar obj)
   createByteString len $
      getStr obj len nullPtr

createByteString :: Integral a => a -> (Ptr GLchar -> IO ()) -> IO B.ByteString
createByteString size act = BI.create (fromIntegral size) (act . castPtr)

createAndTrimByteString ::
   (Integral a, Integral b) => a -> (Ptr GLchar -> IO b) -> IO B.ByteString
createAndTrimByteString maxLen act =
   BI.createAndTrim (fromIntegral maxLen) (fmap fromIntegral . act . castPtr)

withByteString :: B.ByteString -> (Ptr GLchar -> GLsizei -> IO b) -> IO b
withByteString bs act =
   BU.unsafeUseAsCStringLen bs $ \(ptr, size) ->
      act (castPtr ptr) (fromIntegral size)

withGLstring :: String -> (Ptr GLchar -> IO a) -> IO a
withGLstring s act = withByteString (packUtf8 (s ++ "\0")) (const . act)

packUtf8 :: String -> B.ByteString
packUtf8 = TE.encodeUtf8 . T.pack

unpackUtf8 :: B.ByteString -> String
unpackUtf8 = T.unpack . TE.decodeUtf8
