-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.DataType
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling DataType.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.DataType (
   DataType(..), marshalDataType, unmarshalDataType
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )

--------------------------------------------------------------------------------

data DataType =
     Byte
   | UnsignedByte
   | Short
   | UnsignedShort
   | Int
   | UnsignedInt
   | Float
   | TwoBytes
   | ThreeBytes
   | FourBytes
   | Double
   deriving ( Eq, Ord, Show )

marshalDataType :: DataType -> GLenum
marshalDataType x = case x of
   Byte -> 0x1400
   UnsignedByte -> 0x1401
   Short -> 0x1402
   UnsignedShort -> 0x1403
   Int -> 0x1404
   UnsignedInt -> 0x1405
   Float -> 0x1406
   TwoBytes -> 0x1407
   ThreeBytes -> 0x1408
   FourBytes -> 0x1409
   Double -> 0x140a

unmarshalDataType :: GLenum -> DataType
unmarshalDataType x
   | x == 0x1400 = Byte
   | x == 0x1401 = UnsignedByte
   | x == 0x1402 = Short
   | x == 0x1403 = UnsignedShort
   | x == 0x1404 = Int
   | x == 0x1405 = UnsignedInt
   | x == 0x1406 = Float
   | x == 0x1407 = TwoBytes
   | x == 0x1408 = ThreeBytes
   | x == 0x1409 = FourBytes
   | x == 0x140a = Double
   | otherwise = error ("unmarshalDataType: illegal value " ++ show x)
