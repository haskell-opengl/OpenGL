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
   | Bitmap
   | UnsignedByte332
   | UnsignedShort4444
   | UnsignedShort5551
   | UnsignedInt8888
   | UnsignedInt1010102
   | UnsignedByte233Rev
   | UnsignedShort565
   | UnsignedShort565Rev
   | UnsignedShort4444Rev
   | UnsignedShort1555Rev
   | UnsignedInt8888Rev
   | UnsignedInt2101010Rev
   | UnsignedShort88
   | UnsignedShort88Rev
   | UnsignedInt248
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
   Bitmap -> 0x1a00
   UnsignedByte332 -> 0x8032
   UnsignedShort4444 -> 0x8033
   UnsignedShort5551 -> 0x8034
   UnsignedInt8888 -> 0x8035
   UnsignedInt1010102 -> 0x8036
   UnsignedByte233Rev -> 0x8362
   UnsignedShort565 -> 0x8363
   UnsignedShort565Rev -> 0x8364
   UnsignedShort4444Rev -> 0x8365
   UnsignedShort1555Rev -> 0x8366
   UnsignedInt8888Rev -> 0x8367
   UnsignedInt2101010Rev -> 0x8368
   UnsignedShort88 -> 0x85ba
   UnsignedShort88Rev -> 0x85bb
   UnsignedInt248 -> 0x84fa

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
   | x == 0x1a00 = Bitmap
   | x == 0x8032 = UnsignedByte332
   | x == 0x8033 = UnsignedShort4444
   | x == 0x8034 = UnsignedShort5551
   | x == 0x8035 = UnsignedInt8888
   | x == 0x8036 = UnsignedInt1010102
   | x == 0x8362 = UnsignedByte233Rev
   | x == 0x8363 = UnsignedShort565
   | x == 0x8364 = UnsignedShort565Rev
   | x == 0x8365 = UnsignedShort4444Rev
   | x == 0x8366 = UnsignedShort1555Rev
   | x == 0x8367 = UnsignedInt8888Rev
   | x == 0x8368 = UnsignedInt2101010Rev
   | x == 0x85ba = UnsignedShort88
   | x == 0x85bb = UnsignedShort88Rev
   | x == 0x84fa = UnsignedInt248
   | otherwise = error ("unmarshalDataType: illegal value " ++ show x)
