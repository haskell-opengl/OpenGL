-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.DataType
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
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

-- basically table 3.2 (pixel data type parameter) plus a few additions
data DataType =
     UnsignedByte
   | Byte
   | UnsignedShort
   | Short
   | UnsignedInt
   | Int
   | HalfFloat
   | Float
   | UnsignedByte332
   | UnsignedByte233Rev
   | UnsignedShort565
   | UnsignedShort565Rev
   | UnsignedShort4444
   | UnsignedShort4444Rev
   | UnsignedShort5551
   | UnsignedShort1555Rev
   | UnsignedInt8888
   | UnsignedInt8888Rev
   | UnsignedInt1010102
   | UnsignedInt2101010Rev
   | UnsignedInt248
   | UnsignedInt10f11f11fRev
   | UnsignedInt5999Rev
   | Float32UnsignedInt248Rev
   | Bitmap                    -- pixel data, deprecated in 3.1
   | UnsignedShort88           -- MESA_ycbcr_texture/APPLE_ycbcr_422
   | UnsignedShort88Rev        -- MESA_ycbcr_texture/APPLE_ycbcr_422
   | Double                    -- vertex arrays (EXT_vertex_array, now core)
   | TwoBytes                  -- CallLists
   | ThreeBytes                -- CallLists
   | FourBytes                 -- CallLists
   deriving ( Eq, Ord, Show )

marshalDataType :: DataType -> GLenum
marshalDataType x = case x of
   UnsignedByte -> 0x1401
   Byte -> 0x1400
   UnsignedShort -> 0x1403
   Short -> 0x1402
   UnsignedInt -> 0x1405
   Int -> 0x1404
   HalfFloat -> 0x140B
   Float -> 0x1406
   UnsignedByte332 -> 0x8032
   UnsignedByte233Rev -> 0x8362
   UnsignedShort565 -> 0x8363
   UnsignedShort565Rev -> 0x8364
   UnsignedShort4444 -> 0x8033
   UnsignedShort4444Rev -> 0x8365
   UnsignedShort5551 -> 0x8034
   UnsignedShort1555Rev -> 0x8366
   UnsignedInt8888 -> 0x8035
   UnsignedInt8888Rev -> 0x8367
   UnsignedInt1010102 -> 0x8036
   UnsignedInt2101010Rev -> 0x8368
   UnsignedInt248 -> 0x84fa
   UnsignedInt10f11f11fRev -> 0x8C3B
   UnsignedInt5999Rev -> 0x8C3E
   Float32UnsignedInt248Rev -> 0x8DAD
   Bitmap -> 0x1a00
   UnsignedShort88 -> 0x85ba
   UnsignedShort88Rev -> 0x85bb
   Double -> 0x140a
   TwoBytes -> 0x1407
   ThreeBytes -> 0x1408
   FourBytes -> 0x1409

unmarshalDataType :: GLenum -> DataType
unmarshalDataType x
   | x == 0x1401 = UnsignedByte
   | x == 0x1400 = Byte
   | x == 0x1403 = UnsignedShort
   | x == 0x1402 = Short
   | x == 0x1405 = UnsignedInt
   | x == 0x1404 = Int
   | x == 0x140B = HalfFloat
   | x == 0x1406 = Float
   | x == 0x8032 = UnsignedByte332
   | x == 0x8362 = UnsignedByte233Rev
   | x == 0x8363 = UnsignedShort565
   | x == 0x8364 = UnsignedShort565Rev
   | x == 0x8033 = UnsignedShort4444
   | x == 0x8365 = UnsignedShort4444Rev
   | x == 0x8034 = UnsignedShort5551
   | x == 0x8366 = UnsignedShort1555Rev
   | x == 0x8035 = UnsignedInt8888
   | x == 0x8367 = UnsignedInt8888Rev
   | x == 0x8036 = UnsignedInt1010102
   | x == 0x8368 = UnsignedInt2101010Rev
   | x == 0x84fa = UnsignedInt248
   | x == 0x8C3B = UnsignedInt10f11f11fRev
   | x == 0x8C3E = UnsignedInt5999Rev
   | x == 0x8DAD = Float32UnsignedInt248Rev
   | x == 0x1a00 = Bitmap
   | x == 0x85ba = UnsignedShort88
   | x == 0x85bb = UnsignedShort88Rev
   | x == 0x140a = Double
   | x == 0x1407 = TwoBytes
   | x == 0x1408 = ThreeBytes
   | x == 0x1409 = FourBytes
   | otherwise = error ("unmarshalDataType: illegal value " ++ show x)
