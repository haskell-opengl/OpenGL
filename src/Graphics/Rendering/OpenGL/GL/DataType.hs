{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.DataType
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling DataType.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.DataType (
   DataType(..), marshalDataType, unmarshalDataType,
   DataTypeType(..), marshalDataTypeType, unmarshalDataTypeType
) where

import Graphics.Rendering.OpenGL.Raw

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
   UnsignedByte -> gl_UNSIGNED_BYTE
   Byte -> gl_BYTE
   UnsignedShort -> gl_UNSIGNED_SHORT
   Short -> gl_SHORT
   UnsignedInt -> gl_UNSIGNED_INT
   Int -> gl_INT
   HalfFloat -> gl_HALF_FLOAT
   Float -> gl_FLOAT
   UnsignedByte332 -> gl_UNSIGNED_BYTE_3_3_2
   UnsignedByte233Rev -> gl_UNSIGNED_BYTE_2_3_3_REV
   UnsignedShort565 -> gl_UNSIGNED_SHORT_5_6_5
   UnsignedShort565Rev -> gl_UNSIGNED_SHORT_5_6_5_REV
   UnsignedShort4444 -> gl_UNSIGNED_SHORT_4_4_4_4
   UnsignedShort4444Rev -> gl_UNSIGNED_SHORT_4_4_4_4_REV
   UnsignedShort5551 -> gl_UNSIGNED_SHORT_5_5_5_1
   UnsignedShort1555Rev -> gl_UNSIGNED_SHORT_1_5_5_5_REV
   UnsignedInt8888 -> gl_UNSIGNED_INT_8_8_8_8
   UnsignedInt8888Rev -> gl_UNSIGNED_INT_8_8_8_8_REV
   UnsignedInt1010102 -> gl_UNSIGNED_INT_10_10_10_2
   UnsignedInt2101010Rev -> gl_UNSIGNED_INT_2_10_10_10_REV
   UnsignedInt248 -> gl_UNSIGNED_INT_24_8
   UnsignedInt10f11f11fRev -> gl_UNSIGNED_INT_10F_11F_11F_REV
   UnsignedInt5999Rev -> gl_UNSIGNED_INT_5_9_9_9_REV
   Float32UnsignedInt248Rev -> gl_FLOAT_32_UNSIGNED_INT_24_8_REV
   Bitmap -> gl_BITMAP
   UnsignedShort88 -> gl_UNSIGNED_SHORT_8_8_APPLE
   UnsignedShort88Rev -> gl_UNSIGNED_SHORT_8_8_REV_APPLE
   Double -> gl_DOUBLE
   TwoBytes -> gl_2_BYTES
   ThreeBytes -> gl_3_BYTES
   FourBytes -> gl_4_BYTES

unmarshalDataType :: GLenum -> DataType
unmarshalDataType x
   | x == gl_UNSIGNED_BYTE = UnsignedByte
   | x == gl_BYTE = Byte
   | x == gl_UNSIGNED_SHORT = UnsignedShort
   | x == gl_SHORT = Short
   | x == gl_UNSIGNED_INT = UnsignedInt
   | x == gl_INT = Int
   | x == gl_HALF_FLOAT = HalfFloat
   | x == gl_FLOAT = Float
   | x == gl_UNSIGNED_BYTE_3_3_2 = UnsignedByte332
   | x == gl_UNSIGNED_BYTE_2_3_3_REV = UnsignedByte233Rev
   | x == gl_UNSIGNED_SHORT_5_6_5 = UnsignedShort565
   | x == gl_UNSIGNED_SHORT_5_6_5_REV = UnsignedShort565Rev
   | x == gl_UNSIGNED_SHORT_4_4_4_4 = UnsignedShort4444
   | x == gl_UNSIGNED_SHORT_4_4_4_4_REV = UnsignedShort4444Rev
   | x == gl_UNSIGNED_SHORT_5_5_5_1 = UnsignedShort5551
   | x == gl_UNSIGNED_SHORT_1_5_5_5_REV = UnsignedShort1555Rev
   | x == gl_UNSIGNED_INT_8_8_8_8 = UnsignedInt8888
   | x == gl_UNSIGNED_INT_8_8_8_8_REV = UnsignedInt8888Rev
   | x == gl_UNSIGNED_INT_10_10_10_2 = UnsignedInt1010102
   | x == gl_UNSIGNED_INT_2_10_10_10_REV = UnsignedInt2101010Rev
   | x == gl_UNSIGNED_INT_24_8 = UnsignedInt248
   | x == gl_UNSIGNED_INT_10F_11F_11F_REV = UnsignedInt10f11f11fRev
   | x == gl_UNSIGNED_INT_5_9_9_9_REV = UnsignedInt5999Rev
   | x == gl_FLOAT_32_UNSIGNED_INT_24_8_REV = Float32UnsignedInt248Rev
   | x == gl_BITMAP = Bitmap
   | x == gl_UNSIGNED_SHORT_8_8_APPLE = UnsignedShort88
   | x == gl_UNSIGNED_SHORT_8_8_REV_APPLE = UnsignedShort88Rev
   | x == gl_DOUBLE = Double
   | x == gl_2_BYTES = TwoBytes
   | x == gl_3_BYTES = ThreeBytes
   | x == gl_4_BYTES = FourBytes
   | otherwise = error ("unmarshalDataType: illegal value " ++ show x)

data DataTypeType
   = TNone
   | TSignedNormalized
   | TUnsignedNormalized
   | TFloat
   | TInt
   | TUnsignedInt

marshalDataTypeType :: DataTypeType -> GLenum
marshalDataTypeType x = case x of
   TNone -> gl_NONE
   TSignedNormalized -> gl_SIGNED_NORMALIZED
   TUnsignedNormalized -> gl_UNSIGNED_NORMALIZED
   TFloat -> gl_FLOAT
   TInt -> gl_INT
   TUnsignedInt -> gl_UNSIGNED_INT

unmarshalDataTypeType :: GLenum -> DataTypeType
unmarshalDataTypeType x
   | x == gl_NONE = TNone
   | x == gl_SIGNED_NORMALIZED = TSignedNormalized
   | x == gl_UNSIGNED_NORMALIZED = TUnsignedNormalized
   | x == gl_FLOAT = TFloat
   | x == gl_INT = TInt
   | x == gl_UNSIGNED_INT = TUnsignedInt
   | otherwise = error $ "unmarshalDataTypeType: illegal value " ++ show x
