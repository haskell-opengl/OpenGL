{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.DataType
-- Copyright   :  (c) Sven Panne 2002-2019
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
   DataRepresentation(..), unmarshalDataRepresentation
) where

import Graphics.GL

--------------------------------------------------------------------------------

-- basically table 8.7 (pixel data type parameter) plus a few additions
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
   UnsignedByte -> GL_UNSIGNED_BYTE
   Byte -> GL_BYTE
   UnsignedShort -> GL_UNSIGNED_SHORT
   Short -> GL_SHORT
   UnsignedInt -> GL_UNSIGNED_INT
   Int -> GL_INT
   HalfFloat -> GL_HALF_FLOAT
   Float -> GL_FLOAT
   UnsignedByte332 -> GL_UNSIGNED_BYTE_3_3_2
   UnsignedByte233Rev -> GL_UNSIGNED_BYTE_2_3_3_REV
   UnsignedShort565 -> GL_UNSIGNED_SHORT_5_6_5
   UnsignedShort565Rev -> GL_UNSIGNED_SHORT_5_6_5_REV
   UnsignedShort4444 -> GL_UNSIGNED_SHORT_4_4_4_4
   UnsignedShort4444Rev -> GL_UNSIGNED_SHORT_4_4_4_4_REV
   UnsignedShort5551 -> GL_UNSIGNED_SHORT_5_5_5_1
   UnsignedShort1555Rev -> GL_UNSIGNED_SHORT_1_5_5_5_REV
   UnsignedInt8888 -> GL_UNSIGNED_INT_8_8_8_8
   UnsignedInt8888Rev -> GL_UNSIGNED_INT_8_8_8_8_REV
   UnsignedInt1010102 -> GL_UNSIGNED_INT_10_10_10_2
   UnsignedInt2101010Rev -> GL_UNSIGNED_INT_2_10_10_10_REV
   UnsignedInt248 -> GL_UNSIGNED_INT_24_8
   UnsignedInt10f11f11fRev -> GL_UNSIGNED_INT_10F_11F_11F_REV
   UnsignedInt5999Rev -> GL_UNSIGNED_INT_5_9_9_9_REV
   Float32UnsignedInt248Rev -> GL_FLOAT_32_UNSIGNED_INT_24_8_REV
   Bitmap -> GL_BITMAP
   UnsignedShort88 -> GL_UNSIGNED_SHORT_8_8_APPLE
   UnsignedShort88Rev -> GL_UNSIGNED_SHORT_8_8_REV_APPLE
   Double -> GL_DOUBLE
   TwoBytes -> GL_2_BYTES
   ThreeBytes -> GL_3_BYTES
   FourBytes -> GL_4_BYTES

unmarshalDataType :: GLenum -> DataType
unmarshalDataType x
   | x == GL_UNSIGNED_BYTE = UnsignedByte
   | x == GL_BYTE = Byte
   | x == GL_UNSIGNED_SHORT = UnsignedShort
   | x == GL_SHORT = Short
   | x == GL_UNSIGNED_INT = UnsignedInt
   | x == GL_INT = Int
   | x == GL_HALF_FLOAT = HalfFloat
   | x == GL_FLOAT = Float
   | x == GL_UNSIGNED_BYTE_3_3_2 = UnsignedByte332
   | x == GL_UNSIGNED_BYTE_2_3_3_REV = UnsignedByte233Rev
   | x == GL_UNSIGNED_SHORT_5_6_5 = UnsignedShort565
   | x == GL_UNSIGNED_SHORT_5_6_5_REV = UnsignedShort565Rev
   | x == GL_UNSIGNED_SHORT_4_4_4_4 = UnsignedShort4444
   | x == GL_UNSIGNED_SHORT_4_4_4_4_REV = UnsignedShort4444Rev
   | x == GL_UNSIGNED_SHORT_5_5_5_1 = UnsignedShort5551
   | x == GL_UNSIGNED_SHORT_1_5_5_5_REV = UnsignedShort1555Rev
   | x == GL_UNSIGNED_INT_8_8_8_8 = UnsignedInt8888
   | x == GL_UNSIGNED_INT_8_8_8_8_REV = UnsignedInt8888Rev
   | x == GL_UNSIGNED_INT_10_10_10_2 = UnsignedInt1010102
   | x == GL_UNSIGNED_INT_2_10_10_10_REV = UnsignedInt2101010Rev
   | x == GL_UNSIGNED_INT_24_8 = UnsignedInt248
   | x == GL_UNSIGNED_INT_10F_11F_11F_REV = UnsignedInt10f11f11fRev
   | x == GL_UNSIGNED_INT_5_9_9_9_REV = UnsignedInt5999Rev
   | x == GL_FLOAT_32_UNSIGNED_INT_24_8_REV = Float32UnsignedInt248Rev
   | x == GL_BITMAP = Bitmap
   | x == GL_UNSIGNED_SHORT_8_8_APPLE = UnsignedShort88
   | x == GL_UNSIGNED_SHORT_8_8_REV_APPLE = UnsignedShort88Rev
   | x == GL_DOUBLE = Double
   | x == GL_2_BYTES = TwoBytes
   | x == GL_3_BYTES = ThreeBytes
   | x == GL_4_BYTES = FourBytes
   | otherwise = error ("unmarshalDataType: illegal value " ++ show x)

data DataRepresentation
   = SignedNormalizedRepresentation
   | UnsignedNormalizedRepresentation
   | FloatRepresentation
   | IntRepresentation
   | UnsignedIntRepresentation
   deriving ( Eq, Ord, Show )

unmarshalDataRepresentation :: GLenum -> Maybe DataRepresentation
unmarshalDataRepresentation x
   | x == GL_SIGNED_NORMALIZED = Just SignedNormalizedRepresentation
   | x == GL_UNSIGNED_NORMALIZED = Just UnsignedNormalizedRepresentation
   | x == GL_FLOAT = Just FloatRepresentation
   | x == GL_INT = Just IntRepresentation
   | x == GL_UNSIGNED_INT = Just UnsignedIntRepresentation
   | x == GL_NONE = Nothing
   | otherwise = error $ "unmarshalDataRepresentation: illegal value " ++ show x
