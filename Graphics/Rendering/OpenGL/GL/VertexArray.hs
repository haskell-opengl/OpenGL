module Graphics.Rendering.OpenGL.GL.VertexArray where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )

data PixelType =
     Bitmap
   | Byte'4
   | UnsignedByte'3
   | Short'5
   | UnsignedShort'3
   | Int'5
   | UnsignedInt'3
   | Float'5
   | BGR
   | BGRA
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
   deriving ( Eq, Ord, Show )

marshalPixelType :: PixelType -> GLenum
marshalPixelType x = case x of
   Bitmap -> 0x1a00
   Byte'4 -> 0x1400
   UnsignedByte'3 -> 0x1401
   Short'5 -> 0x1402
   UnsignedShort'3 -> 0x1403
   Int'5 -> 0x1404
   UnsignedInt'3 -> 0x1405
   Float'5 -> 0x1406
   BGR -> 0x80e0
   BGRA -> 0x80e1
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
