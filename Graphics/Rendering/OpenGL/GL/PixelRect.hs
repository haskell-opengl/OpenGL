module Graphics.Rendering.OpenGL.GL.PixelRect where

import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )
import Graphics.Rendering.OpenGL.GL.VertexArray ( PixelType )

data PixelFormat =
     ColorIndex
   | StencilIndex
   | DepthComponent
   | Red
   | Green
   | Blue
   | Alpha
   | RGB
   | RGBA
   | Luminance
   | LuminanceAlpha
   deriving ( Eq, Ord, Show )

marshalPixelFormat :: PixelFormat -> GLenum
marshalPixelFormat x = case x of
   ColorIndex -> 0x1900
   StencilIndex -> 0x1901
   DepthComponent -> 0x1902
   Red -> 0x1903
   Green -> 0x1904
   Blue -> 0x1905
   Alpha -> 0x1906
   RGB -> 0x1907
   RGBA -> 0x1908
   Luminance -> 0x1909
   LuminanceAlpha -> 0x190a

data PixelDescriptor  = PixelDescriptor PixelFormat PixelType (Ptr ())
