module Graphics.Rendering.OpenGL.GL.Texturing where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )

data TextureTarget =
     Texture1D
   | Texture2D
   | Texture3D
   | ProxyTexture1D
   | ProxyTexture2D
   | ProxyTexture3D
   | TextureCubeMap
   | ProxyTextureCubeMap
   | TextureCubeMapPositiveX
   | TextureCubeMapNegativeX
   | TextureCubeMapPositiveY
   | TextureCubeMapNegativeY
   | TextureCubeMapPositiveZ
   | TextureCubeMapNegativeZ
   deriving ( Eq, Ord, Show )

marshalTextureTarget :: TextureTarget -> GLenum
marshalTextureTarget x = case x of
   Texture1D -> 0xde0
   Texture2D -> 0xde1
   Texture3D -> 0x806f
   ProxyTexture1D -> 0x8063
   ProxyTexture2D -> 0x8064
   ProxyTexture3D -> 0x8070
   TextureCubeMap -> 0x8513
   ProxyTextureCubeMap -> 0x851b
   TextureCubeMapPositiveX -> 0x8515
   TextureCubeMapNegativeX -> 0x8516
   TextureCubeMapPositiveY -> 0x8517
   TextureCubeMapNegativeY -> 0x8518
   TextureCubeMapPositiveZ -> 0x8519
   TextureCubeMapNegativeZ -> 0x851a

data PixelInternalFormat =
     Alpha4
   | Alpha8
   | Alpha12
   | Alpha16
   | DepthComponent16
   | DepthComponent24
   | DepthComponent32
   | Luminance4
   | Luminance8
   | Luminance12
   | Luminance16
   | Luminance4Alpha4
   | Luminance6Alpha2
   | Luminance8Alpha8
   | Luminance12Alpha4
   | Luminance12Alpha12
   | Luminance16Alpha16
   | Intensity
   | Intensity4
   | Intensity8
   | Intensity12
   | Intensity16
   | R3G3B2
   | Rgb4
   | Rgb5
   | Rgb8
   | Rgb10
   | Rgb12
   | Rgb16
   | Rgba2
   | Rgba4
   | Rgb5A1
   | Rgba8
   | Rgb10A2
   | Rgba12
   | Rgba16
   | CompressedAlpha
   | CompressedLuminance
   | CompressedLuminanceAlpha
   | CompressedIntensity
   | CompressedRGB
   | CompressedRGBA
   deriving ( Eq, Ord, Show )

marshalPixelInternalFormat :: PixelInternalFormat -> GLenum
marshalPixelInternalFormat x = case x of
   Alpha4 -> 0x803b
   Alpha8 -> 0x803c
   Alpha12 -> 0x803d
   Alpha16 -> 0x803e
   DepthComponent16 -> 0x81a5
   DepthComponent24 -> 0x81a6
   DepthComponent32 -> 0x81a7
   Luminance4 -> 0x803f
   Luminance8 -> 0x8040
   Luminance12 -> 0x8041
   Luminance16 -> 0x8042
   Luminance4Alpha4 -> 0x8043
   Luminance6Alpha2 -> 0x8044
   Luminance8Alpha8 -> 0x8045
   Luminance12Alpha4 -> 0x8046
   Luminance12Alpha12 -> 0x8047
   Luminance16Alpha16 -> 0x8048
   Intensity -> 0x8049
   Intensity4 -> 0x804a
   Intensity8 -> 0x804b
   Intensity12 -> 0x804c
   Intensity16 -> 0x804d
   R3G3B2 -> 0x2a10
   Rgb4 -> 0x804f
   Rgb5 -> 0x8050
   Rgb8 -> 0x8051
   Rgb10 -> 0x8052
   Rgb12 -> 0x8053
   Rgb16 -> 0x8054
   Rgba2 -> 0x8055
   Rgba4 -> 0x8056
   Rgb5A1 -> 0x8057
   Rgba8 -> 0x8058
   Rgb10A2 -> 0x8059
   Rgba12 -> 0x805a
   Rgba16 -> 0x805b
   CompressedAlpha -> 0x84e9
   CompressedLuminance -> 0x84ea
   CompressedLuminanceAlpha -> 0x84eb
   CompressedIntensity -> 0x84ec
   CompressedRGB -> 0x84ed
   CompressedRGBA -> 0x84ee
