--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Environments
-- Copyright   :  (c) Sven Panne 2002-2004
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 3.8.13 (Texture Environments and Texture
-- Functions) of the OpenGL 1.5 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing.Environments (
   TextureEnvMode(..), textureEnvMode
) where

import Foreign.Marshal.Alloc ( alloca )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLint, GLenum, GLfloat )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek1 )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar, makeStateVar )

--------------------------------------------------------------------------------

data TextureEnvTarget =
     TextureEnv
   | TextureFilterControl   -- GL_TEXTURE_LOD_BIAS_EXT
   | PointSprite            -- GL_COORD_REPLACE_NV

marshalTextureEnvTarget :: TextureEnvTarget -> GLenum
marshalTextureEnvTarget x = case x of
   TextureEnv -> 0x2300
   TextureFilterControl -> 0x8500
   PointSprite -> 0x8861

--------------------------------------------------------------------------------

data TextureEnvParameter =
     TexEnvParamTextureEnvMode
   | TexEnvParamTextureEnvColor
   | TexEnvParamCombineRGB
   | TexEnvParamCombineAlpha
   | TexEnvParamSrc0RGB
   | TexEnvParamSrc1RGB
   | TexEnvParamSrc2RGB
   | TexEnvParamSrc0Alpha
   | TexEnvParamSrc1Alpha
   | TexEnvParamSrc2Alpha
   | TexEnvParamOperand0RGB
   | TexEnvParamOperand1RGB
   | TexEnvParamOperand2RGB
   | TexEnvParamOperand0Alpha
   | TexEnvParamOperand1Alpha
   | TexEnvParamOperand2Alpha
   | TexEnvParamRGBScale
   | TexEnvParamAlphaScale

marshalTextureEnvParameter :: TextureEnvParameter -> GLenum
marshalTextureEnvParameter x = case x of
   TexEnvParamTextureEnvMode -> 0x2200
   TexEnvParamTextureEnvColor -> 0x2201
   TexEnvParamCombineRGB -> 0x8571
   TexEnvParamCombineAlpha -> 0x8572
   TexEnvParamSrc0RGB -> 0x8580
   TexEnvParamSrc1RGB -> 0x8581
   TexEnvParamSrc2RGB -> 0x8582
   TexEnvParamSrc0Alpha -> 0x8588
   TexEnvParamSrc1Alpha -> 0x8589
   TexEnvParamSrc2Alpha -> 0x858a
   TexEnvParamOperand0RGB -> 0x8590
   TexEnvParamOperand1RGB -> 0x8591
   TexEnvParamOperand2RGB -> 0x8592
   TexEnvParamOperand0Alpha -> 0x8598
   TexEnvParamOperand1Alpha -> 0x8599
   TexEnvParamOperand2Alpha -> 0x859a
   TexEnvParamRGBScale -> 0x8573
   TexEnvParamAlphaScale -> 0xd1c

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glTexEnvf"
   glTexEnvf :: GLenum -> GLenum ->  GLfloat -> IO ()

texEnvi :: (a -> GLint) -> TextureEnvTarget -> TextureEnvParameter -> a -> IO ()
texEnvi f t p =
   glTexEnvi (marshalTextureEnvTarget t) (marshalTextureEnvParameter p) . f

foreign import CALLCONV unsafe "glTexEnvi"
   glTexEnvi :: GLenum -> GLenum ->  GLint -> IO ()

foreign import CALLCONV unsafe "glTexEnvfv"
   glTexEnvfv :: GLenum -> GLenum -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexEnviv"
   glTexEnviv :: GLenum -> GLenum -> Ptr GLint -> IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glGetTexEnvfv"
   glGetTexEnvfv :: GLenum -> GLenum -> Ptr GLfloat -> IO ()

getTexEnvi :: (GLint -> a) -> TextureEnvTarget -> TextureEnvParameter -> IO a
getTexEnvi f t p =
   alloca $ \buf -> do
     glGetTexEnviv (marshalTextureEnvTarget t) (marshalTextureEnvParameter p) buf
     peek1 f buf

foreign import CALLCONV unsafe "glGetTexEnviv"
   glGetTexEnviv :: GLenum -> GLenum -> Ptr GLint -> IO ()
--------------------------------------------------------------------------------

data TextureEnvMode =
     Modulate
   | Decal
   | Blend
   | Replace
   | Add'
   | Combine
   deriving ( Eq, Ord, Show )

marshalTextureEnvMode :: TextureEnvMode -> GLint
marshalTextureEnvMode x = case x of
   Modulate -> 0x2100
   Decal -> 0x2101
   Blend -> 0xbe2
   Replace -> 0x1e01
   Add' -> 0x104
   Combine -> 0x8570

unmarshalTextureEnvMode :: GLint -> TextureEnvMode
unmarshalTextureEnvMode x
   | x == 0x2100 = Modulate
   | x == 0x2101 = Decal
   | x == 0xbe2 = Blend
   | x == 0x1e01 = Replace
   | x == 0x104 = Add'
   | x == 0x8570 = Combine
   | otherwise = error ("unmarshalTextureEnvMode: illegal value " ++ show x)

--------------------------------------------------------------------------------

textureEnvMode :: StateVar TextureEnvMode
textureEnvMode =
   makeStateVar
      (getTexEnvi unmarshalTextureEnvMode TextureEnv TexEnvParamTextureEnvMode)
      (texEnvi    marshalTextureEnvMode   TextureEnv TexEnvParamTextureEnvMode)
