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
   TextureEnvMode(..), textureEnvMode, textureEnvColor, textureUnitLODBias
) where

import Foreign.Marshal.Alloc ( alloca )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable )
import Foreign.Marshal.Utils ( with )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLint, GLenum, GLfloat )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek1 )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.Texturing.Parameters ( LOD )
import Graphics.Rendering.OpenGL.GL.VertexSpec( Color4(..) )

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
   | TexEnvParamSrc3RGB
   | TexEnvParamSrc0Alpha
   | TexEnvParamSrc1Alpha
   | TexEnvParamSrc2Alpha
   | TexEnvParamSrc3Alpha
   | TexEnvParamOperand0RGB
   | TexEnvParamOperand1RGB
   | TexEnvParamOperand2RGB
   | TexEnvParamOperand3RGB
   | TexEnvParamOperand0Alpha
   | TexEnvParamOperand1Alpha
   | TexEnvParamOperand2Alpha
   | TexEnvParamOperand3Alpha
   | TexEnvParamRGBScale
   | TexEnvParamAlphaScale
   | TexEnvParamLODBias

marshalTextureEnvParameter :: TextureEnvParameter -> GLenum
marshalTextureEnvParameter x = case x of
   TexEnvParamTextureEnvMode -> 0x2200
   TexEnvParamTextureEnvColor -> 0x2201
   TexEnvParamCombineRGB -> 0x8571
   TexEnvParamCombineAlpha -> 0x8572
   TexEnvParamSrc0RGB -> 0x8580
   TexEnvParamSrc1RGB -> 0x8581
   TexEnvParamSrc2RGB -> 0x8582
   TexEnvParamSrc3RGB -> 0x8583
   TexEnvParamSrc0Alpha -> 0x8588
   TexEnvParamSrc1Alpha -> 0x8589
   TexEnvParamSrc2Alpha -> 0x858a
   TexEnvParamSrc3Alpha -> 0x858b
   TexEnvParamOperand0RGB -> 0x8590
   TexEnvParamOperand1RGB -> 0x8591
   TexEnvParamOperand2RGB -> 0x8592
   TexEnvParamOperand3RGB -> 0x8593
   TexEnvParamOperand0Alpha -> 0x8598
   TexEnvParamOperand1Alpha -> 0x8599
   TexEnvParamOperand2Alpha -> 0x859a
   TexEnvParamOperand3Alpha -> 0x859b
   TexEnvParamRGBScale -> 0x8573
   TexEnvParamAlphaScale -> 0xd1c
   TexEnvParamLODBias -> 0x8501

--------------------------------------------------------------------------------

texEnv :: (GLenum -> GLenum -> b -> IO ())
       -> (a -> (b -> IO ()) -> IO ())
       -> TextureEnvTarget -> TextureEnvParameter -> a -> IO ()
texEnv glTexEnv marshalAct t p x =
   marshalAct x $
      glTexEnv (marshalTextureEnvTarget t) (marshalTextureEnvParameter p)

foreign import CALLCONV unsafe "glTexEnvi"
   glTexEnvi :: GLenum -> GLenum ->  GLint -> IO ()

foreign import CALLCONV unsafe "glTexEnvf"
   glTexEnvf :: GLenum -> GLenum ->  GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexEnvfv"
   glTexEnvC4f :: GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()

--------------------------------------------------------------------------------

getTexEnv :: Storable b
          => (GLenum -> GLenum -> Ptr b -> IO ())
          -> (b -> a)
          -> TextureEnvTarget -> TextureEnvParameter -> IO a
getTexEnv glGetTexEnv unmarshal t p =
   alloca $ \buf -> do
     glGetTexEnv (marshalTextureEnvTarget t) (marshalTextureEnvParameter p) buf
     peek1 unmarshal buf

foreign import CALLCONV unsafe "glGetTexEnviv"
   glGetTexEnviv :: GLenum -> GLenum -> Ptr GLint -> IO ()

foreign import CALLCONV unsafe "glGetTexEnvfv"
   glGetTexEnvfv :: GLenum -> GLenum -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glGetTexEnvfv"
   glGetTexEnvC4f :: GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()

--------------------------------------------------------------------------------

m2a :: (a -> b) -> a -> (b -> IO ()) -> IO ()
m2a marshal x act = act (marshal x)

texEnvi ::
   (GLint -> a) -> (a -> GLint) -> TextureEnvTarget -> TextureEnvParameter -> StateVar a
texEnvi unmarshal marshal t p =
   makeStateVar
      (getTexEnv glGetTexEnviv unmarshal     t p)
      (texEnv    glTexEnvi     (m2a marshal) t p)

texEnvf ::
   (GLfloat -> a) -> (a -> GLfloat) -> TextureEnvTarget -> TextureEnvParameter -> StateVar a
texEnvf unmarshal marshal t p =
   makeStateVar
      (getTexEnv glGetTexEnvfv unmarshal     t p)
      (texEnv    glTexEnvf     (m2a marshal) t p)

texEnvC4f :: TextureEnvTarget -> TextureEnvParameter -> StateVar (Color4 GLfloat)
texEnvC4f t p =
   makeStateVar
      (getTexEnv glGetTexEnvC4f id   t p)
      (texEnv    glTexEnvC4f    with t p)

--------------------------------------------------------------------------------

data TextureEnvMode =
     Modulate
   | Decal
   | Blend
   | Replace
   | Add'
   | Combine
   | Combine4
   deriving ( Eq, Ord, Show )

marshalTextureEnvMode :: TextureEnvMode -> GLint
marshalTextureEnvMode x = case x of
   Modulate -> 0x2100
   Decal -> 0x2101
   Blend -> 0xbe2
   Replace -> 0x1e01
   Add' -> 0x104
   Combine -> 0x8570
   Combine4 -> 0x8503

unmarshalTextureEnvMode :: GLint -> TextureEnvMode
unmarshalTextureEnvMode x
   | x == 0x2100 = Modulate
   | x == 0x2101 = Decal
   | x == 0xbe2 = Blend
   | x == 0x1e01 = Replace
   | x == 0x104 = Add'
   | x == 0x8570 = Combine
   | x == 0x8503 = Combine4
   | otherwise = error ("unmarshalTextureEnvMode: illegal value " ++ show x)

--------------------------------------------------------------------------------

textureEnvMode :: StateVar TextureEnvMode
textureEnvMode =
   texEnvi unmarshalTextureEnvMode marshalTextureEnvMode TextureEnv TexEnvParamTextureEnvMode

--------------------------------------------------------------------------------

textureEnvColor :: StateVar (Color4 GLfloat)
textureEnvColor = texEnvC4f TextureEnv TexEnvParamTextureEnvColor

--------------------------------------------------------------------------------

textureUnitLODBias :: StateVar LOD
textureUnitLODBias = texEnvf id id TextureFilterControl TexEnvParamLODBias
