--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Environments
-- Copyright   :  (c) Sven Panne 2002-2005
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
   TextureFunction(..), textureFunction,
   TextureCombineFunction(..), combineRGB, combineAlpha,
   ArgNum(..), Arg(..), Src(..), argRGB, argAlpha,
   rgbScale, alphaScale,
   constantColor, textureUnitLODBias
) where

import Control.Monad ( liftM2 )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable )
import Foreign.Marshal.Utils ( with )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLint, GLenum, GLfloat )
import Graphics.Rendering.OpenGL.GL.BlendingFactor (
   marshalBlendingFactor, unmarshalBlendingFactor )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek1 )
import Graphics.Rendering.OpenGL.GL.PerFragment ( BlendingFactor )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasGetter(get), HasSetter(($=)), StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.Texturing.Parameters ( LOD )
import Graphics.Rendering.OpenGL.GL.Texturing.TextureUnit (
   marshalTextureUnit, unmarshalTextureUnit )
import Graphics.Rendering.OpenGL.GL.VertexSpec( Color4(..), TextureUnit )

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

data TextureFunction =
     Modulate
   | Decal
   | Blend
   | Replace
   | AddUnsigned
   | Combine
   | Combine4
   deriving ( Eq, Ord, Show )

marshalTextureFunction :: TextureFunction -> GLint
marshalTextureFunction x = case x of
   Modulate -> 0x2100
   Decal -> 0x2101
   Blend -> 0xbe2
   Replace -> 0x1e01
   AddUnsigned -> 0x104
   Combine -> 0x8570
   Combine4 -> 0x8503

unmarshalTextureFunction :: GLint -> TextureFunction
unmarshalTextureFunction x
   | x == 0x2100 = Modulate
   | x == 0x2101 = Decal
   | x == 0xbe2 = Blend
   | x == 0x1e01 = Replace
   | x == 0x104 = AddUnsigned
   | x == 0x8570 = Combine
   | x == 0x8503 = Combine4
   | otherwise = error ("unmarshalTextureFunction: illegal value " ++ show x)

--------------------------------------------------------------------------------

textureFunction :: StateVar TextureFunction
textureFunction =
   texEnvi unmarshalTextureFunction marshalTextureFunction TextureEnv TexEnvParamTextureEnvMode

--------------------------------------------------------------------------------

data TextureCombineFunction =
     Replace'
   | Modulate'
   | AddUnsigned'
   | AddSigned
   | Interpolate
   | Subtract
   | Dot3RGB
   | Dot3RGBA
   deriving ( Eq, Ord, Show )

marshalTextureCombineFunction :: TextureCombineFunction -> GLint
marshalTextureCombineFunction x = case x of
   Replace' -> 0x1e01
   Modulate' -> 0x2100
   AddUnsigned' -> 0x104
   AddSigned -> 0x8574
   Interpolate -> 0x8575
   Subtract -> 0x84e7
   Dot3RGB -> 0x86ae
   Dot3RGBA -> 0x86af

unmarshalTextureCombineFunction :: GLint -> TextureCombineFunction
unmarshalTextureCombineFunction x
   | x == 0x1e01 = Replace'
   | x == 0x2100 = Modulate'
   | x == 0x104 = AddUnsigned'
   | x == 0x8574 = AddSigned
   | x == 0x8575 = Interpolate
   | x == 0x84e7 = Subtract
   | x == 0x86ae = Dot3RGB
   | x == 0x86af = Dot3RGBA
   | otherwise = error ("unmarshalTextureCombineFunction: illegal value " ++ show x)

--------------------------------------------------------------------------------

combineRGB :: StateVar TextureCombineFunction
combineRGB = combine TexEnvParamCombineRGB

combineAlpha :: StateVar TextureCombineFunction
combineAlpha = combine TexEnvParamCombineAlpha

combine :: TextureEnvParameter -> StateVar TextureCombineFunction
combine =
   texEnvi unmarshalTextureCombineFunction marshalTextureCombineFunction TextureEnv

--------------------------------------------------------------------------------

data ArgNum =
     Arg0
   | Arg1
   | Arg2
   | Arg3
   deriving ( Eq, Ord, Show )

argNumToOperandRGB :: ArgNum -> TextureEnvParameter
argNumToOperandRGB x = case x of
   Arg0 -> TexEnvParamOperand0RGB
   Arg1 -> TexEnvParamOperand1RGB
   Arg2 -> TexEnvParamOperand2RGB
   Arg3 -> TexEnvParamOperand3RGB

argNumToOperandAlpha :: ArgNum -> TextureEnvParameter
argNumToOperandAlpha x = case x of
   Arg0 -> TexEnvParamOperand0Alpha
   Arg1 -> TexEnvParamOperand1Alpha
   Arg2 -> TexEnvParamOperand2Alpha
   Arg3 -> TexEnvParamOperand3Alpha

argNumToSrcRGB :: ArgNum -> TextureEnvParameter
argNumToSrcRGB x = case x of
   Arg0 -> TexEnvParamSrc0RGB
   Arg1 -> TexEnvParamSrc1RGB
   Arg2 -> TexEnvParamSrc2RGB
   Arg3 -> TexEnvParamSrc3RGB

argNumToSrcAlpha :: ArgNum -> TextureEnvParameter
argNumToSrcAlpha x = case x of
   Arg0 -> TexEnvParamSrc0Alpha
   Arg1 -> TexEnvParamSrc1Alpha
   Arg2 -> TexEnvParamSrc2Alpha
   Arg3 -> TexEnvParamSrc3Alpha

--------------------------------------------------------------------------------

data Arg = Arg BlendingFactor Src
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

data Src =
     CurrentUnit
   | Previous
   | Crossbar TextureUnit
   | Constant
   | PrimaryColor
   deriving ( Eq, Ord, Show )

marshalSrc :: Src -> GLint
marshalSrc x = case x of
   CurrentUnit -> 0x1702
   Previous -> 0x8578
   Crossbar u -> fromIntegral (marshalTextureUnit u)
   Constant -> 0x8576
   PrimaryColor -> 0x8577

unmarshalSrc :: GLint -> Src
unmarshalSrc x
   | x == 0x1702 = CurrentUnit
   | x == 0x8578 = Previous
   | x == 0x8576 = Constant
   | x == 0x8577 = PrimaryColor
   | otherwise = Crossbar (unmarshalTextureUnit (fromIntegral x))

--------------------------------------------------------------------------------

argRGB :: ArgNum -> StateVar Arg
argRGB n = arg (argNumToOperandRGB n) (argNumToSrcRGB n)

argAlpha :: ArgNum -> StateVar Arg
argAlpha n = arg (argNumToOperandAlpha n) (argNumToSrcAlpha n)

arg :: TextureEnvParameter -> TextureEnvParameter -> StateVar Arg
arg op src = combineArg (textureEnvOperand op) (textureEnvSrc src)
   where combineArg v w = makeStateVar
                             (liftM2 Arg (get v) (get w))
                             (\(Arg x y) -> do v $= x; w $= y)

textureEnvOperand :: TextureEnvParameter -> StateVar BlendingFactor
textureEnvOperand =
   texEnvi (unmarshalBlendingFactor . fromIntegral) (fromIntegral . marshalBlendingFactor) TextureEnv

textureEnvSrc :: TextureEnvParameter -> StateVar Src
textureEnvSrc = texEnvi unmarshalSrc marshalSrc TextureEnv

--------------------------------------------------------------------------------

rgbScale :: StateVar GLfloat
rgbScale = scale TexEnvParamRGBScale

alphaScale :: StateVar GLfloat
alphaScale = scale TexEnvParamAlphaScale

scale :: TextureEnvParameter -> StateVar GLfloat
scale = texEnvf id id TextureEnv

--------------------------------------------------------------------------------

constantColor :: StateVar (Color4 GLfloat)
constantColor = texEnvC4f TextureEnv TexEnvParamTextureEnvColor

--------------------------------------------------------------------------------

textureUnitLODBias :: StateVar LOD
textureUnitLODBias = texEnvf id id TextureFilterControl TexEnvParamLODBias
