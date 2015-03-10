--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Environments
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 3.8.13 (Texture Environments and Texture
-- Functions) of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing.Environments (
   TextureFunction(..), textureFunction,
   TextureCombineFunction(..), combineRGB, combineAlpha,
   ArgNum(..), Arg(..), Src(..), argRGB, argAlpha,
   rgbScale, alphaScale,
   constantColor, textureUnitLODBias
) where

import Control.Monad
import Data.StateVar
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.BlendingFactor
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.Texturing.Parameters
import Graphics.Rendering.OpenGL.GL.Texturing.TextureUnit
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

data TextureEnvTarget =
     TextureEnv
   | TextureFilterControl   -- GL_TEXTURE_LOD_BIAS_EXT
   | PointSprite            -- GL_COORD_REPLACE_NV

marshalTextureEnvTarget :: TextureEnvTarget -> GLenum
marshalTextureEnvTarget x = case x of
   TextureEnv -> gl_TEXTURE_ENV
   TextureFilterControl -> gl_TEXTURE_FILTER_CONTROL
   PointSprite -> gl_POINT_SPRITE

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
   TexEnvParamTextureEnvMode -> gl_TEXTURE_ENV_MODE
   TexEnvParamTextureEnvColor -> gl_TEXTURE_ENV_COLOR
   TexEnvParamCombineRGB -> gl_COMBINE_RGB
   TexEnvParamCombineAlpha -> gl_COMBINE_ALPHA
   TexEnvParamSrc0RGB -> gl_SRC0_RGB
   TexEnvParamSrc1RGB -> gl_SRC1_RGB
   TexEnvParamSrc2RGB -> gl_SRC2_RGB
   TexEnvParamSrc3RGB -> gl_SOURCE3_RGB_NV
   TexEnvParamSrc0Alpha -> gl_SRC0_ALPHA
   TexEnvParamSrc1Alpha -> gl_SRC1_ALPHA
   TexEnvParamSrc2Alpha -> gl_SRC2_ALPHA
   TexEnvParamSrc3Alpha -> gl_SOURCE3_ALPHA_NV
   TexEnvParamOperand0RGB -> gl_OPERAND0_RGB
   TexEnvParamOperand1RGB -> gl_OPERAND1_RGB
   TexEnvParamOperand2RGB -> gl_OPERAND2_RGB
   TexEnvParamOperand3RGB -> gl_OPERAND3_RGB_NV
   TexEnvParamOperand0Alpha -> gl_OPERAND0_ALPHA
   TexEnvParamOperand1Alpha -> gl_OPERAND1_ALPHA
   TexEnvParamOperand2Alpha -> gl_OPERAND2_ALPHA
   TexEnvParamOperand3Alpha -> gl_OPERAND3_ALPHA_NV
   TexEnvParamRGBScale -> gl_RGB_SCALE
   TexEnvParamAlphaScale -> gl_ALPHA_SCALE
   TexEnvParamLODBias -> gl_TEXTURE_LOD_BIAS

--------------------------------------------------------------------------------

texEnv :: (GLenum -> GLenum -> b -> IO ())
       -> (a -> (b -> IO ()) -> IO ())
       -> TextureEnvTarget -> TextureEnvParameter -> a -> IO ()
texEnv glTexEnv marshalAct t p x =
   marshalAct x $
      glTexEnv (marshalTextureEnvTarget t) (marshalTextureEnvParameter p)

glTexEnvC4f :: GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()
glTexEnvC4f t p ptr = glTexEnvfv t p (castPtr ptr)


--------------------------------------------------------------------------------

getTexEnv :: Storable b
          => (GLenum -> GLenum -> Ptr b -> IO ())
          -> (b -> a)
          -> TextureEnvTarget -> TextureEnvParameter -> IO a
getTexEnv glGetTexEnv unmarshal t p =
   alloca $ \buf -> do
     glGetTexEnv (marshalTextureEnvTarget t) (marshalTextureEnvParameter p) buf
     peek1 unmarshal buf

glGetTexEnvC4f :: GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()
glGetTexEnvC4f t p ptr = glGetTexEnvfv t p (castPtr ptr)

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
marshalTextureFunction x = fromIntegral $ case x of
   Modulate -> gl_MODULATE
   Decal -> gl_DECAL
   Blend -> gl_BLEND
   Replace -> gl_REPLACE
   AddUnsigned -> gl_ADD
   Combine -> gl_COMBINE
   Combine4 -> gl_COMBINE4_NV

unmarshalTextureFunction :: GLint -> TextureFunction
unmarshalTextureFunction x
   | y == gl_MODULATE = Modulate
   | y == gl_DECAL = Decal
   | y == gl_BLEND = Blend
   | y == gl_REPLACE = Replace
   | y == gl_ADD = AddUnsigned
   | y == gl_COMBINE = Combine
   | y == gl_COMBINE4_NV = Combine4
   | otherwise = error ("unmarshalTextureFunction: illegal value " ++ show x)
   where y = fromIntegral x

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
marshalTextureCombineFunction x = fromIntegral $ case x of
   Replace' -> gl_REPLACE
   Modulate' -> gl_MODULATE
   AddUnsigned' -> gl_ADD
   AddSigned -> gl_ADD_SIGNED
   Interpolate -> gl_INTERPOLATE
   Subtract -> gl_SUBTRACT
   Dot3RGB -> gl_DOT3_RGB
   Dot3RGBA -> gl_DOT3_RGBA

unmarshalTextureCombineFunction :: GLint -> TextureCombineFunction
unmarshalTextureCombineFunction x
   | y == gl_REPLACE = Replace'
   | y == gl_MODULATE = Modulate'
   | y == gl_ADD = AddUnsigned'
   | y == gl_ADD_SIGNED = AddSigned
   | y == gl_INTERPOLATE = Interpolate
   | y == gl_SUBTRACT = Subtract
   | y == gl_DOT3_RGB = Dot3RGB
   | y == gl_DOT3_RGBA = Dot3RGBA
   | otherwise = error ("unmarshalTextureCombineFunction: illegal value " ++ show x)
   where y = fromIntegral x

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
marshalSrc x = fromIntegral $ case x of
   CurrentUnit -> gl_TEXTURE
   Previous -> gl_PREVIOUS
   Crossbar u -> fromIntegral (marshalTextureUnit u)
   Constant -> gl_CONSTANT
   PrimaryColor -> gl_PRIMARY_COLOR

unmarshalSrc :: GLint -> Src
unmarshalSrc x
   | y == gl_TEXTURE = CurrentUnit
   | y == gl_PREVIOUS = Previous
   | y == gl_CONSTANT = Constant
   | y == gl_PRIMARY_COLOR = PrimaryColor
   | otherwise = Crossbar (unmarshalTextureUnit (fromIntegral x))
   where y = fromIntegral x

--------------------------------------------------------------------------------

argRGB :: ArgNum -> StateVar Arg
argRGB n = arg (argNumToOperandRGB n) (argNumToSrcRGB n)

argAlpha :: ArgNum -> StateVar Arg
argAlpha n = arg (argNumToOperandAlpha n) (argNumToSrcAlpha n)

arg :: TextureEnvParameter -> TextureEnvParameter -> StateVar Arg
arg op src = combineArg (textureEnvOperand op) (textureEnvSrc src)
   where combineArg :: StateVar BlendingFactor -> StateVar Src -> StateVar Arg
         combineArg v w = makeStateVar
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
