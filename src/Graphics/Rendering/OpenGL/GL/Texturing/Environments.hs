--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Environments
-- Copyright   :  (c) Sven Panne 2002-2019
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
import Graphics.GL

--------------------------------------------------------------------------------

data TextureEnvTarget =
     TextureEnv
   | TextureFilterControl   -- GL_TEXTURE_LOD_BIAS_EXT
   | PointSprite            -- GL_COORD_REPLACE_NV

marshalTextureEnvTarget :: TextureEnvTarget -> GLenum
marshalTextureEnvTarget x = case x of
   TextureEnv -> GL_TEXTURE_ENV
   TextureFilterControl -> GL_TEXTURE_FILTER_CONTROL
   PointSprite -> GL_POINT_SPRITE

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
   TexEnvParamTextureEnvMode -> GL_TEXTURE_ENV_MODE
   TexEnvParamTextureEnvColor -> GL_TEXTURE_ENV_COLOR
   TexEnvParamCombineRGB -> GL_COMBINE_RGB
   TexEnvParamCombineAlpha -> GL_COMBINE_ALPHA
   TexEnvParamSrc0RGB -> GL_SRC0_RGB
   TexEnvParamSrc1RGB -> GL_SRC1_RGB
   TexEnvParamSrc2RGB -> GL_SRC2_RGB
   TexEnvParamSrc3RGB -> GL_SOURCE3_RGB_NV
   TexEnvParamSrc0Alpha -> GL_SRC0_ALPHA
   TexEnvParamSrc1Alpha -> GL_SRC1_ALPHA
   TexEnvParamSrc2Alpha -> GL_SRC2_ALPHA
   TexEnvParamSrc3Alpha -> GL_SOURCE3_ALPHA_NV
   TexEnvParamOperand0RGB -> GL_OPERAND0_RGB
   TexEnvParamOperand1RGB -> GL_OPERAND1_RGB
   TexEnvParamOperand2RGB -> GL_OPERAND2_RGB
   TexEnvParamOperand3RGB -> GL_OPERAND3_RGB_NV
   TexEnvParamOperand0Alpha -> GL_OPERAND0_ALPHA
   TexEnvParamOperand1Alpha -> GL_OPERAND1_ALPHA
   TexEnvParamOperand2Alpha -> GL_OPERAND2_ALPHA
   TexEnvParamOperand3Alpha -> GL_OPERAND3_ALPHA_NV
   TexEnvParamRGBScale -> GL_RGB_SCALE
   TexEnvParamAlphaScale -> GL_ALPHA_SCALE
   TexEnvParamLODBias -> GL_TEXTURE_LOD_BIAS

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
   Modulate -> GL_MODULATE
   Decal -> GL_DECAL
   Blend -> GL_BLEND
   Replace -> GL_REPLACE
   AddUnsigned -> GL_ADD
   Combine -> GL_COMBINE
   Combine4 -> GL_COMBINE4_NV

unmarshalTextureFunction :: GLint -> TextureFunction
unmarshalTextureFunction x
   | y == GL_MODULATE = Modulate
   | y == GL_DECAL = Decal
   | y == GL_BLEND = Blend
   | y == GL_REPLACE = Replace
   | y == GL_ADD = AddUnsigned
   | y == GL_COMBINE = Combine
   | y == GL_COMBINE4_NV = Combine4
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
   Replace' -> GL_REPLACE
   Modulate' -> GL_MODULATE
   AddUnsigned' -> GL_ADD
   AddSigned -> GL_ADD_SIGNED
   Interpolate -> GL_INTERPOLATE
   Subtract -> GL_SUBTRACT
   Dot3RGB -> GL_DOT3_RGB
   Dot3RGBA -> GL_DOT3_RGBA

unmarshalTextureCombineFunction :: GLint -> TextureCombineFunction
unmarshalTextureCombineFunction x
   | y == GL_REPLACE = Replace'
   | y == GL_MODULATE = Modulate'
   | y == GL_ADD = AddUnsigned'
   | y == GL_ADD_SIGNED = AddSigned
   | y == GL_INTERPOLATE = Interpolate
   | y == GL_SUBTRACT = Subtract
   | y == GL_DOT3_RGB = Dot3RGB
   | y == GL_DOT3_RGBA = Dot3RGBA
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
   CurrentUnit -> GL_TEXTURE
   Previous -> GL_PREVIOUS
   Crossbar u -> fromIntegral (marshalTextureUnit u)
   Constant -> GL_CONSTANT
   PrimaryColor -> GL_PRIMARY_COLOR

unmarshalSrc :: GLint -> Src
unmarshalSrc x
   | y == GL_TEXTURE = CurrentUnit
   | y == GL_PREVIOUS = Previous
   | y == GL_CONSTANT = Constant
   | y == GL_PRIMARY_COLOR = PrimaryColor
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
