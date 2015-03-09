--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Parameters
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 3.8.4 (Texture Parameters), section 3.8.7
-- (Texture Wrap Mode), section 3.8.8 (Texture Minification), and section 3.8.9
-- (Texture Magnification) of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing.Parameters (
   TextureFilter(..), MinificationFilter, MagnificationFilter, textureFilter,
   Repetition(..), Clamping(..), textureWrapMode,
   textureBorderColor, LOD, textureObjectLODBias, maxTextureLODBias,
   textureLODRange, textureMaxAnisotropy, maxTextureMaxAnisotropy,
   textureLevelRange, generateMipmap, depthTextureMode, textureCompareMode,
   textureCompareFailValue, TextureCompareOperator(..), textureCompareOperator
) where

import Control.Monad
import Data.StateVar
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.ComparisonFunction
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.Texturing.Filter
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat
import Graphics.Rendering.OpenGL.GL.Texturing.Specification
import Graphics.Rendering.OpenGL.GL.Texturing.TexParameter
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

textureFilter :: ParameterizedTextureTarget t => t -> StateVar (MinificationFilter, MagnificationFilter)
textureFilter =
   combineTexParams
      (texParami unmarshalMinificationFilter  marshalMinificationFilter  TextureMinFilter)
      (texParami unmarshalMagnificationFilter marshalMagnificationFilter TextureMagFilter)

--------------------------------------------------------------------------------

data Repetition =
     Repeated
   | Mirrored
   deriving ( Eq, Ord, Show )

data Clamping =
     Clamp
   | Repeat
   | ClampToEdge
   | ClampToBorder
   deriving ( Eq, Ord, Show )

marshalTextureWrapMode :: (Repetition, Clamping) -> GLint
marshalTextureWrapMode x = fromIntegral $ case x of
   (Repeated, Clamp) -> gl_CLAMP
   (Repeated, Repeat) -> gl_REPEAT
   (Repeated, ClampToEdge) -> gl_CLAMP_TO_EDGE
   (Repeated, ClampToBorder) -> gl_CLAMP_TO_BORDER
   (Mirrored, Clamp) -> gl_MIRROR_CLAMP_EXT
   (Mirrored, Repeat) -> gl_MIRRORED_REPEAT
   (Mirrored, ClampToEdge) -> gl_MIRROR_CLAMP_TO_EDGE
   (Mirrored, ClampToBorder) -> gl_MIRROR_CLAMP_TO_BORDER_EXT

unmarshalTextureWrapMode :: GLint -> (Repetition, Clamping)
unmarshalTextureWrapMode x
   | y == gl_CLAMP = (Repeated, Clamp)
   | y == gl_REPEAT = (Repeated, Repeat)
   | y == gl_CLAMP_TO_EDGE = (Repeated, ClampToEdge)
   | y == gl_CLAMP_TO_BORDER = (Repeated, ClampToBorder)
   | y == gl_MIRROR_CLAMP_EXT = (Mirrored, Clamp)
   | y == gl_MIRRORED_REPEAT = (Mirrored, Repeat)
   | y == gl_MIRROR_CLAMP_TO_EDGE = (Mirrored, ClampToEdge)
   | y == gl_MIRROR_CLAMP_TO_BORDER_EXT = (Mirrored, ClampToBorder)
   | otherwise = error ("unmarshalTextureWrapMode: illegal value " ++ show x)
   where y = fromIntegral x

--------------------------------------------------------------------------------

textureWrapMode :: ParameterizedTextureTarget t => t -> TextureCoordName -> StateVar (Repetition,Clamping)
textureWrapMode t coord = case coord of
   S -> wrap TextureWrapS
   T -> wrap TextureWrapT
   R -> wrap TextureWrapR
   Q -> invalidTextureCoord
   where wrap c = texParami unmarshalTextureWrapMode marshalTextureWrapMode c t

invalidTextureCoord :: StateVar (Repetition,Clamping)
invalidTextureCoord =
   makeStateVar
      (do recordInvalidEnum; return (Repeated, Repeat))
      (const recordInvalidEnum)

--------------------------------------------------------------------------------

textureBorderColor :: ParameterizedTextureTarget t => t -> StateVar (Color4 GLfloat)
textureBorderColor = texParamC4f TextureBorderColor

--------------------------------------------------------------------------------

type LOD = GLfloat

textureObjectLODBias :: ParameterizedTextureTarget t => t -> StateVar LOD
textureObjectLODBias = texParamf id id TextureLODBias

maxTextureLODBias :: GettableStateVar LOD
maxTextureLODBias =
   makeGettableStateVar (getFloat1 id GetMaxTextureLODBias)

textureLODRange :: ParameterizedTextureTarget t => t -> StateVar (LOD,LOD)
textureLODRange =
   combineTexParams
      (texParamf id id TextureMinLOD)
      (texParamf id id TextureMaxLOD)

--------------------------------------------------------------------------------

textureMaxAnisotropy :: ParameterizedTextureTarget t => t -> StateVar GLfloat
textureMaxAnisotropy = texParamf id id TextureMaxAnisotropy

maxTextureMaxAnisotropy :: GettableStateVar GLfloat
maxTextureMaxAnisotropy =
   makeGettableStateVar (getFloat1 id GetMaxTextureMaxAnisotropy)

--------------------------------------------------------------------------------

textureLevelRange :: ParameterizedTextureTarget t => t -> StateVar (Level,Level)
textureLevelRange =
   combineTexParams
      (texParami id id TextureBaseLevel)
      (texParami id id TextureMaxLevel)

--------------------------------------------------------------------------------

-- | Note: OpenGL 3.1 deprecated this texture parameter, use
-- 'Graphics.Rendering.OpenGL.GL.Texturing.Objects.generateMipmap'' instead.

generateMipmap :: ParameterizedTextureTarget t => t -> StateVar Capability
generateMipmap = texParami unmarshal marshal GenerateMipmap
   where unmarshal = unmarshalCapability . fromIntegral
         marshal = fromIntegral . marshalCapability

--------------------------------------------------------------------------------

-- Only Luminance', Intensity, and Alpha' allowed
depthTextureMode :: ParameterizedTextureTarget t => t -> StateVar PixelInternalFormat
depthTextureMode =
   texParami unmarshalPixelInternalFormat marshalPixelInternalFormat DepthTextureMode

--------------------------------------------------------------------------------

marshalTextureCompareMode :: Capability -> GLint
marshalTextureCompareMode x = fromIntegral $ case x of
   Disabled -> gl_NONE
   Enabled -> gl_COMPARE_REF_TO_TEXTURE

unmarshalTextureCompareMode :: GLint -> Capability
unmarshalTextureCompareMode x
   | y == gl_NONE = Disabled
   | y == gl_COMPARE_REF_TO_TEXTURE = Enabled
   | otherwise = error ("unmarshalTextureCompareMode: illegal value " ++ show x)
   where y = fromIntegral x

--------------------------------------------------------------------------------

textureCompareMode :: ParameterizedTextureTarget t => t -> StateVar (Maybe ComparisonFunction)
textureCompareMode =
   combineTexParamsMaybe
      (texParami unmarshalTextureCompareMode marshalTextureCompareMode TextureCompareMode)
      (texParami unmarshal marshal TextureCompareFunc)
   where unmarshal = unmarshalComparisonFunction . fromIntegral
         marshal = fromIntegral . marshalComparisonFunction

--------------------------------------------------------------------------------

textureCompareFailValue :: ParameterizedTextureTarget t => t -> StateVar GLclampf
textureCompareFailValue = texParamf realToFrac realToFrac TextureCompareFailValue

--------------------------------------------------------------------------------

data TextureCompareOperator =
     LequalR
   | GequalR
   deriving ( Eq, Ord, Show )

marshalTextureCompareOperator :: TextureCompareOperator -> GLenum
marshalTextureCompareOperator x = case x of
   LequalR -> gl_TEXTURE_LEQUAL_R_SGIX
   GequalR -> gl_TEXTURE_GEQUAL_R_SGIX

unmarshalTextureCompareOperator :: GLenum -> TextureCompareOperator
unmarshalTextureCompareOperator x
   | x == gl_TEXTURE_LEQUAL_R_SGIX = LequalR
   | x == gl_TEXTURE_GEQUAL_R_SGIX = GequalR
   | otherwise = error ("unmarshalTextureCompareOperator: illegal value " ++ show x)

--------------------------------------------------------------------------------

textureCompareOperator :: ParameterizedTextureTarget t => t -> StateVar (Maybe TextureCompareOperator)
textureCompareOperator =
   combineTexParamsMaybe
      (texParami (unmarshalCapability . fromIntegral) (fromIntegral. marshalCapability) TextureCompare)
      (texParami (unmarshalTextureCompareOperator . fromIntegral) (fromIntegral . marshalTextureCompareOperator) TextureCompareOperator)

--------------------------------------------------------------------------------

combineTexParams :: ParameterizedTextureTarget t
                 => (t -> StateVar a)
                 -> (t -> StateVar b)
                 -> (t -> StateVar (a,b))
combineTexParams v w t =
   makeStateVar
      (liftM2 (,) (get (v t)) (get (w t)))
      (\(x,y) -> do v t $= x; w t $= y)

combineTexParamsMaybe :: ParameterizedTextureTarget t
                      => (t -> StateVar Capability)
                      -> (t -> StateVar a)
                      -> (t -> StateVar (Maybe a))
combineTexParamsMaybe enab val t =
   makeStateVar
      (do tcm <- get (enab t)
          case tcm of
             Disabled -> return Nothing
             Enabled -> fmap Just $ get (val t))
      (maybe (enab t $= Disabled)
             (\tcf -> do val t $= tcf
                         enab t $= Enabled))
