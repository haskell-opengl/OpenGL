--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Parameters
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
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

import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.ComparisonFunction
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat
import Graphics.Rendering.OpenGL.GL.Texturing.Specification
import Graphics.Rendering.OpenGL.GL.Texturing.TexParameter
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility ( gl_CLAMP )
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.EXT.TextureMirrorClamp (
   gl_MIRROR_CLAMP, gl_MIRROR_CLAMP_TO_BORDER, gl_MIRROR_CLAMP_TO_EDGE )

--------------------------------------------------------------------------------

data TextureFilter =
     Nearest
   | Linear'
   deriving ( Eq, Ord, Show )

type MinificationFilter = (TextureFilter, Maybe TextureFilter)

type MagnificationFilter = TextureFilter

-- We treat MagnificationFilter as a degenerated case of MinificationFilter
magToMin :: MagnificationFilter -> MinificationFilter
magToMin magFilter = (magFilter, Nothing)

minToMag :: MinificationFilter -> MagnificationFilter
minToMag (magFilter, Nothing) = magFilter
minToMag minFilter = error ("minToMag: illegal value " ++ show minFilter)

marshalMinificationFilter :: MinificationFilter -> GLint
marshalMinificationFilter x = fromIntegral $ case x of
   (Nearest, Nothing     ) -> gl_NEAREST
   (Linear', Nothing     ) -> gl_LINEAR
   (Nearest, Just Nearest) -> gl_NEAREST_MIPMAP_NEAREST
   (Linear', Just Nearest) -> gl_LINEAR_MIPMAP_NEAREST
   (Nearest, Just Linear') -> gl_NEAREST_MIPMAP_LINEAR
   (Linear', Just Linear') -> gl_LINEAR_MIPMAP_LINEAR

marshalMagnificationFilter :: MagnificationFilter -> GLint
marshalMagnificationFilter = marshalMinificationFilter . magToMin

unmarshalMinificationFilter :: GLint -> MinificationFilter
unmarshalMinificationFilter x
   | y ==  gl_NEAREST = (Nearest, Nothing)
   | y ==  gl_LINEAR = (Linear', Nothing)
   | y ==  gl_NEAREST_MIPMAP_NEAREST = (Nearest, Just Nearest)
   | y ==  gl_LINEAR_MIPMAP_NEAREST = (Linear', Just Nearest)
   | y ==  gl_NEAREST_MIPMAP_LINEAR = (Nearest, Just Linear')
   | y ==  gl_LINEAR_MIPMAP_LINEAR = (Linear', Just Linear')
   | otherwise = error ("unmarshalMinificationFilter: illegal value " ++ show x)
   where y = fromIntegral x

unmarshalMagnificationFilter :: GLint -> MagnificationFilter
unmarshalMagnificationFilter = minToMag . unmarshalMinificationFilter

--------------------------------------------------------------------------------

textureFilter :: TextureTarget -> StateVar (MinificationFilter, MagnificationFilter)
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
   (Mirrored, Clamp) -> gl_MIRROR_CLAMP
   (Mirrored, Repeat) -> gl_MIRRORED_REPEAT
   (Mirrored, ClampToEdge) -> gl_MIRROR_CLAMP_TO_EDGE
   (Mirrored, ClampToBorder) -> gl_MIRROR_CLAMP_TO_BORDER

unmarshalTextureWrapMode :: GLint -> (Repetition, Clamping)
unmarshalTextureWrapMode x
   | y == gl_CLAMP = (Repeated, Clamp)
   | y == gl_REPEAT = (Repeated, Repeat)
   | y == gl_CLAMP_TO_EDGE = (Repeated, ClampToEdge)
   | y == gl_CLAMP_TO_BORDER = (Repeated, ClampToBorder)
   | y == gl_MIRROR_CLAMP = (Mirrored, Clamp)
   | y == gl_MIRRORED_REPEAT = (Mirrored, Repeat)
   | y == gl_MIRROR_CLAMP_TO_EDGE = (Mirrored, ClampToEdge)
   | y == gl_MIRROR_CLAMP_TO_BORDER = (Mirrored, ClampToBorder)
   | otherwise = error ("unmarshalTextureWrapMode: illegal value " ++ show x)
   where y = fromIntegral x

--------------------------------------------------------------------------------

textureWrapMode :: TextureTarget -> TextureCoordName -> StateVar (Repetition,Clamping)
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

textureBorderColor :: TextureTarget -> StateVar (Color4 GLfloat)
textureBorderColor = texParamC4f TextureBorderColor

--------------------------------------------------------------------------------

type LOD = GLfloat

textureObjectLODBias :: TextureTarget -> StateVar LOD
textureObjectLODBias = texParamf id id TextureLODBias

maxTextureLODBias :: GettableStateVar LOD
maxTextureLODBias =
   makeGettableStateVar (getFloat1 id GetMaxTextureLODBias)

textureLODRange :: TextureTarget -> StateVar (LOD,LOD)
textureLODRange =
   combineTexParams
      (texParamf id id TextureMinLOD)
      (texParamf id id TextureMaxLOD)

--------------------------------------------------------------------------------

textureMaxAnisotropy :: TextureTarget -> StateVar GLfloat
textureMaxAnisotropy = texParamf id id TextureMaxAnisotropy

maxTextureMaxAnisotropy :: GettableStateVar GLfloat
maxTextureMaxAnisotropy =
   makeGettableStateVar (getFloat1 id GetMaxTextureMaxAnisotropy)

--------------------------------------------------------------------------------

textureLevelRange :: TextureTarget -> StateVar (Level,Level)
textureLevelRange =
   combineTexParams
      (texParami id id TextureBaseLevel)
      (texParami id id TextureMaxLevel)

--------------------------------------------------------------------------------

generateMipmap :: TextureTarget -> StateVar Capability
generateMipmap = texParami unmarshal marshal GenerateMipmap
   where unmarshal = unmarshalCapability . fromIntegral
         marshal = fromIntegral . marshalCapability

--------------------------------------------------------------------------------

-- Only Luminance', Intensity, and Alpha' allowed
depthTextureMode :: TextureTarget -> StateVar PixelInternalFormat
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

textureCompareMode :: TextureTarget -> StateVar (Maybe ComparisonFunction)
textureCompareMode =
   combineTexParamsMaybe
      (texParami unmarshalTextureCompareMode marshalTextureCompareMode TextureCompareMode)
      (texParami unmarshal marshal TextureCompareFunc)
   where unmarshal = unmarshalComparisonFunction . fromIntegral
         marshal = fromIntegral . marshalComparisonFunction

--------------------------------------------------------------------------------

textureCompareFailValue :: TextureTarget -> StateVar GLclampf
textureCompareFailValue = texParamf realToFrac realToFrac TextureCompareFailValue

--------------------------------------------------------------------------------

data TextureCompareOperator =
     LequalR
   | GequalR
   deriving ( Eq, Ord, Show )

marshalTextureCompareOperator :: TextureCompareOperator -> GLint
marshalTextureCompareOperator x = case x of
   LequalR -> 0x819c
   GequalR -> 0x819d

unmarshalTextureCompareOperator :: GLint -> TextureCompareOperator
unmarshalTextureCompareOperator x
   | x == 0x819c = LequalR
   | x == 0x819d = GequalR
   | otherwise = error ("unmarshalTextureCompareOperator: illegal value " ++ show x)

--------------------------------------------------------------------------------

textureCompareOperator :: TextureTarget -> StateVar (Maybe TextureCompareOperator)
textureCompareOperator =
   combineTexParamsMaybe
      (texParami (unmarshalCapability . fromIntegral) (fromIntegral. marshalCapability) TextureCompare)
      (texParami unmarshalTextureCompareOperator marshalTextureCompareOperator TextureCompareOperator)
