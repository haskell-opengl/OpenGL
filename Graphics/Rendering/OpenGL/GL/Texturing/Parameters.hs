--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Parameters
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 3.8.4 (Texture Parameters), section 3.8.7
-- (Texture Wrap Mode), section 3.8.8 (Texture Minification), and section 3.8.9
-- (Texture Magnification) of the OpenGL 1.5 specs.
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

import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLint, GLfloat, GLclampf, Capability(..) )
import Graphics.Rendering.OpenGL.GL.Capability (
   marshalCapability, unmarshalCapability )
import Graphics.Rendering.OpenGL.GL.ComparisonFunction (
   marshalComparisonFunction, unmarshalComparisonFunction )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( TextureCoordName(..) )
import Graphics.Rendering.OpenGL.GL.PerFragment ( ComparisonFunction )
import Graphics.Rendering.OpenGL.GL.PixelRectangles (
   PixelInternalFormat(..) )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetMaxTextureMaxAnisotropy,GetMaxTextureLODBias), getFloat1)
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar,
   StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.Texturing.Specification (
   Level, TextureTarget(..) )
import Graphics.Rendering.OpenGL.GL.Texturing.TexParameter (
   TexParameter(TextureMinFilter,TextureMagFilter,TextureWrapS,TextureWrapT,
                TextureWrapR,TextureBorderColor,TextureMinLOD,TextureMaxLOD,
                TextureBaseLevel,TextureMaxLevel,TextureMaxAnisotropy,
                TextureLODBias,GenerateMipmap,DepthTextureMode,
                TextureCompareMode,TextureCompareFunc,TextureCompareFailValue,
                TextureCompare,TextureCompareOperator),
   texParami, texParamf, texParamC4f, combineTexParams, combineTexParamsMaybe )
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat (
   marshalPixelInternalFormat, unmarshalPixelInternalFormat )
import Graphics.Rendering.OpenGL.GL.VertexSpec( Color4(..) )
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal ( recordInvalidEnum )

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
marshalMinificationFilter x = case x of
   (Nearest, Nothing     ) -> 0x2600
   (Linear', Nothing     ) -> 0x2601
   (Nearest, Just Nearest) -> 0x2700
   (Linear', Just Nearest) -> 0x2701
   (Nearest, Just Linear') -> 0x2702
   (Linear', Just Linear') -> 0x2703

marshalMagnificationFilter :: MagnificationFilter -> GLint
marshalMagnificationFilter = marshalMinificationFilter . magToMin

unmarshalMinificationFilter :: GLint -> MinificationFilter
unmarshalMinificationFilter x
   | x ==  0x2600 = (Nearest, Nothing     )
   | x ==  0x2601 = (Linear', Nothing     )
   | x ==  0x2700 = (Nearest, Just Nearest)
   | x ==  0x2701 = (Linear', Just Nearest)
   | x ==  0x2702 = (Nearest, Just Linear')
   | x ==  0x2703 = (Linear', Just Linear')
   | otherwise = error ("unmarshalMinificationFilter: illegal value " ++ show x)

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
marshalTextureWrapMode x = case x of
   (Repeated, Clamp) -> 0x2900
   (Repeated, Repeat) -> 0x2901
   (Repeated, ClampToEdge) -> 0x812f
   (Repeated, ClampToBorder) -> 0x812d
   (Mirrored, Clamp) -> 0x8742
   (Mirrored, Repeat) -> 0x8370
   (Mirrored, ClampToEdge) -> 0x8743
   (Mirrored, ClampToBorder) -> 0x8912

unmarshalTextureWrapMode :: GLint -> (Repetition, Clamping)
unmarshalTextureWrapMode x
   | x == 0x2900 = (Repeated, Clamp)
   | x == 0x2901 = (Repeated, Repeat)
   | x == 0x812f = (Repeated, ClampToEdge)
   | x == 0x812d = (Repeated, ClampToBorder)
   | x == 0x8742 = (Mirrored, Clamp)
   | x == 0x8370 = (Mirrored, Repeat)
   | x == 0x8743 = (Mirrored, ClampToEdge)
   | x == 0x8912 = (Mirrored, ClampToBorder)
   | otherwise = error ("unmarshalTextureWrapMode: illegal value " ++ show x)

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
marshalTextureCompareMode x = case x of
   Disabled -> 0x0     -- i.e. None
   Enabled -> 0x884e   -- i.e. CompareRToTexture

unmarshalTextureCompareMode :: GLint -> Capability
unmarshalTextureCompareMode x
   | x == 0x0 = Disabled
   | x == 0x884e = Enabled
   | otherwise = error ("unmarshalTextureCompareMode: illegal value " ++ show x)

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
textureCompareFailValue = texParamf id id TextureCompareFailValue

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
