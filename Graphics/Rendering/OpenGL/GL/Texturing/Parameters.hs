--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Parameters
-- Copyright   :  (c) Sven Panne 2002-2004
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
   textureLevelRange
) where

import Control.Monad ( liftM2 )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLint, GLfloat )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( TextureCoordName(..) )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetMaxTextureMaxAnisotropy,GetMaxTextureLODBias), getFloat1)
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.Texturing.Specification ( Level )
import Graphics.Rendering.OpenGL.GL.Texturing.TexParameter (
   TexParameter(TextureMinFilter,TextureMagFilter,TextureWrapS,TextureWrapT,
                TextureWrapR,TextureBorderColor,TextureMinLOD,TextureMaxLOD,
                TextureBaseLevel,TextureMaxLevel,TextureMaxAnisotropy,
                TextureLODBias),
   texParameteri, texParameterf, texParameterC4f,
   getTexParameteri, getTexParameterf, getTexParameterC4f )
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget (
   TextureTarget(..) )
import Graphics.Rendering.OpenGL.GL.VertexSpec( Color4(..) )
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal ( recordInvalidEnum )

--------------------------------------------------------------------------------

data TextureFilter =
     Nearest
   | Linear'
   deriving ( Eq, Ord, Show )

marshalMinificationFilter :: MinificationFilter -> GLint
marshalMinificationFilter x = case x of
   (Nearest, Nothing     ) -> 0x2600
   (Linear', Nothing     ) -> 0x2601
   (Nearest, Just Nearest) -> 0x2700
   (Linear', Just Nearest) -> 0x2701
   (Nearest, Just Linear') -> 0x2702
   (Linear', Just Linear') -> 0x2703

unmarshalMinificationFilter :: GLint -> MinificationFilter
unmarshalMinificationFilter x
   | x ==  0x2600 = (Nearest, Nothing     )
   | x ==  0x2601 = (Linear', Nothing     )
   | x ==  0x2700 = (Nearest, Just Nearest)
   | x ==  0x2701 = (Linear', Just Nearest)
   | x ==  0x2702 = (Nearest, Just Linear')
   | x ==  0x2703 = (Linear', Just Linear')
   | otherwise = error ("unmarshalMinificationFilter: illegal value " ++ show x)

--------------------------------------------------------------------------------

type MinificationFilter = (TextureFilter, Maybe TextureFilter)

type MagnificationFilter = TextureFilter

-- We treat MagnificationFilter as a degenerated case of MinificationFilter
magToMin :: MagnificationFilter -> MinificationFilter
magToMin magFilter = (magFilter, Nothing)

minToMag :: MinificationFilter -> MagnificationFilter
minToMag (magFilter, Nothing) = magFilter
minToMag minFilter = error ("minToMag: illegal value " ++ show minFilter)

--------------------------------------------------------------------------------

-- ToDo: cube maps
textureFilter :: TextureTarget -> StateVar (MinificationFilter, MagnificationFilter)
textureFilter t =
   makeStateVar
      (do minFilter      <- getTexParameteri unmarshalMinificationFilter t TextureMinFilter
          magFilterAsMin <- getTexParameteri unmarshalMinificationFilter t TextureMagFilter
          return (minFilter, minToMag magFilterAsMin))
      (\(minFilter, magFilter) -> do
         texParameteri marshalMinificationFilter t TextureMinFilter minFilter
         texParameteri marshalMinificationFilter t TextureMagFilter (magToMin magFilter))

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

-- ToDo: cube maps
textureWrapMode :: TextureTarget -> TextureCoordName -> StateVar (Repetition, Clamping)
textureWrapMode t coord =
   let mp = textureCoordNameToTexParameter coord
   in makeStateVar
      (maybe (do recordInvalidEnum; return (Repeated, Repeat)) (getTexParameteri unmarshalTextureWrapMode t) mp)
      (\m -> maybe recordInvalidEnum (\p -> texParameteri marshalTextureWrapMode t p m) mp)

textureCoordNameToTexParameter :: TextureCoordName -> Maybe TexParameter
textureCoordNameToTexParameter x = case x of
   S -> Just TextureWrapS
   T -> Just TextureWrapT
   R -> Just TextureWrapR
   Q -> Nothing

--------------------------------------------------------------------------------

-- ToDo: cube maps
textureBorderColor :: TextureTarget -> StateVar (Color4 GLfloat)
textureBorderColor t =
   makeStateVar
       (getTexParameterC4f t TextureBorderColor)
       (texParameterC4f t TextureBorderColor)

--------------------------------------------------------------------------------

type LOD = GLfloat

-- ToDo: cube maps
textureObjectLODBias :: TextureTarget -> StateVar LOD
textureObjectLODBias t =
   makeStateVar
      (getTexParameterf t TextureLODBias)
      (texParameterf t TextureLODBias)

maxTextureLODBias :: GettableStateVar LOD
maxTextureLODBias =
   makeGettableStateVar (getFloat1 id GetMaxTextureLODBias)

-- ToDo: cube maps
textureLODRange :: TextureTarget -> StateVar (LOD,LOD)
textureLODRange t =
   makeStateVar
       (liftM2 (,) (getTexParameterf t TextureMinLOD)
                   (getTexParameterf t TextureMaxLOD))
       (\(minLOD,maxLOD) -> do
          texParameterf t TextureMinLOD minLOD
          texParameterf t TextureMaxLOD maxLOD)

--------------------------------------------------------------------------------

-- ToDo: cube maps
textureMaxAnisotropy :: TextureTarget -> StateVar GLfloat
textureMaxAnisotropy t =
   makeStateVar
      (getTexParameterf t TextureMaxAnisotropy)
      (texParameterf t TextureMaxAnisotropy)

maxTextureMaxAnisotropy :: GettableStateVar GLfloat
maxTextureMaxAnisotropy =
   makeGettableStateVar (getFloat1 id GetMaxTextureMaxAnisotropy)

--------------------------------------------------------------------------------

-- ToDo: cube maps
textureLevelRange :: TextureTarget -> StateVar (Level,Level)
textureLevelRange t =
   makeStateVar
       (liftM2 (,) (getTexParameteri id t TextureBaseLevel)
                   (getTexParameteri id t TextureMaxLevel))
       (\(baseLevel,maxLevel) -> do
          texParameteri id t TextureBaseLevel baseLevel
          texParameteri id t TextureMaxLevel  maxLevel)
