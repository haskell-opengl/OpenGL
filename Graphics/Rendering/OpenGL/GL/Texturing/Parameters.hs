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
   textureBorderColor, textureLODRange, textureLevelRange,
   textureMaxAnisotropy, maxTextureMaxAnisotropy
) where

import Control.Monad ( liftM2 )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLint, GLfloat )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( TextureCoordName(..) )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetMaxTextureMaxAnisotropy), getFloat1)
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.Texturing.Specification ( Level )
import Graphics.Rendering.OpenGL.GL.Texturing.TexParameter (
   TexParameter(TextureMinFilter,TextureMagFilter,TextureWrapS,TextureWrapT,
                TextureWrapR,TextureBorderColor,TextureMinLOD,TextureMaxLOD,
                TextureBaseLevel,TextureMaxLevel,TextureMaxAnisotropy),
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

marshalTextureFilter :: (TextureFilter, Maybe TextureFilter) -> GLint
marshalTextureFilter x = case x of
   (Nearest, Nothing     ) -> 0x2600
   (Linear', Nothing     ) -> 0x2601
   (Nearest, Just Nearest) -> 0x2700
   (Linear', Just Nearest) -> 0x2701
   (Nearest, Just Linear') -> 0x2702
   (Linear', Just Linear') -> 0x2703

unmarshalTextureFilter :: GLint -> (TextureFilter, Maybe TextureFilter)
unmarshalTextureFilter x
   | x ==  0x2600 = (Nearest, Nothing     )
   | x ==  0x2601 = (Linear', Nothing     )
   | x ==  0x2700 = (Nearest, Just Nearest)
   | x ==  0x2701 = (Linear', Just Nearest)
   | x ==  0x2702 = (Nearest, Just Linear')
   | x ==  0x2703 = (Linear', Just Linear')
   | otherwise = error ("unmarshalTextureFilter: illegal value " ++ show x)

--------------------------------------------------------------------------------

type MinificationFilter = (TextureFilter, Maybe TextureFilter)

type MagnificationFilter = TextureFilter

-- ToDo: cube maps
textureFilter :: TextureTarget -> StateVar (MinificationFilter, MagnificationFilter)
textureFilter t =
   makeStateVar
      (do minFilter      <- getTexParameteri unmarshalTextureFilter t TextureMinFilter
          (magFilter, _) <- getTexParameteri unmarshalTextureFilter t TextureMagFilter
          return (minFilter, magFilter))
      (\(minFilter, magFilter) -> do
         texParameteri marshalTextureFilter t TextureMinFilter minFilter
         texParameteri marshalTextureFilter t TextureMagFilter (magFilter, Nothing))

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

-- ToDo: cube maps
textureLODRange :: TextureTarget -> StateVar (GLfloat,GLfloat)
textureLODRange t =
   makeStateVar
       (liftM2 (,) (getTexParameterf t TextureMinLOD)
                   (getTexParameterf t TextureMaxLOD))
       (\(minLOD,maxLOD) -> do
          texParameterf t TextureMinLOD minLOD
          texParameterf t TextureMaxLOD maxLOD)

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
