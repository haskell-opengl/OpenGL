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
   textureLevelRange, generateMipmap, depthTextureMode, textureCompareMode,
   textureCompareFailValue, TextureCompareOperator(..), textureCompareOperator
) where

import Control.Monad ( liftM, liftM2 )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLint, GLfloat, GLclampf )
import Graphics.Rendering.OpenGL.GL.Capability (
   Capability, marshalCapability, unmarshalCapability )
import Graphics.Rendering.OpenGL.GL.ComparisonFunction ( ComparisonFunction,
   marshalComparisonFunction, unmarshalComparisonFunction )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( TextureCoordName(..) )
import Graphics.Rendering.OpenGL.GL.GLboolean (
   marshalGLboolean, unmarshalGLboolean )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetMaxTextureMaxAnisotropy,GetMaxTextureLODBias), getFloat1)
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasGetter(get), GettableStateVar, makeGettableStateVar,
   HasSetter(($=)), StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.Texturing.Specification ( Level )
import Graphics.Rendering.OpenGL.GL.Texturing.TexParameter (
   TexParameter(TextureMinFilter,TextureMagFilter,TextureWrapS,TextureWrapT,
                TextureWrapR,TextureBorderColor,TextureMinLOD,TextureMaxLOD,
                TextureBaseLevel,TextureMaxLevel,TextureMaxAnisotropy,
                TextureLODBias,GenerateMipmap,DepthTextureMode,
                TextureCompareMode,TextureCompareFunc,TextureCompareFailValue,
                TextureCompare,TextureCompareOperator),
   texParameteri, texParameterf, texParameterC4f,
   getTexParameteri, getTexParameterf, getTexParameterC4f )
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat (
   PixelInternalFormat, marshalPixelInternalFormat,
   unmarshalPixelInternalFormat )
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
textureFilter = combineStateVars textureMinFilter textureMagFilter

combineStateVars :: (TextureTarget -> StateVar a)
                 -> (TextureTarget -> StateVar b)
                 -> (TextureTarget -> StateVar (a,b))
combineStateVars v w t =
   makeStateVar
      (liftM2 (,) (get (v t)) (get (w t)))
      (\(x,y) -> do v t $= x; w t $= y)

textureMinFilter :: TextureTarget -> StateVar MinificationFilter
textureMinFilter t =
   makeStateVar
      (getTexParameteri unmarshalMinificationFilter  t TextureMinFilter)
      (texParameteri    marshalMinificationFilter    t TextureMinFilter)

textureMagFilter :: TextureTarget -> StateVar MagnificationFilter
textureMagFilter t =
   makeStateVar
      (getTexParameteri unmarshalMagnificationFilter t TextureMagFilter)
      (texParameteri    marshalMagnificationFilter   t TextureMagFilter)

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
textureWrapMode :: TextureTarget -> TextureCoordName -> StateVar (Repetition,Clamping)
textureWrapMode t coord = case coord of
   S -> textureWrapMode' t TextureWrapS
   T -> textureWrapMode' t TextureWrapT
   R -> textureWrapMode' t TextureWrapR
   Q -> invalidTextureCoord

invalidTextureCoord :: StateVar (Repetition,Clamping)
invalidTextureCoord =
   makeStateVar
      (do recordInvalidEnum; return (Repeated, Repeat))
      (const recordInvalidEnum)

textureWrapMode' :: TextureTarget -> TexParameter -> StateVar (Repetition,Clamping)
textureWrapMode' t p =
   makeStateVar
      (getTexParameteri unmarshalTextureWrapMode t p)
      (texParameteri    marshalTextureWrapMode   t p)

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
      (getTexParameterf id t TextureLODBias)
      (texParameterf    id t TextureLODBias)

maxTextureLODBias :: GettableStateVar LOD
maxTextureLODBias =
   makeGettableStateVar (getFloat1 id GetMaxTextureLODBias)

-- ToDo: cube maps
textureLODRange :: TextureTarget -> StateVar (LOD,LOD)
textureLODRange = combineStateVars textureMinLOD textureMaxLOD

textureMinLOD :: TextureTarget -> StateVar LOD
textureMinLOD t =
   makeStateVar
      (getTexParameterf id t TextureMinLOD)
      (texParameterf    id t TextureMinLOD)

textureMaxLOD :: TextureTarget -> StateVar LOD
textureMaxLOD t =
   makeStateVar
      (getTexParameterf id t TextureMaxLOD)
      (texParameterf    id t TextureMaxLOD)

--------------------------------------------------------------------------------

-- ToDo: cube maps
textureMaxAnisotropy :: TextureTarget -> StateVar GLfloat
textureMaxAnisotropy t =
   makeStateVar
      (getTexParameterf id t TextureMaxAnisotropy)
      (texParameterf    id t TextureMaxAnisotropy)

maxTextureMaxAnisotropy :: GettableStateVar GLfloat
maxTextureMaxAnisotropy =
   makeGettableStateVar (getFloat1 id GetMaxTextureMaxAnisotropy)

--------------------------------------------------------------------------------

-- ToDo: cube maps
textureLevelRange :: TextureTarget -> StateVar (Level,Level)
textureLevelRange = combineStateVars textureBaseLevel textureMaxLevel

textureBaseLevel :: TextureTarget -> StateVar Level
textureBaseLevel t =
   makeStateVar
      (getTexParameteri id t TextureBaseLevel)
      (texParameteri    id t TextureBaseLevel)

textureMaxLevel :: TextureTarget -> StateVar Level
textureMaxLevel t =
   makeStateVar
      (getTexParameteri id t TextureMaxLevel)
      (texParameteri    id t TextureMaxLevel)

--------------------------------------------------------------------------------

-- ToDo: cube maps
generateMipmap :: TextureTarget -> StateVar Capability
generateMipmap t =
   makeStateVar
      (getTexParameteri (unmarshalCapability . fromIntegral) t GenerateMipmap)
      (texParameteri    (fromIntegral . marshalCapability)   t GenerateMipmap)

--------------------------------------------------------------------------------

-- ToDo: cube maps
-- Only Luminance', Intensity, and Alpha' allowed
depthTextureMode :: TextureTarget -> StateVar PixelInternalFormat
depthTextureMode t =
   makeStateVar
      (getTexParameteri unmarshalPixelInternalFormat t DepthTextureMode)
      (texParameteri    marshalPixelInternalFormat   t DepthTextureMode)

--------------------------------------------------------------------------------

data TextureCompareMode =
     None
   | CompareRToTexture

marshalTextureCompareMode :: TextureCompareMode -> GLint
marshalTextureCompareMode x = case x of
   None -> 0x0
   CompareRToTexture -> 0x884e

unmarshalTextureCompareMode :: GLint -> TextureCompareMode
unmarshalTextureCompareMode x
   | x == 0x0 = None
   | x == 0x884e = CompareRToTexture
   | otherwise = error ("unmarshalTextureCompareMode: illegal value " ++ show x)

--------------------------------------------------------------------------------

-- ToDo: cube maps
textureCompareMode :: TextureTarget -> StateVar (Maybe ComparisonFunction)
textureCompareMode t =
   makeStateVar
      (do tcm <- get (textureCompareMode' t)
          case tcm of
             None -> return Nothing
             CompareRToTexture -> liftM Just $ get (textureCompareFunc t))
      (maybe (textureCompareMode' t $= None)
             (\tcf -> do textureCompareMode' t $= CompareRToTexture
                         textureCompareFunc t $= tcf))

textureCompareMode' :: TextureTarget -> StateVar TextureCompareMode
textureCompareMode' t =
   makeStateVar
      (getTexParameteri unmarshalTextureCompareMode t TextureCompareMode)
      (texParameteri    marshalTextureCompareMode   t TextureCompareMode)

textureCompareFunc :: TextureTarget -> StateVar ComparisonFunction
textureCompareFunc t =
   makeStateVar
      (getTexParameteri (unmarshalComparisonFunction . fromIntegral) t TextureCompareFunc)
      (texParameteri    (fromIntegral . marshalComparisonFunction)   t TextureCompareFunc)

--------------------------------------------------------------------------------

-- ToDo: cube maps
textureCompareFailValue :: TextureTarget -> StateVar GLclampf
textureCompareFailValue t =
   makeStateVar
      (getTexParameterf id t TextureCompareFailValue)
      (texParameterf    id t TextureCompareFailValue)

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

-- ToDo: cube maps
textureCompareOperator :: TextureTarget -> StateVar (Maybe TextureCompareOperator)
textureCompareOperator t =
   makeStateVar
      (do c <- get (textureCompare t)
          case c of
             False -> return Nothing
             True -> liftM Just $ get (textureCompareOperator' t))
      (maybe (textureCompare t $= False)
             (\tco -> do textureCompare t $= True
                         textureCompareOperator' t $= tco))
          
textureCompare :: TextureTarget -> StateVar Bool
textureCompare t =
   makeStateVar
      (getTexParameteri (unmarshalGLboolean . fromIntegral) t TextureCompare)
      (texParameteri    (fromIntegral . marshalGLboolean)   t TextureCompare)

textureCompareOperator' :: TextureTarget -> StateVar TextureCompareOperator
textureCompareOperator' t =
   makeStateVar
      (getTexParameteri unmarshalTextureCompareOperator t TextureCompareOperator)
      (texParameteri    marshalTextureCompareOperator   t TextureCompareOperator)
