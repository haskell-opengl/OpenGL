-- #hide
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
-- This is a purely internal module for glTexParameter-related stuff.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing.Parameters where

import Control.Monad ( liftM2 )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable(peek) )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLint, GLenum, GLfloat, GLclampf )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( TextureCoordName(..) )
import Graphics.Rendering.OpenGL.GL.GLboolean ( unmarshalGLboolean )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek1 )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetMaxTextureMaxAnisotropy), getFloat1)
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.Texturing.Specification ( Level )
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget (
   TextureTarget(..), marshalTextureTarget )
import Graphics.Rendering.OpenGL.GL.VertexSpec( Color4(..) )
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal ( recordInvalidEnum )

--------------------------------------------------------------------------------

data TexParameter =
     TextureMinFilter
   | TextureMagFilter
   | TextureWrapS
   | TextureWrapT
   | TextureWrapR
   | TextureBorderColor
   | TextureMinLod
   | TextureMaxLod
   | TextureBaseLevel
   | TextureMaxLevel
   | TexturePriority
   | TextureMaxAnisotropy
   | TextureCompare
   | TextureCompareOperator
   | TextureCompareFailValue
   | GenerateMipmap
   | TextureCompareMode
   | TextureCompareFunc
   | DepthTextureMode
   | TextureLodBias
   | TextureResident

marshalTexParameter :: TexParameter -> GLenum
marshalTexParameter x = case x of
   TextureMinFilter -> 0x2801
   TextureMagFilter -> 0x2800
   TextureWrapS -> 0x2802
   TextureWrapT -> 0x2803
   TextureWrapR -> 0x8072
   TextureBorderColor -> 0x1004
   TextureMinLod -> 0x813A
   TextureMaxLod -> 0x813B
   TextureBaseLevel -> 0x813C
   TextureMaxLevel -> 0x813D
   TexturePriority -> 0x8066
   TextureMaxAnisotropy -> 0x84FE
   TextureCompare -> 0x819A
   TextureCompareOperator -> 0x819B
   TextureCompareFailValue -> 0x80BF
   GenerateMipmap -> 0x8191
   TextureCompareMode -> 0x884C
   TextureCompareFunc -> 0x884D
   DepthTextureMode -> 0x884B
   TextureLodBias -> 0x8501
   TextureResident -> 0x8067

--------------------------------------------------------------------------------

texParameteri :: (a -> GLint) -> TextureTarget -> TexParameter -> a -> IO ()
texParameteri f t p =
   glTexParameteri (marshalTextureTarget t) (marshalTexParameter p) . f

foreign import CALLCONV unsafe "glTexParameteri"
   glTexParameteri :: GLenum -> GLenum ->  GLint -> IO ()

texParameterf :: TextureTarget -> TexParameter -> GLfloat -> IO ()
texParameterf t = glTexParameterf (marshalTextureTarget t) . marshalTexParameter

foreign import CALLCONV unsafe "glTexParameterf"
   glTexParameterf :: GLenum -> GLenum ->  GLfloat -> IO ()

texParameterC4f :: TextureTarget -> TexParameter -> Color4 GLfloat -> IO ()
texParameterC4f t p c =
   with c $
      glTexParameterC4f (marshalTextureTarget t) (marshalTexParameter p)

foreign import CALLCONV unsafe "glTexParameterfv"
   glTexParameterC4f :: GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()

--------------------------------------------------------------------------------

getTexParameteri :: (GLint -> a) -> TextureTarget -> TexParameter -> IO a
getTexParameteri f t p =
   alloca $ \buf -> do
     glGetTexParameteriv (marshalTextureTarget t) (marshalTexParameter p) buf
     peek1 f buf

foreign import CALLCONV unsafe "glGetTexParameteriv"
   glGetTexParameteriv :: GLenum -> GLenum -> Ptr GLint -> IO ()

getTexParameterf :: TextureTarget -> TexParameter -> IO GLfloat
getTexParameterf t p =
   alloca $ \buf -> do
     glGetTexParameterfv (marshalTextureTarget t) (marshalTexParameter p) buf
     peek buf

foreign import CALLCONV unsafe "glGetTexParameterfv"
   glGetTexParameterfv :: GLenum -> GLenum -> Ptr GLfloat -> IO ()

getTexParameterC4f :: TextureTarget -> TexParameter -> IO (Color4 GLfloat)
getTexParameterC4f t p =
   alloca $ \buf -> do
     glGetTexParameterC4f (marshalTextureTarget t) (marshalTexParameter p) buf
     peek buf

foreign import CALLCONV unsafe "glGetTexParameterfv"
   glGetTexParameterC4f :: GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()

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

-- ToDo: cube maps
textureFilter :: TextureTarget -> StateVar ((TextureFilter, Maybe TextureFilter), TextureFilter)
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
       (liftM2 (,) (getTexParameterf t TextureMinLod)
                   (getTexParameterf t TextureMaxLod))
       (\(minLOD,maxLOD) -> do
          texParameterf t TextureMinLod minLOD
          texParameterf t TextureMaxLod maxLOD)

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

--------------------------------------------------------------------------------

textureResident :: TextureTarget -> GettableStateVar Bool
textureResident t =
   makeGettableStateVar $
      getTexParameteri (unmarshalGLboolean . fromIntegral) t TextureResident

texturePriority :: TextureTarget -> StateVar GLclampf
texturePriority t =
   makeStateVar
      (getTexParameterf t TexturePriority)
      (texParameterf t TexturePriority)
