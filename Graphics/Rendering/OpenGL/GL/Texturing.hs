--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing
-- Copyright   :  (c) Sven Panne 2002-2004
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 3.8 (Texturing) of the OpenGL 1.5 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing (
   -- * Texture-related Data Types
   TextureTarget(..), Level, PixelInternalFormat(..), Border,
   TexturePosition1D(..), TexturePosition2D(..), TexturePosition3D(..),
   TextureSize1D(..), TextureSize2D(..), TextureSize3D(..),

   -- * Texture Image Specification
   texImage1D, texImage2D, texImage3D,

   -- * Alternate Texture Image Specification Commands
   copyTexImage1D, copyTexImage2D,
   texSubImage1D, texSubImage2D, texSubImage3D,
   copyTexSubImage1D, copyTexSubImage2D, copyTexSubImage3D,

   -- * Compressed Texture Images
   CompressedTextureFormat(..), compressedTextureFormats,
   CompressedPixelData(..),
   compressedTexImage1D, compressedTexImage2D, compressedTexImage3D,
   compressedTexSubImage1D, compressedTexSubImage2D, compressedTexSubImage3D,

   -- * Texture Parameters
   TextureFilter(..), textureFilter,
   Repetition(..), Clamping(..), textureWrapMode,
   textureBorderColor, textureLODRange, textureLevelRange,
   textureMaxAnisotropy, maxTextureMaxAnisotropy,

   -- * Texture Objects
   TextureObject, defaultTextureObject, textureBinding,
   textureResident, areTexturesResident,
   texturePriority, prioritizeTextures,

   -- * Texture Environment and Texture Functions
   TextureEnvMode(..), textureEnvMode,
   texture,

   -- * Texture Queries
   getTexImage, getCompressedTexImage,
   textureInternalFormat, textureSize1D, textureSize2D, textureSize3D,
   textureBorder, textureRGBASizes, textureIntensitySize, textureLuminanceSize,
   textureIndexSize, textureDepthBits, textureCompressedImageSize,
   textureProxyOK
) where

import Control.Monad ( liftM, liftM2, liftM3, liftM4 )
import Data.List ( partition, genericLength )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( withArray, peekArray, allocaArray )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLint, GLuint, GLsizei, GLenum, GLclampf )
import Graphics.Rendering.OpenGL.GL.BufferObjects ( ObjectName(..) )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapTexture1D,CapTexture2D,CapTexture3D,CapTextureCubeMap),
   Capability, makeCapability )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Position(..) )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.GLboolean (
   GLboolean, unmarshalGLboolean )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek1 )
import Graphics.Rendering.OpenGL.GL.PixelInternalFormat (
   PixelInternalFormat(..), marshalPixelInternalFormat,
   unmarshalPixelInternalFormat )
import Graphics.Rendering.OpenGL.GL.PixelData ( withPixelData )
import Graphics.Rendering.OpenGL.GL.PixelRectangles ( PixelData, Proxy(..) )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetTextureBinding1D,GetTextureBinding2D,GetTextureBinding3D,
            GetTextureBindingCubeMap,GetTextureBindingRectangle,
            GetNumCompressedTextureFormats,GetCompressedTextureFormats),
   getInteger1, getIntegerv, getEnum1)
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.TextureTarget (
   TextureTarget(..), marshalTextureTarget, marshalProxyTextureTarget )
import Graphics.Rendering.OpenGL.GL.TexEnv (
   TextureEnvMode(..), textureEnvMode )
import Graphics.Rendering.OpenGL.GL.TexParameter (
   Level, TextureFilter(..), textureFilter, Repetition(..), Clamping(..),
   textureWrapMode, textureBorderColor, textureLODRange, textureLevelRange,
   textureMaxAnisotropy, maxTextureMaxAnisotropy, textureResident,
   texturePriority )
import Graphics.Rendering.OpenGL.GL.VertexSpec( Color4(..) )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"

--------------------------------------------------------------------------------

textureTargetToGetPName :: TextureTarget -> GetPName
textureTargetToGetPName x = case x of
    Texture1D -> GetTextureBinding1D
    Texture2D -> GetTextureBinding2D
    Texture3D -> GetTextureBinding3D
    TextureCubeMap -> GetTextureBindingCubeMap
    TextureRectangle -> GetTextureBindingRectangle

textureTargetToEnableCap :: TextureTarget -> EnableCap
textureTargetToEnableCap x = case x of
    Texture1D -> CapTexture1D
    Texture2D -> CapTexture2D
    Texture3D -> CapTexture3D
    TextureCubeMap -> CapTextureCubeMap
    TextureRectangle -> error "ToDo: TextureRectangle"

--------------------------------------------------------------------------------

type Border = GLint

newtype TexturePosition1D = TexturePosition1D GLint
   deriving ( Eq, Ord, Show )

data TexturePosition2D = TexturePosition2D GLint GLint
   deriving ( Eq, Ord, Show )

data TexturePosition3D = TexturePosition3D GLint GLint GLint
   deriving ( Eq, Ord, Show )

newtype TextureSize1D = TextureSize1D GLsizei
   deriving ( Eq, Ord, Show )

data TextureSize2D = TextureSize2D GLsizei GLsizei
   deriving ( Eq, Ord, Show )

data TextureSize3D = TextureSize3D GLsizei GLsizei GLsizei
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

texImage1D :: Proxy -> Level -> PixelInternalFormat -> TextureSize1D -> Border -> PixelData a -> IO ()
texImage1D proxy level int (TextureSize1D w) border pd =
   withPixelData pd $
      glTexImage1D
         (marshalProxyTextureTarget proxy Texture1D)
         level (fromIntegral (marshalPixelInternalFormat int)) w border

foreign import CALLCONV unsafe "glTexImage1D"
   glTexImage1D :: GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ()

--------------------------------------------------------------------------------

-- ToDo: cube maps
texImage2D :: Proxy -> Level -> PixelInternalFormat -> TextureSize2D -> Border -> PixelData a -> IO ()
texImage2D proxy level int (TextureSize2D w h) border pd =
   withPixelData pd $
      glTexImage2D
         (marshalProxyTextureTarget proxy Texture2D)
         level (fromIntegral (marshalPixelInternalFormat int)) w h border

foreign import CALLCONV unsafe "glTexImage2D"
   glTexImage2D :: GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ()

--------------------------------------------------------------------------------

texImage3D :: Proxy -> Level -> PixelInternalFormat -> TextureSize3D -> Border -> PixelData a -> IO ()
texImage3D proxy level int (TextureSize3D w h d) border pd =
   withPixelData pd $
      glTexImage3DEXT
         (marshalProxyTextureTarget proxy Texture3D)
         level (fromIntegral (marshalPixelInternalFormat int)) w h d border

EXTENSION_ENTRY("GL_EXT_texture3D or OpenGL 1.2",glTexImage3DEXT,GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())

--------------------------------------------------------------------------------

-- ToDo: cube maps
getTexImage :: TextureTarget -> Level -> PixelData a -> IO ()
getTexImage t level pd =
   withPixelData pd $
      glGetTexImage (marshalTextureTarget t) level

foreign import CALLCONV unsafe "glGetTexImage"
   glGetTexImage :: GLenum -> GLint -> GLenum -> GLenum -> Ptr a -> IO ()

--------------------------------------------------------------------------------

copyTexImage1D :: Level -> PixelInternalFormat -> Position -> TextureSize1D -> Border -> IO ()
copyTexImage1D level int (Position x y) (TextureSize1D w) border =
   glCopyTexImage1D
      (marshalTextureTarget Texture1D) level
      (marshalPixelInternalFormat int) x y w border

foreign import CALLCONV unsafe "glCopyTexImage1D"
   glCopyTexImage1D :: GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ()

--------------------------------------------------------------------------------

-- ToDo: cube maps
copyTexImage2D :: Level -> PixelInternalFormat -> Position -> TextureSize2D -> Border -> IO ()
copyTexImage2D level int (Position x y) (TextureSize2D w h) border =
   glCopyTexImage2D
      (marshalTextureTarget Texture2D) level
      (marshalPixelInternalFormat int) x y w h border

foreign import CALLCONV unsafe "glCopyTexImage2D"
   glCopyTexImage2D :: GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ()

--------------------------------------------------------------------------------

texSubImage1D :: Level -> TexturePosition1D -> TextureSize1D -> PixelData a -> IO ()
texSubImage1D level (TexturePosition1D xOff) (TextureSize1D w) pd =
   withPixelData pd $
      glTexSubImage1D (marshalTextureTarget Texture1D) level xOff w

foreign import CALLCONV unsafe "glTexSubImage1D"
   glTexSubImage1D :: GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ()

--------------------------------------------------------------------------------

-- ToDo: cube maps
texSubImage2D :: Level -> TexturePosition2D -> TextureSize2D -> PixelData a -> IO ()
texSubImage2D level (TexturePosition2D xOff yOff) (TextureSize2D w h) pd =
   withPixelData pd $
      glTexSubImage2D (marshalTextureTarget Texture2D) level xOff yOff w h

foreign import CALLCONV unsafe "glTexSubImage2D"
   glTexSubImage2D :: GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ()

--------------------------------------------------------------------------------

texSubImage3D :: Level -> TexturePosition3D -> TextureSize3D -> PixelData a -> IO ()
texSubImage3D level (TexturePosition3D xOff yOff zOff) (TextureSize3D w h d) pd =
   withPixelData pd $
      glTexSubImage3DEXT (marshalTextureTarget Texture3D) level xOff yOff zOff w h d

EXTENSION_ENTRY("GL_EXT_texture3D or OpenGL 1.2",glTexSubImage3DEXT,GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())

--------------------------------------------------------------------------------

copyTexSubImage1D :: Level -> TexturePosition1D -> Position -> TextureSize1D -> IO ()
copyTexSubImage1D level (TexturePosition1D xOff) (Position x y) (TextureSize1D w) =
   glCopyTexSubImage1D (marshalTextureTarget Texture1D) level xOff x y w

foreign import CALLCONV unsafe "glCopyTexSubImage1D"
   glCopyTexSubImage1D :: GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ()

--------------------------------------------------------------------------------

-- ToDo: cube maps
copyTexSubImage2D :: Level -> TexturePosition2D -> Position -> TextureSize2D -> IO ()
copyTexSubImage2D level (TexturePosition2D xOff yOff) (Position x y) (TextureSize2D w h) =
   glCopyTexSubImage2D (marshalTextureTarget Texture2D) level xOff yOff x y w h

foreign import CALLCONV unsafe "glCopyTexSubImage2D"
   glCopyTexSubImage2D :: GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()

--------------------------------------------------------------------------------

copyTexSubImage3D :: Level -> TexturePosition3D -> Position -> TextureSize2D -> IO ()
copyTexSubImage3D level (TexturePosition3D xOff yOff zOff) (Position x y) (TextureSize2D w h) =
   glCopyTexSubImage3DEXT (marshalTextureTarget Texture3D) level xOff yOff zOff x y w h

EXTENSION_ENTRY("GL_EXT_texture3D or OpenGL 1.2",glCopyTexSubImage3DEXT,GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())

--------------------------------------------------------------------------------

newtype CompressedTextureFormat = CompressedTextureFormat GLenum
   deriving ( Eq, Ord, Show )

compressedTextureFormats :: GettableStateVar [CompressedTextureFormat]
compressedTextureFormats =
   makeGettableStateVar $ do
      n <- getInteger1 fromIntegral GetNumCompressedTextureFormats
      allocaArray n $ \buf -> do
         getIntegerv GetCompressedTextureFormats buf
         liftM (map (CompressedTextureFormat . fromIntegral)) $ peekArray n buf

--------------------------------------------------------------------------------

data CompressedPixelData a =
     CompressedPixelData CompressedTextureFormat GLsizei (Ptr a)
#ifdef __HADDOCK__
-- Help Haddock a bit, because it doesn't do any instance inference.
instance Eq (CompressedPixelData a)
instance Ord (CompressedPixelData a)
instance Show (CompressedPixelData a)
#else
   deriving ( Eq, Ord, Show )
#endif

withCompressedPixelData ::
   CompressedPixelData a -> (GLenum -> GLsizei -> Ptr a -> b) -> b
withCompressedPixelData
   (CompressedPixelData (CompressedTextureFormat fmt) size ptr) f =
   f fmt size ptr

--------------------------------------------------------------------------------

compressedTexImage1D :: Proxy -> Level -> TextureSize1D -> Border -> CompressedPixelData a -> IO ()
compressedTexImage1D proxy level (TextureSize1D w) border cpd =
   withCompressedPixelData cpd $ \fmt ->
      glCompressedTexImage1DARB
         (marshalProxyTextureTarget proxy Texture1D) level fmt w border

EXTENSION_ENTRY("GL_ARB_texture_compression or OpenGL 1.3",glCompressedTexImage1DARB,GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())

--------------------------------------------------------------------------------

-- ToDo: cube maps
compressedTexImage2D :: Proxy -> Level -> TextureSize2D -> Border -> CompressedPixelData a -> IO ()
compressedTexImage2D proxy level (TextureSize2D w h) border cpd =
   withCompressedPixelData cpd $ \fmt ->
      glCompressedTexImage2DARB
         (marshalProxyTextureTarget proxy Texture2D) level fmt w h border

EXTENSION_ENTRY("GL_ARB_texture_compression or OpenGL 1.3",glCompressedTexImage2DARB,GLenum -> GLint -> GLenum -> GLsizei -> GLsizei ->GLint -> GLsizei -> Ptr a -> IO ())

--------------------------------------------------------------------------------

compressedTexImage3D :: Proxy -> Level -> TextureSize3D -> Border -> CompressedPixelData a -> IO ()
compressedTexImage3D proxy level (TextureSize3D w h d) border cpd =
   withCompressedPixelData cpd $ \fmt ->
      glCompressedTexImage3DARB
         (marshalProxyTextureTarget proxy Texture3D) level fmt w h d border

EXTENSION_ENTRY("GL_ARB_texture_compression or OpenGL 1.3",glCompressedTexImage3DARB,GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())

--------------------------------------------------------------------------------

-- ToDo: cube maps
getCompressedTexImage :: TextureTarget -> Level -> Ptr a -> IO ()
getCompressedTexImage = glGetCompressedTexImageARB . marshalTextureTarget

EXTENSION_ENTRY("GL_ARB_texture_compression or OpenGL 1.3",glGetCompressedTexImageARB,GLenum -> GLint -> Ptr a -> IO ())

--------------------------------------------------------------------------------

compressedTexSubImage1D :: Level -> TexturePosition1D -> TextureSize1D -> CompressedPixelData a -> IO ()
compressedTexSubImage1D level (TexturePosition1D xOff) (TextureSize1D w) cpd =
   withCompressedPixelData cpd $
      glCompressedTexSubImage1DARB (marshalTextureTarget Texture1D) level xOff w

EXTENSION_ENTRY("GL_ARB_texture_compression or OpenGL 1.3",glCompressedTexSubImage1DARB,GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())

--------------------------------------------------------------------------------

-- ToDo: cube maps
compressedTexSubImage2D :: Level -> TexturePosition2D -> TextureSize2D -> CompressedPixelData a -> IO ()
compressedTexSubImage2D level (TexturePosition2D xOff yOff) (TextureSize2D w h) cpd =
   withCompressedPixelData cpd $
      glCompressedTexSubImage2DARB (marshalTextureTarget Texture2D) level xOff yOff w h

EXTENSION_ENTRY("GL_ARB_texture_compression or OpenGL 1.3",glCompressedTexSubImage2DARB,GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a-> IO ())

--------------------------------------------------------------------------------

compressedTexSubImage3D :: Level -> TexturePosition3D -> TextureSize3D -> CompressedPixelData a -> IO ()
compressedTexSubImage3D level (TexturePosition3D xOff yOff zOff) (TextureSize3D w h d) cpd =
   withCompressedPixelData cpd $
      glCompressedTexSubImage3DARB (marshalTextureTarget Texture3D) level xOff yOff zOff w h d

EXTENSION_ENTRY("GL_ARB_texture_compression or OpenGL 1.3",glCompressedTexSubImage3DARB,GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())

--------------------------------------------------------------------------------

data TexLevelParameter =
     TextureInternalFormat
   | TextureWidth
   | TextureHeight
   | TextureDepth
   | TextureBorder
   | TextureRedSize
   | TextureGreenSize
   | TextureBlueSize
   | TextureAlphaSize
   | TextureIntensitySize
   | TextureLuminanceSize
   | TextureIndexSize
   | DepthBits
   | TextureCompressedImageSize
   | TextureCompressed

marshalTexLevelParameter :: TexLevelParameter -> GLenum
marshalTexLevelParameter x = case x of
   TextureInternalFormat -> 0x1003
   TextureWidth -> 0x1000
   TextureHeight -> 0x1001
   TextureDepth -> 0x8071
   TextureBorder -> 0x1005
   TextureRedSize -> 0x805C
   TextureGreenSize -> 0x805D
   TextureBlueSize -> 0x805E
   TextureAlphaSize -> 0x805F
   TextureIntensitySize -> 0x8061
   TextureLuminanceSize -> 0x8060
   TextureIndexSize -> 0x80ED
   DepthBits -> 0x0D56
   TextureCompressedImageSize -> 0x86A0
   TextureCompressed -> 0x86A1

--------------------------------------------------------------------------------

-- ToDo: cube maps
textureInternalFormat :: TextureTarget -> Level -> GettableStateVar PixelInternalFormat
textureInternalFormat t level =
   makeGettableStateVar $
      getTexLevelParameteri (unmarshalPixelInternalFormat . fromIntegral) NoProxy t level TextureInternalFormat

-- ToDo: cube maps
textureSize1D :: TextureTarget -> Level -> GettableStateVar TextureSize1D
textureSize1D t level =
   makeGettableStateVar $
      getTexLevelParameteri (TextureSize1D . fromIntegral) NoProxy t level TextureWidth

-- ToDo: cube maps
textureSize2D :: TextureTarget -> Level -> GettableStateVar TextureSize2D
textureSize2D t level =
   makeGettableStateVar $
      liftM2 TextureSize2D
             (getTexLevelParameteri fromIntegral NoProxy t level TextureWidth )
             (getTexLevelParameteri fromIntegral NoProxy t level TextureHeight)

-- ToDo: cube maps
textureSize3D :: TextureTarget -> Level -> GettableStateVar TextureSize3D
textureSize3D t level =
   makeGettableStateVar $
      liftM3 TextureSize3D
             (getTexLevelParameteri fromIntegral NoProxy t level TextureWidth )
             (getTexLevelParameteri fromIntegral NoProxy t level TextureHeight)
             (getTexLevelParameteri fromIntegral NoProxy t level TextureDepth )

-- ToDo: cube maps
textureBorder :: TextureTarget -> Level -> GettableStateVar Border
textureBorder t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level TextureBorder

-- ToDo: cube maps
textureRGBASizes :: TextureTarget -> Level -> GettableStateVar (Color4 GLsizei)
textureRGBASizes t level =
   makeGettableStateVar $
      liftM4 Color4
             (getTexLevelParameteri fromIntegral NoProxy t level TextureRedSize  )
             (getTexLevelParameteri fromIntegral NoProxy t level TextureGreenSize)
             (getTexLevelParameteri fromIntegral NoProxy t level TextureBlueSize )
             (getTexLevelParameteri fromIntegral NoProxy t level TextureAlphaSize)

-- ToDo: cube maps
textureIntensitySize :: TextureTarget -> Level -> GettableStateVar GLsizei
textureIntensitySize t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level TextureIntensitySize

-- ToDo: cube maps
textureLuminanceSize :: TextureTarget -> Level -> GettableStateVar GLsizei
textureLuminanceSize t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level TextureLuminanceSize

-- ToDo: cube maps
textureIndexSize :: TextureTarget -> Level -> GettableStateVar GLsizei
textureIndexSize t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level TextureIndexSize

-- ToDo: cube maps
textureDepthBits :: TextureTarget -> Level -> GettableStateVar GLsizei
textureDepthBits t level =
   makeGettableStateVar $
      getTexLevelParameteri fromIntegral NoProxy t level DepthBits

-- ToDo: cube maps
textureCompressedImageSize :: TextureTarget -> Level -> GettableStateVar (Maybe GLsizei)
textureCompressedImageSize t level =
   makeGettableStateVar $ do
      isCompressed <- getTexLevelParameteri (unmarshalGLboolean . fromIntegral) NoProxy t level TextureCompressed
      if isCompressed
         then getTexLevelParameteri (Just . fromIntegral) NoProxy t level TextureCompressedImageSize
         else return Nothing

-- ToDo: cube maps
textureProxyOK :: TextureTarget -> Level -> GettableStateVar Bool
textureProxyOK t level =
   makeGettableStateVar $
      getTexLevelParameteri (unmarshalGLboolean . fromIntegral) Proxy t level TextureWidth

getTexLevelParameteri :: (GLint -> a) -> Proxy -> TextureTarget -> Level -> TexLevelParameter -> IO a
getTexLevelParameteri f proxy t level p =
   alloca $ \buf -> do
      glGetTexLevelParameteriv (marshalProxyTextureTarget proxy t) level (marshalTexLevelParameter p) buf
      peek1 f buf

-- all targets incl. proxies and all cube maps
foreign import CALLCONV unsafe "glGetTexLevelParameteriv"
   glGetTexLevelParameteriv :: GLenum -> GLint -> GLenum -> Ptr GLint -> IO ()

--------------------------------------------------------------------------------

newtype TextureObject = TextureObject { textureID :: GLuint }
   deriving ( Eq, Ord, Show )

defaultTextureObject :: TextureObject
defaultTextureObject = TextureObject 0

--------------------------------------------------------------------------------

instance ObjectName TextureObject where
   genObjectNames n =
      allocaArray n $ \buf -> do
        glGenTextures (fromIntegral n) buf
        liftM (map TextureObject) $ peekArray n buf

   deleteObjectNames textureObjects = do
      withArray (map textureID textureObjects) $
         glDeleteTextures (genericLength textureObjects)

   isObjectName = liftM unmarshalGLboolean . glIsTexture . textureID

foreign import CALLCONV unsafe "glGenTextures"
   glGenTextures :: GLsizei -> Ptr GLuint -> IO ()

foreign import CALLCONV unsafe "glDeleteTextures"
   glDeleteTextures :: GLsizei -> Ptr GLuint -> IO ()

foreign import CALLCONV unsafe "glIsTexture"
   glIsTexture :: GLuint -> IO GLboolean

--------------------------------------------------------------------------------

textureBinding :: TextureTarget -> StateVar TextureObject
textureBinding t =
   makeStateVar
      (getEnum1 TextureObject (textureTargetToGetPName t))
      (glBindTexture (marshalTextureTarget t) . textureID)

foreign import CALLCONV unsafe "glBindTexture"
   glBindTexture :: GLenum -> GLuint -> IO ()

--------------------------------------------------------------------------------

areTexturesResident :: [TextureObject] -> IO ([TextureObject],[TextureObject])
areTexturesResident texObjs = do
   let len = length texObjs
   withArray (map textureID texObjs) $ \texObjsBuf ->
      allocaArray len $ \residentBuf -> do
         allResident <-
            glAreTexturesResident (fromIntegral len) texObjsBuf residentBuf
         if unmarshalGLboolean allResident
            then return (texObjs, [])
            else do
               tr <- liftM (zip texObjs) $ peekArray len residentBuf
               let (resident, nonResident) = partition (unmarshalGLboolean . snd) tr
               return (map fst resident, map fst nonResident)

foreign import CALLCONV unsafe "glAreTexturesResident"
   glAreTexturesResident :: GLsizei -> Ptr GLuint -> Ptr GLboolean -> IO GLboolean

--------------------------------------------------------------------------------

prioritizeTextures :: [(TextureObject,GLclampf)] -> IO ()
prioritizeTextures tps =
   withArray (map (textureID . fst) tps) $ \texObjsBuf ->
      withArray (map snd tps) $
         glPrioritizeTextures (genericLength tps) texObjsBuf

foreign import CALLCONV unsafe "glPrioritizeTextures"
   glPrioritizeTextures :: GLsizei -> Ptr GLuint -> Ptr GLclampf -> IO ()

--------------------------------------------------------------------------------

-- ToDo: cube maps
texture :: TextureTarget -> StateVar Capability
texture = makeCapability . textureTargetToEnableCap
