--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Objects
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 3.8.12 (Texture Objects) of the OpenGL 2.1
-- specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing.Objects (
   TextureObject(TextureObject), textureBinding,
   textureResident, areTexturesResident,
   TexturePriority, texturePriority, prioritizeTextures
) where

import Data.List ( partition )
import Foreign.Marshal.Array ( withArray, withArrayLen, peekArray, allocaArray )
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility
import Graphics.Rendering.OpenGL.GL.BufferObjects ( ObjectName(..) )
import Graphics.Rendering.OpenGL.GL.GLboolean ( unmarshalGLboolean )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetTextureBinding1D,GetTextureBinding2D,GetTextureBinding3D,
            GetTextureBindingCubeMap,GetTextureBindingRectangle),
   getEnum1)
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.Texturing.TexParameter (
   TexParameter(TextureResident,TexturePriority), texParamf, getTexParameteri )
import Graphics.Rendering.OpenGL.GL.Texturing.Specification (
   TextureTarget(..) )
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget (
   marshalTextureTarget )

--------------------------------------------------------------------------------

newtype TextureObject = TextureObject { textureID :: GLuint }
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

instance ObjectName TextureObject where
   genObjectNames n =
      allocaArray n $ \buf -> do
        glGenTextures (fromIntegral n) buf
        fmap (map TextureObject) $ peekArray n buf

   deleteObjectNames textureObjects =
      withArrayLen (map textureID textureObjects) $
         glDeleteTextures . fromIntegral

   isObjectName = fmap unmarshalGLboolean . glIsTexture . textureID

--------------------------------------------------------------------------------

textureBinding :: TextureTarget -> StateVar (Maybe TextureObject)
textureBinding t =
   makeStateVar
      (do o <- getEnum1 TextureObject (textureTargetToGetPName t)
          return $ if o == defaultTextureObject then Nothing else Just o)
      (glBindTexture (marshalTextureTarget t) . textureID . (maybe defaultTextureObject id))

defaultTextureObject :: TextureObject
defaultTextureObject = TextureObject 0

textureTargetToGetPName :: TextureTarget -> GetPName
textureTargetToGetPName x = case x of
    Texture1D -> GetTextureBinding1D
    Texture2D -> GetTextureBinding2D
    Texture3D -> GetTextureBinding3D
    TextureCubeMap -> GetTextureBindingCubeMap
    TextureRectangle -> GetTextureBindingRectangle

--------------------------------------------------------------------------------

textureResident :: TextureTarget -> GettableStateVar Bool
textureResident t =
   makeGettableStateVar $
      getTexParameteri unmarshalGLboolean t TextureResident

areTexturesResident :: [TextureObject] -> IO ([TextureObject],[TextureObject])
areTexturesResident texObjs = do
   withArrayLen (map textureID texObjs) $ \len texObjsBuf ->
      allocaArray len $ \residentBuf -> do
         allResident <-
            glAreTexturesResident (fromIntegral len) texObjsBuf residentBuf
         if unmarshalGLboolean allResident
            then return (texObjs, [])
            else do
               tr <- fmap (zip texObjs) $ peekArray len residentBuf
               let (resident, nonResident) = partition (unmarshalGLboolean . snd) tr
               return (map fst resident, map fst nonResident)

--------------------------------------------------------------------------------

type TexturePriority = GLclampf

texturePriority :: TextureTarget -> StateVar TexturePriority
texturePriority = texParamf id id TexturePriority

prioritizeTextures :: [(TextureObject,TexturePriority)] -> IO ()
prioritizeTextures tps =
   withArrayLen (map (textureID . fst) tps) $ \len texObjsBuf ->
      withArray (map snd tps) $
         glPrioritizeTextures (fromIntegral len) texObjsBuf
