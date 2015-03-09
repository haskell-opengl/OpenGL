--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Objects
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
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
   TexturePriority, texturePriority, prioritizeTextures,
   generateMipmap'
) where

import Data.List
import Data.Maybe (fromMaybe)
import Data.StateVar
import Foreign.Marshal.Array
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.Texturing.TexParameter
import Graphics.Rendering.OpenGL.GL.Texturing.TextureObject
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

textureBinding :: BindableTextureTarget t => t -> StateVar (Maybe TextureObject)
textureBinding t =
   makeStateVar
      (do o <- getEnum1 (TextureObject . fromIntegral) (marshalBindableTextureTargetPName1I t)
          return $ if o == defaultTextureObject then Nothing else Just o)
      (glBindTexture (marshalBindableTextureTarget t) . textureID . (fromMaybe defaultTextureObject))

defaultTextureObject :: TextureObject
defaultTextureObject = TextureObject 0

--------------------------------------------------------------------------------

textureResident :: ParameterizedTextureTarget t => t -> GettableStateVar Bool
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

texturePriority :: ParameterizedTextureTarget t => t -> StateVar TexturePriority
texturePriority = texParamf realToFrac realToFrac TexturePriority

prioritizeTextures :: [(TextureObject,TexturePriority)] -> IO ()
prioritizeTextures tps =
   withArrayLen (map (textureID . fst) tps) $ \len texObjsBuf ->
      withArray (map snd tps) $
         glPrioritizeTextures (fromIntegral len) texObjsBuf

--------------------------------------------------------------------------------

-- | Generate mipmaps for the specified texture target. Note that from OpenGL
-- 3.1 onwards you should use this function instead of the texture parameter
-- 'Graphics.Rendering.OpenGL.GL.Texturing.Parameters.generateMipmap'.

generateMipmap' :: ParameterizedTextureTarget t => t -> IO ()
generateMipmap' = glGenerateMipmap . marshalParameterizedTextureTarget
