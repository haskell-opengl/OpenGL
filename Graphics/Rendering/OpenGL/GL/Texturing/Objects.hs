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
   TextureObject, textureBinding,
   textureResident, areTexturesResident,
   TexturePriority, texturePriority, prioritizeTextures
) where

import Data.List
import Data.Maybe (fromMaybe)
import Foreign.Marshal.Array
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.Texturing.TexParameter
import Graphics.Rendering.OpenGL.GL.Texturing.TextureObject
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

textureBinding :: TextureTarget t => t -> StateVar (Maybe TextureObject)
textureBinding t =
   makeStateVar
      (do o <- getEnum1 (TextureObject . fromIntegral) (textureTargetToBinding t)
          return $ if o == defaultTextureObject then Nothing else Just o)
      (glBindTexture (marshalTextureTarget t) . textureID . (fromMaybe defaultTextureObject))

defaultTextureObject :: TextureObject
defaultTextureObject = TextureObject 0

--------------------------------------------------------------------------------

textureResident :: TextureTarget t => t -> GettableStateVar Bool
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

texturePriority :: TextureTarget t => t -> StateVar TexturePriority
texturePriority = texParamf realToFrac realToFrac TexturePriority

prioritizeTextures :: [(TextureObject,TexturePriority)] -> IO ()
prioritizeTextures tps =
   withArrayLen (map (textureID . fst) tps) $ \len texObjsBuf ->
      withArray (map snd tps) $
         glPrioritizeTextures (fromIntegral len) texObjsBuf
