--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Selection
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 5.2 (Selection) of the OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Selection (
   SelectionName(..), HitRecord(..), withSelection, withName, loadName,
   RenderMode(..), renderMode
) where

import Control.Monad ( liftM )
import Foreign.Marshal.Array ( allocaArray )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLint, GLsizei, GLuint, GLfloat )
import Graphics.Rendering.OpenGL.GL.Exception ( finally )
import Graphics.Rendering.OpenGL.GL.IOState
import Graphics.Rendering.OpenGL.GL.RenderMode (
   RenderMode(..), withRenderMode, renderMode )

--------------------------------------------------------------------------------

newtype SelectionName = SelectionName GLuint
   deriving ( Eq, Ord, Show )

data HitRecord = HitRecord GLfloat GLfloat [SelectionName]
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

withSelection :: GLsizei -> IO a -> IO (a, Maybe [HitRecord])
withSelection bufSize action =
   allocaArray (fromIntegral bufSize) $ \buf -> do
      glSelectBuffer bufSize buf
      (value, numHits) <- withRenderMode Select $ do
         glInitNames
         action
      hits <- getHitRecords numHits buf
      return (value, hits)

foreign import CALLCONV unsafe "glInitNames" glInitNames :: IO ()

foreign import CALLCONV unsafe "glSelectBuffer" glSelectBuffer ::
   GLsizei -> Ptr GLuint -> IO ()

--------------------------------------------------------------------------------

getHitRecords :: GLint -> Ptr GLuint -> IO (Maybe [HitRecord])
getHitRecords numHits buf
   | numHits < 0 = return Nothing
   | otherwise   = liftM Just $ evalIOState (nTimes numHits getSelectionHit) buf

type Parser a = IOState (Ptr GLuint) a

nTimes :: Integral a => a -> Parser b -> Parser [b]
nTimes n = sequence . replicate (fromIntegral n)

getSelectionHit :: Parser HitRecord
getSelectionHit = do
   nameStackDepth <- getGLuint
   minZ <- getGLfloat
   maxZ <- getGLfloat
   nameStack <- nTimes nameStackDepth getSelectionName
   return $ HitRecord minZ maxZ nameStack

getGLuint :: Parser GLuint
getGLuint = peekIOState

getGLfloat :: Parser GLfloat
getGLfloat = liftM (\x -> fromIntegral x / 0xffffffff) getGLuint

getSelectionName :: Parser SelectionName
getSelectionName = liftM SelectionName getGLuint

--------------------------------------------------------------------------------

withName :: SelectionName -> IO a -> IO a
withName name action = (do glPushName name ; action) `finally` glPopName

foreign import CALLCONV unsafe "glPopName" glPopName :: IO ()

foreign import CALLCONV unsafe "glPushName" glPushName :: SelectionName -> IO ()

foreign import CALLCONV unsafe "glLoadName" loadName :: SelectionName -> IO ()
