--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Selection
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 5.2 (Selection) of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Selection (
   HitRecord(..), getHitRecords,
   Name(..), withName, loadName, maxNameStackDepth, nameStackDepth,
   RenderMode(..), renderMode
) where

import Data.StateVar
import Foreign.Marshal.Array
import Foreign.Ptr
import Graphics.Rendering.OpenGL.GL.Exception
import Graphics.Rendering.OpenGL.GL.IOState
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.RenderMode
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

data HitRecord = HitRecord GLfloat GLfloat [Name]
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

getHitRecords :: GLsizei -> IO a -> IO (a, Maybe [HitRecord])
getHitRecords bufSize action =
   allocaArray (fromIntegral bufSize) $ \buf -> do
      glSelectBuffer bufSize buf
      (value, numHits) <- withRenderMode Select $ do
         glInitNames
         action
      hits <- parseSelectionBuffer numHits buf
      return (value, hits)

--------------------------------------------------------------------------------

parseSelectionBuffer :: GLint -> Ptr GLuint -> IO (Maybe [HitRecord])
parseSelectionBuffer numHits buf
   | numHits < 0 = return Nothing
   | otherwise = fmap Just $ evalIOState (nTimes numHits parseSelectionHit) buf

type Parser a = IOState GLuint a

parseSelectionHit :: Parser HitRecord
parseSelectionHit = do
   numNames <- parseGLuint
   minZ <- parseGLfloat
   maxZ <- parseGLfloat
   nameStack <- nTimes numNames parseName
   return $ HitRecord minZ maxZ nameStack

parseGLuint :: Parser GLuint
parseGLuint = peekIOState

parseGLfloat :: Parser GLfloat
parseGLfloat = fmap (\x -> fromIntegral x / 0xffffffff) parseGLuint

parseName :: Parser Name
parseName = fmap Name parseGLuint

--------------------------------------------------------------------------------

newtype Name = Name GLuint
   deriving ( Eq, Ord, Show )

withName :: Name -> IO a -> IO a
withName (Name name) = bracket_ (glPushName name) glPopName

loadName :: Name -> IO ()
loadName (Name n) = glLoadName n

maxNameStackDepth :: GettableStateVar GLsizei
maxNameStackDepth = makeGettableStateVar (getSizei1 id GetMaxNameStackDepth)

nameStackDepth :: GettableStateVar GLsizei
nameStackDepth = makeGettableStateVar (getSizei1 id GetNameStackDepth)
