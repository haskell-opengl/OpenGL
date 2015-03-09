{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.QueryObject
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for handling QueryObjects.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.QueryObject (
   QueryObject(..), noQueryObject
) where

import Control.Monad.IO.Class
import Data.ObjectName
import Foreign.Marshal.Array ( allocaArray, peekArray, withArrayLen )
import Graphics.Rendering.OpenGL.GL.DebugOutput
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

newtype QueryObject = QueryObject { queryID :: GLuint }
   deriving ( Eq, Ord, Show )

noQueryObject :: QueryObject
noQueryObject = QueryObject 0

--------------------------------------------------------------------------------

instance ObjectName QueryObject where
   isObjectName = liftIO . fmap unmarshalGLboolean . glIsQuery . queryID

   deleteObjectNames queryObjects =
      liftIO . withArrayLen (map queryID queryObjects) $
         glDeleteQueries . fromIntegral

instance GeneratableObjectName QueryObject where
   genObjectNames n =
      liftIO . allocaArray n $ \buf -> do
        glGenQueries (fromIntegral n) buf
        fmap (map QueryObject) $ peekArray n buf

instance CanBeLabeled QueryObject where
   objectLabel = objectNameLabel gl_QUERY . queryID
