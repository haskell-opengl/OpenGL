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
   QueryObject(..)
) where

import Foreign.Marshal.Array
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.ObjectName
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

newtype QueryObject = QueryObject { queryID :: GLuint }
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

instance ObjectName QueryObject where
   isObjectName = fmap unmarshalGLboolean . glIsQuery . queryID

   deleteObjectNames queryObjects =
      withArrayLen (map queryID queryObjects) $
         glDeleteQueries . fromIntegral

instance GeneratableObjectName QueryObject where
   genObjectNames n =
      allocaArray n $ \buf -> do
        glGenQueries (fromIntegral n) buf
        fmap (map QueryObject) $ peekArray n buf
