--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.ObjectName
-- Copyright   :  (c) Sven Panne 2009
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- Object names are explicitly handled identifiers for API objects, e.g. a
-- texture object name in OpenGL or a buffer object name in OpenAL.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.ObjectName ( ObjectName(..) ) where

-- | An 'ObjectName' is an explicitly handled identifier for API objects, e.g. a
-- texture object name in OpenGL or a buffer object name in OpenAL.

class ObjectName a where
   -- | Generate a given number of object names, which are guaranteed to be
   -- unused. By generating the names, they become used.
   genObjectNames :: Int -> IO [a]

   -- | Make the given object names available again, declaring them as unused.
   deleteObjectNames:: [a] -> IO ()

   -- | Test if the given object name is currently in use, i.e. test if it has
   -- been generated, but not been deleted so far.
   isObjectName :: a -> IO Bool
