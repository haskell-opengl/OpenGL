-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Domain
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for handling evaluator domains.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Domain (
   Domain(..)
) where

import Foreign.C.Types
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable )
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName, getFloat2, getFloat4, getDouble2, getDouble4 )

--------------------------------------------------------------------------------

class Storable d => Domain d where
   glMap1      :: GLenum -> d -> d -> GLint -> GLint -> Ptr d -> IO ()
   glMap2      :: GLenum -> d -> d -> GLint -> GLint -> d -> d -> GLint -> GLint -> Ptr d -> IO ()
   glGetMapv   :: GLenum -> GLenum -> Ptr d -> IO ()
   evalCoord1  :: d -> IO ()
   evalCoord1v :: Ptr d -> IO ()
   evalCoord2  :: (d, d) -> IO ()
   evalCoord2v :: Ptr d -> IO ()
   glMapGrid1  :: GLint -> d -> d -> IO ()
   glMapGrid2  :: GLint -> d -> d -> GLint -> d -> d -> IO ()
   get2        :: (d -> d -> a) -> GetPName -> IO a
   get4        :: (d -> d -> d -> d -> a) -> GetPName -> IO a

--------------------------------------------------------------------------------

-- GLfloat instance
instance Domain CFloat where
   glMap1      = glMap1f
   glMap2      = glMap2f
   glGetMapv   = glGetMapfv
   evalCoord1  = glEvalCoord1f
   evalCoord1v = glEvalCoord1fv
   evalCoord2  = uncurry glEvalCoord2f
   evalCoord2v = glEvalCoord2fv
   glMapGrid1  = glMapGrid1f
   glMapGrid2  = glMapGrid2f
   get2        = getFloat2
   get4        = getFloat4

-- GLdouble instance
instance Domain CDouble where
   glMap1      = glMap1d
   glMap2      = glMap2d
   glGetMapv   = glGetMapdv
   evalCoord1  = glEvalCoord1d
   evalCoord1v = glEvalCoord1dv
   evalCoord2  = uncurry glEvalCoord2d
   evalCoord2v = glEvalCoord2dv
   glMapGrid1  = glMapGrid1d
   glMapGrid2  = glMapGrid2d
   get2        = getDouble2
   get4        = getDouble4
