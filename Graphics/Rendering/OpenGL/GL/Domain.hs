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

import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLint, GLfloat, GLdouble )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName, getFloat2, getFloat4, getDouble2, getDouble4 )

--------------------------------------------------------------------------------

#include "HsOpenGLTypes.h"

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

instance Domain GLfloat_ where
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

foreign import CALLCONV unsafe "glMap1f" glMap1f ::
      GLenum
   -> GLfloat -> GLfloat -> GLint -> GLint
   -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glMap2f" glMap2f ::
      GLenum
   -> GLfloat -> GLfloat -> GLint -> GLint
   -> GLfloat -> GLfloat -> GLint -> GLint
   -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glGetMapfv" glGetMapfv ::
   GLenum -> GLenum -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glEvalCoord1f" glEvalCoord1f ::
   GLfloat -> IO ()

foreign import CALLCONV unsafe "glEvalCoord1fv" glEvalCoord1fv ::
   Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glEvalCoord2f" glEvalCoord2f ::
   GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glEvalCoord2fv" glEvalCoord2fv ::
   Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glMapGrid1f" glMapGrid1f ::
   GLint -> GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glMapGrid2f" glMapGrid2f ::
   GLint -> GLfloat -> GLfloat -> GLint -> GLfloat -> GLfloat -> IO ()

--------------------------------------------------------------------------------

instance Domain GLdouble_ where
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

foreign import CALLCONV unsafe "glMap1d" glMap1d ::
      GLenum
   -> GLdouble -> GLdouble -> GLint -> GLint
   -> Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glMap2d" glMap2d ::
      GLenum
   -> GLdouble -> GLdouble -> GLint -> GLint
   -> GLdouble -> GLdouble -> GLint -> GLint
   -> Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glGetMapdv" glGetMapdv ::
   GLenum -> GLenum -> Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glEvalCoord1d" glEvalCoord1d ::
   GLdouble -> IO ()

foreign import CALLCONV unsafe "glEvalCoord1dv" glEvalCoord1dv ::
   Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glEvalCoord2d" glEvalCoord2d ::
   GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glEvalCoord2dv" glEvalCoord2dv ::
   Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glMapGrid1d" glMapGrid1d ::
   GLint -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glMapGrid2d" glMapGrid2d ::
   GLint -> GLdouble -> GLdouble -> GLint -> GLdouble -> GLdouble -> IO ()
