--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Evaluators
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 5.1 (Evaluators) of the OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Evaluators (
   maxEvalOrder, autoNormal,
   EvalCoord1(..), EvalCoord2(..),
   map1Gridf, map1Gridd, map2Gridf, map2Gridd,
   evalPoint1, evalPoint2, evalMesh1, evalMesh2
) where

import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapAutoNormal), makeCapability )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLint, GLsizei, GLfloat, GLdouble, Capability )
import Graphics.Rendering.OpenGL.GL.PrimitiveMode ( marshalPrimitiveMode )
import Graphics.Rendering.OpenGL.GL.BeginEnd ( PrimitiveMode )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetMaxEvalOrder,
            GetMap1GridSegments,GetMap1GridDomain,
            GetMap2GridSegments,GetMap2GridDomain),
   getSizei1, getInteger1, getInteger2, getFloat2, getFloat4, getDouble2,
   getDouble4 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )

--------------------------------------------------------------------------------

#include "HsOpenGLTypes.h"

--------------------------------------------------------------------------------

maxEvalOrder :: GettableStateVar GLsizei
maxEvalOrder = makeGettableStateVar (getSizei1 id GetMaxEvalOrder)

autoNormal :: StateVar Capability
autoNormal = makeCapability CapAutoNormal

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glMap1f" glMap1f ::
      GLenum
   -> GLfloat -> GLfloat -> GLint -> GLint
   -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glMap1d" glMap1d ::
      GLenum
   -> GLdouble -> GLdouble -> GLint -> GLint
   -> Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glMap2f" glMap2f ::
      GLenum
   -> GLfloat -> GLfloat -> GLint -> GLint
   -> GLfloat -> GLfloat -> GLint -> GLint
   -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glMap2d" glMap2d ::
      GLenum
   -> GLdouble -> GLdouble -> GLint -> GLint
   -> GLdouble -> GLdouble -> GLint -> GLint
   -> Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glGetMapdv" glGetMapdv ::
   GLenum -> GLenum -> Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glGetMapfv" glGetMapfv ::
   GLenum -> GLenum -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glGetMapiv" glGetMapiv ::
   GLenum -> GLenum -> Ptr GLint -> IO ()

--------------------------------------------------------------------------------

class EvalCoord1 c where
    evalCoord1  :: c -> IO ()
    evalCoord1v :: Ptr c -> IO ()

instance EvalCoord1 GLfloat_ where
    evalCoord1  = glEvalCoord1f
    evalCoord1v = glEvalCoord1fv

foreign import CALLCONV unsafe "glEvalCoord1f" glEvalCoord1f ::
   GLfloat -> IO ()

foreign import CALLCONV unsafe "glEvalCoord1fv" glEvalCoord1fv ::
   Ptr GLfloat -> IO ()

instance EvalCoord1 GLdouble_ where
    evalCoord1  = glEvalCoord1d
    evalCoord1v = glEvalCoord1dv

foreign import CALLCONV unsafe "glEvalCoord1d" glEvalCoord1d ::
   GLdouble -> IO ()

foreign import CALLCONV unsafe "glEvalCoord1dv" glEvalCoord1dv ::
   Ptr GLdouble -> IO ()

--------------------------------------------------------------------------------

class EvalCoord2 c where
    evalCoord2  :: c -> c -> IO ()
    evalCoord2v :: Ptr c -> IO ()

instance EvalCoord2 GLfloat_ where
    evalCoord2  = glEvalCoord2f
    evalCoord2v = glEvalCoord2fv

foreign import CALLCONV unsafe "glEvalCoord2f" glEvalCoord2f ::
   GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glEvalCoord2fv" glEvalCoord2fv ::
   Ptr GLfloat -> IO ()

instance EvalCoord2 GLdouble_ where
    evalCoord2  = glEvalCoord2d
    evalCoord2v = glEvalCoord2dv

foreign import CALLCONV unsafe "glEvalCoord2d" glEvalCoord2d ::
   GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glEvalCoord2dv" glEvalCoord2dv ::
   Ptr GLdouble -> IO ()

--------------------------------------------------------------------------------

map1Gridf :: StateVar (GLint, GLfloat, GLfloat)
map1Gridf =
   makeStateVar
      (do n <- getInteger1 id GetMap1GridSegments
          (u1, u2) <- getFloat2 (,) GetMap1GridDomain
          return (n, u1, u2))
      (\(n, u1, u2) -> glMapGrid1f n u1 u2)

foreign import CALLCONV unsafe "glMapGrid1f" glMapGrid1f ::
   GLint -> GLfloat -> GLfloat -> IO ()

map1Gridd :: StateVar (GLint, GLdouble, GLdouble)
map1Gridd =
   makeStateVar
      (do un <- getInteger1 id GetMap1GridSegments
          (u1, u2) <- getDouble2 (,) GetMap1GridDomain
          return (un, u1, u2))
      (\(un, u1, u2) -> glMapGrid1d un u1 u2)

foreign import CALLCONV unsafe "glMapGrid1d" glMapGrid1d ::
   GLint -> GLdouble -> GLdouble -> IO ()

map2Gridf :: StateVar (GLint, GLfloat, GLfloat, GLint, GLfloat, GLfloat)
map2Gridf =
   makeStateVar
      (do (un, vn) <- getInteger2 (,) GetMap2GridSegments
          (u1, u2, v1, v2) <- getFloat4 (,,,) GetMap2GridDomain
          return (un, u1, u2, vn, v1, v2))
      (\(un, u1, u2, vn, v1, v2) -> glMapGrid2f un u1 u2 vn v1 v2)

foreign import CALLCONV unsafe "glMapGrid2f" glMapGrid2f ::
   GLint -> GLfloat -> GLfloat -> GLint -> GLfloat -> GLfloat -> IO ()

map2Gridd :: StateVar (GLint, GLdouble, GLdouble, GLint, GLdouble, GLdouble)
map2Gridd =
   makeStateVar
      (do (un, vn) <- getInteger2 (,) GetMap2GridSegments
          (u1, u2, v1, v2) <- getDouble4 (,,,) GetMap2GridDomain
          return (un, u1, u2, vn, v1, v2))
      (\(un, u1, u2, vn, v1, v2) -> glMapGrid2d un u1 u2 vn v1 v2)

foreign import CALLCONV unsafe "glMapGrid2d" glMapGrid2d ::
   GLint -> GLdouble -> GLdouble -> GLint -> GLdouble -> GLdouble -> IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glEvalPoint1" evalPoint1 ::
   GLint -> IO ()

foreign import CALLCONV unsafe "glEvalPoint2" evalPoint2 ::
   GLint -> GLint -> IO ()

evalMesh1 :: PrimitiveMode -> GLint -> GLint -> IO ()
evalMesh1 = glEvalMesh1 . marshalPrimitiveMode

foreign import CALLCONV unsafe "glEvalMesh1" glEvalMesh1 ::
   GLenum -> GLint -> GLint -> IO ()

evalMesh2 :: PrimitiveMode -> GLint -> GLint -> GLint -> GLint -> IO ()
evalMesh2 = glEvalMesh2 . marshalPrimitiveMode

foreign import CALLCONV unsafe "glEvalMesh2" glEvalMesh2 ::
   GLenum -> GLint -> GLint -> GLint -> GLint -> IO ()
