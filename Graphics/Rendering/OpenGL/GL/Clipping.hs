--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Clipping
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 2.12 (Clipping) of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Clipping (
   ClipPlaneName(..), clipPlane, maxClipPlanes
) where

import Foreign.Marshal.Utils
import Foreign.Ptr
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility ( glClipPlane )
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

newtype ClipPlaneName = ClipPlaneName GLsizei
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

clipPlane :: ClipPlaneName -> StateVar (Maybe (Plane GLdouble))
clipPlane (ClipPlaneName i) =
   makeStateVarMaybe
      (return (CapClipPlane i))
--      (alloca $ \buf -> do
--          getDoublev (GetClipPlane i) (castPtr buf)
--          peek1 id (buf :: Ptr (Plane GLdouble)))
      (getDouble4 Plane (GetClipPlane i))
      (\plane -> maybe recordInvalidEnum (with plane . glClipPlane_)
                       (clipPlaneIndexToEnum i))

glClipPlane_ :: GLenum -> Ptr (Plane GLdouble) -> IO ()
glClipPlane_ plane ptr = glClipPlane plane (castPtr ptr)

--------------------------------------------------------------------------------

maxClipPlanes :: GettableStateVar GLsizei
maxClipPlanes = makeGettableStateVar (getSizei1 id GetMaxClipPlanes)
