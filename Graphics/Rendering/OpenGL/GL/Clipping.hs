--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Clipping
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 2.11 (Clipping) of the OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Clipping (
   ClipPlaneName(..), clipPlane, maxClipPlanes
) where

import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr, castPtr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLsizei, GLdouble )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapClipPlane), makeStateVarMaybe )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Plane(..) )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek1 )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetClipPlane,GetMaxClipPlanes),
   clipPlaneIndexToEnum, getDoublev, getSizei1 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar )

--------------------------------------------------------------------------------

newtype ClipPlaneName = ClipPlaneName GLsizei
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

clipPlane :: ClipPlaneName -> StateVar (Maybe (Plane GLdouble))
clipPlane (ClipPlaneName i) =
   makeStateVarMaybe
      (return (CapClipPlane i))
      (alloca $ \buf -> do
          getDoublev (GetClipPlane i) (castPtr buf)
          peek1 id (buf :: Ptr (Plane GLdouble)))
      (\plane -> with plane $ glClipPlane (clipPlaneIndexToEnum i))

foreign import CALLCONV unsafe "glClipPlane" glClipPlane ::
   GLenum -> Ptr (Plane GLdouble) -> IO ()

--------------------------------------------------------------------------------

maxClipPlanes :: GettableStateVar GLsizei
maxClipPlanes = makeGettableStateVar (getSizei1 id GetMaxClipPlanes)
