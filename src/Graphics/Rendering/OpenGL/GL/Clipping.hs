--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Clipping
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 13.5 (Primitive Clipping) of the OpenGL
-- 4.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Clipping (
   ClipPlaneName(..), clipPlane, maxClipPlanes
) where

import Data.StateVar
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

newtype ClipPlaneName = ClipPlaneName GLsizei
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

clipPlane :: ClipPlaneName -> StateVar (Maybe (Plane GLdouble))
clipPlane name =
   makeStateVarMaybe
      (return $ nameToCap name)
      (alloca $ \buf -> do
          clipPlaneAction name $ flip glGetClipPlane $ castPtr buf
          peek buf)
      (\plane -> with plane $ clipPlaneAction name . flip glClipPlane . castPtr)

nameToCap :: ClipPlaneName -> EnableCap
nameToCap (ClipPlaneName i) = CapClipPlane i

clipPlaneAction :: ClipPlaneName -> (GLenum -> IO ()) -> IO ()
clipPlaneAction (ClipPlaneName i) act =
   maybe recordInvalidEnum act (clipPlaneIndexToEnum i)

--------------------------------------------------------------------------------

maxClipPlanes :: GettableStateVar GLsizei
maxClipPlanes = makeGettableStateVar (getSizei1 id GetMaxClipPlanes)
