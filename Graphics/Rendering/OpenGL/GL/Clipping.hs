--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Clipping
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module corresponds to section 2.11 (Clipping) of the OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Clipping (
   ClipPlaneName(..), clipPlane
) where

import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr, castPtr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLsizei, GLdouble )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapClipPlane), makeCapability )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Plane(..) )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek1 )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetClipPlane), clipPlaneIndexToEnum,getDoublev )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasGetter(get), HasSetter(($=)), StateVar, makeStateVar )

--------------------------------------------------------------------------------

newtype ClipPlaneName = ClipPlaneName GLsizei
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

clipPlane :: ClipPlaneName -> StateVar (Maybe (Plane GLdouble))
clipPlane cpName = makeStateVar (getClipPlane cpName) (setClipPlane cpName)

--------------------------------------------------------------------------------

getClipPlane :: ClipPlaneName -> IO (Maybe (Plane GLdouble))
getClipPlane cpName@(ClipPlaneName i) = do
   enabled <- get (clipPlaneStatus cpName)
   if enabled
      then alloca $ \buf -> do
           getDoublev (GetClipPlane i) (castPtr buf)
           peek1 Just (buf :: Ptr (Plane GLdouble))
      else return Nothing

--------------------------------------------------------------------------------

setClipPlane :: ClipPlaneName -> Maybe (Plane GLdouble) -> IO ()
setClipPlane cpName Nothing =
   clipPlaneStatus cpName $= False
setClipPlane cpName@(ClipPlaneName i) (Just plane) = do
   clipPlaneStatus cpName $= True
   with plane $ glClipPlane (clipPlaneIndexToEnum i)

foreign import CALLCONV unsafe "glClipPlane" glClipPlane ::
   GLenum -> Ptr (Plane GLdouble) -> IO ()

--------------------------------------------------------------------------------

clipPlaneStatus :: ClipPlaneName -> StateVar Bool
clipPlaneStatus (ClipPlaneName i) = makeCapability (CapClipPlane i)
