--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.LineSegments
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 3.4 (LineSegments) of the OpenGL 1.4
-- specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.LineSegments (
   lineWidth, aliasedLineWidthRange, smoothLineWidthRange,
   smoothLineWidthGranularity, lineSmooth, lineStipple
) where

import Control.Monad ( liftM2 )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapLineSmooth,CapLineStipple), makeCapability, makeStateVarMaybe )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLint, GLushort, GLfloat, Capability )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetLineWidth,GetAliasedLineWidthRange,GetSmoothLineWidthRange,
            GetSmoothLineWidthGranularity,GetLineStippleRepeat,
            GetLineStipplePattern),
   getInteger1, getFloat1, getFloat2 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )

--------------------------------------------------------------------------------

lineWidth :: StateVar GLfloat
lineWidth = makeStateVar (getFloat1 id GetLineWidth) glLineWidth

foreign import CALLCONV unsafe "glLineWidth" glLineWidth :: GLfloat -> IO ()

--------------------------------------------------------------------------------

lineSmooth :: StateVar Capability
lineSmooth = makeCapability CapLineSmooth

aliasedLineWidthRange :: GettableStateVar (GLfloat, GLfloat)
aliasedLineWidthRange =
   makeGettableStateVar $ getFloat2 (,) GetAliasedLineWidthRange

smoothLineWidthRange :: GettableStateVar (GLfloat, GLfloat)
smoothLineWidthRange =
   makeGettableStateVar $ getFloat2 (,) GetSmoothLineWidthRange

smoothLineWidthGranularity :: GettableStateVar (GLfloat, GLfloat)
smoothLineWidthGranularity =
   makeGettableStateVar $ getFloat2 (,) GetSmoothLineWidthGranularity

--------------------------------------------------------------------------------

lineStipple :: StateVar (Maybe (GLint, GLushort))
lineStipple =
   makeStateVarMaybe
      (return CapLineStipple)
      (liftM2 (,) (getInteger1 id GetLineStippleRepeat)
                  (getInteger1 fromIntegral GetLineStipplePattern))
      (uncurry glLineStipple)

foreign import CALLCONV unsafe "glLineStipple" glLineStipple ::
   GLint -> GLushort -> IO ()
