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
   lineWidth, lineSmooth, lineStipple
) where

import Control.Monad ( liftM2 )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLint, GLushort, GLfloat )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapLineSmooth,CapLineStipple), makeCapability )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetLineWidth,GetLineStippleRepeat,GetLineStipplePattern),
   getInteger1, getFloat1 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   StateVar, makeStateVar, makeStateVarMaybe )

--------------------------------------------------------------------------------

lineWidth :: StateVar GLfloat
lineWidth = makeStateVar (getFloat1 id GetLineWidth) glLineWidth

foreign import CALLCONV unsafe "glLineWidth" glLineWidth :: GLfloat -> IO ()

--------------------------------------------------------------------------------

lineSmooth :: StateVar Bool
lineSmooth = makeCapability CapLineSmooth

--------------------------------------------------------------------------------

lineStipple :: StateVar (Maybe (GLint, GLushort))
lineStipple =
   makeStateVarMaybe
      (makeCapability CapLineStipple)
      (liftM2 (,) (getInteger1 id GetLineStippleRepeat)
                  (getInteger1 fromIntegral GetLineStipplePattern))
      (uncurry glLineStipple)

foreign import CALLCONV unsafe "glLineStipple" glLineStipple ::
   GLint -> GLushort -> IO ()
