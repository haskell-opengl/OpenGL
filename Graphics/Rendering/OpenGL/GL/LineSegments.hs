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

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLint, GLushort, GLfloat )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapLineSmooth,CapLineStipple), makeCapability )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetLineWidth,GetLineStippleRepeat,GetLineStipplePattern),
   getInteger1, getFloat1 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasGetter(get), HasSetter(($=)), StateVar, makeStateVar )

--------------------------------------------------------------------------------

lineWidth :: StateVar GLfloat
lineWidth = makeStateVar (getFloat1 id GetLineWidth) glLineWidth

foreign import CALLCONV unsafe "glLineWidth" glLineWidth :: GLfloat -> IO ()

--------------------------------------------------------------------------------

lineSmooth :: StateVar Bool
lineSmooth = makeCapability CapLineSmooth

--------------------------------------------------------------------------------

lineStipple :: StateVar (Maybe (GLint, GLushort))
lineStipple = makeStateVar getLineStipple setLineStipple

getLineStipple :: IO (Maybe (GLint, GLushort))
getLineStipple = do
   enabled <- get lineStippleEnabled
   if enabled
      then do factor <- getInteger1 id GetLineStippleRepeat
              pattern <- getInteger1 fromIntegral GetLineStipplePattern
              return $ Just (factor, pattern)
      else return Nothing

setLineStipple :: Maybe (GLint, GLushort) -> IO ()
setLineStipple Nothing = lineStippleEnabled $= False
setLineStipple (Just (factor, pattern)) = do
   lineStippleEnabled $= True
   glLineStipple factor pattern

foreign import CALLCONV unsafe "glLineStipple" glLineStipple ::
   GLint -> GLushort -> IO ()

lineStippleEnabled :: StateVar Bool
lineStippleEnabled = makeCapability CapLineStipple
