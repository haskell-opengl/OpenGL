--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.NURBS
-- Copyright   :  (c) Sven Panne 2002-2004
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to chapter 7 (NURBS) of the GLU specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GLU.NURBS (
) where

import Foreign.Ptr ( Ptr, FunPtr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLint, GLfloat )

--------------------------------------------------------------------------------
-- chapter 7.1: The NURBS Object

-- 'Char' is a fake here, any marshalable type would do
newtype NURBSObj = NURBSObj (Ptr Char)
   deriving ( Eq )

-- GLAPI GLUnurbs* GLAPIENTRY gluNewNurbsRenderer (void);
foreign import CALLCONV unsafe "gluNewNurbsRenderer"
   gluNewNurbsRenderer :: IO NURBSObj

-- GLAPI void GLAPIENTRY gluDeleteNurbsRenderer (GLUnurbs* nurb);
foreign import CALLCONV unsafe "gluDeleteNurbsRenderer"
   gluDeleteNurbsRenderer :: NURBSObj -> IO ()

--------------------------------------------------------------------------------
-- chapter 7.2: Callbacks

data NURBSCallback =
     Error
   | Begin
   | Vertex
   | Normal
   | Color
   | TextureCoord
   | End
   | BeginData
   | VertexData
   | NormalData
   | ColorData
   | TextureCoordData
   | EndData

marshalNURBSCallback :: NURBSCallback -> GLenum
marshalNURBSCallback x = case x of
   Error -> 100103
   Begin -> 100164
   Vertex -> 100165
   Normal -> 100166
   Color -> 100167
   TextureCoord -> 100168
   End -> 100169
   BeginData -> 100170
   VertexData -> 100171
   NormalData -> 100172
   ColorData -> 100173
   TextureCoordData -> 100174
   EndData -> 100175

--------------------------------------------------------------------------------

-- GLAPI void GLAPIENTRY gluNurbsCallback (GLUnurbs* nurb, GLenum which, _GLUfuncptr CallBackFunc);
foreign import CALLCONV unsafe "gluNurbsCallback"
   gluNurbsCallback :: NURBSObj -> GLenum -> FunPtr a -> IO ()

-- GLAPI void GLAPIENTRY gluNurbsCallbackData (GLUnurbs* nurb, GLvoid* userData);
foreign import CALLCONV unsafe "gluNurbsCallbackData"
   gluNurbsCallbackData :: NURBSObj -> Ptr a -> IO ()

--------------------------------------------------------------------------------
-- chapter 7.3: NURBS Curves

-- GLAPI void GLAPIENTRY gluBeginCurve (GLUnurbs* nurb);
foreign import CALLCONV unsafe "gluBeginCurve"
   gluBeginCurve :: NURBSObj -> IO ()

-- GLAPI void GLAPIENTRY gluNurbsCurve (GLUnurbs* nurb, GLint knotCount, GLfloat* knots, GLint stride, GLfloat* control, GLint order, GLenum type);
foreign import CALLCONV unsafe "gluNurbsCurve"
   gluNurbsCurve :: NURBSObj -> GLint -> Ptr GLfloat -> GLint -> Ptr GLfloat -> GLint -> GLenum -> IO ()

-- GLAPI void GLAPIENTRY gluEndCurve (GLUnurbs* nurb);
foreign import CALLCONV unsafe "gluEndCurve"
   gluEndCurve :: NURBSObj -> IO ()

--------------------------------------------------------------------------------
-- chapter 7.4: NURBS Surfaces

-- GLAPI void GLAPIENTRY gluBeginSurface (GLUnurbs* nurb);
foreign import CALLCONV unsafe "gluBeginSurface"
   gluBeginSurface :: NURBSObj -> IO ()

-- GLAPI void GLAPIENTRY gluNurbsSurface (GLUnurbs* nurb, GLint sKnotCount, GLfloat* sKnots, GLint tKnotCount, GLfloat* tKnots, GLint sStride, GLint tStride, GLfloat* control, GLint sOrder, GLint tOrder, GLenum type);
foreign import CALLCONV unsafe "gluNurbsSurface"
   gluNurbsSurface :: NURBSObj -> GLint -> Ptr GLfloat -> GLint -> Ptr GLfloat -> GLint -> GLint -> Ptr GLfloat -> GLint -> GLint -> GLenum -> IO ()

-- GLAPI void GLAPIENTRY gluEndSurface (GLUnurbs* nurb);
foreign import CALLCONV unsafe "gluEndSurface"
   gluEndSurface :: NURBSObj -> IO ()

--------------------------------------------------------------------------------
-- chapter 7.5: Trimming

data NURBSTrim =
     Map1Trim2
   | Map1Trim3

marshalNURBSTrim :: NURBSTrim -> GLenum
marshalNURBSTrim x = case x of
   Map1Trim2 -> 100210
   Map1Trim3 -> 100211

-- GLAPI void GLAPIENTRY gluBeginTrim (GLUnurbs* nurb);
foreign import CALLCONV unsafe "gluBeginTrim"
   gluBeginTrim :: NURBSObj -> IO ()

-- GLAPI void GLAPIENTRY gluPwlCurve (GLUnurbs* nurb, GLint count, GLfloat* data, GLint stride, GLenum type);
foreign import CALLCONV unsafe "gluPwlCurve"
   gluPwlCurve :: NURBSObj -> GLint -> Ptr GLfloat -> GLint -> GLenum -> IO ()

-- GLAPI void GLAPIENTRY gluEndTrim (GLUnurbs* nurb);
foreign import CALLCONV unsafe "gluEndTrim"
   gluEndTrim :: NURBSObj -> IO ()

--------------------------------------------------------------------------------
-- chapter 7.6: NURBS Properties

data NURBSProperty =
     AutoLoadMatrix
   | Culling
   | ParametricTolerance
   | SamplingTolerance
   | DisplayMode
   | SamplingMethod
   | UStep
   | VStep
   | NurbsMode

marshalNURBSProperty :: NURBSProperty -> GLenum
marshalNURBSProperty x = case x of
   AutoLoadMatrix -> 100200
   Culling -> 100201
   ParametricTolerance -> 100202
   SamplingTolerance -> 100203
   DisplayMode -> 100204
   SamplingMethod -> 100205
   UStep -> 100206
   VStep -> 100207
   NurbsMode -> 100160

--------------------------------------------------------------------------------

data DisplayMode =
     Fill
   | OutlinePolygon
   | OutlinePatch

marshalDisplayMode :: DisplayMode -> GLenum
marshalDisplayMode x = case x of
   Fill -> 100012
   OutlinePolygon -> 100240
   OutlinePatch -> 100241

unmarshalDisplayMode :: GLenum -> DisplayMode
unmarshalDisplayMode x
   | x == 100012 = Fill
   | x == 100240 = OutlinePolygon
   | x == 100241 = OutlinePatch
   | otherwise = error ("unmarshalDisplayMode: illegal value " ++ show x)

--------------------------------------------------------------------------------

data SamplingMethod =
     ObjectParametricError
   | ObjectPathLength
   | PathLength
   | ParametricError
   | DomainDistance

marshalSamplingMethod :: SamplingMethod -> GLenum
marshalSamplingMethod x = case x of
   ObjectParametricError -> 100208
   ObjectPathLength -> 100209
   PathLength -> 100215
   ParametricError -> 100216
   DomainDistance -> 100217

unmarshalSamplingMethod :: GLenum -> SamplingMethod
unmarshalSamplingMethod x
   | x == 100208 = ObjectParametricError
   | x == 100209 = ObjectPathLength
   | x == 100215 = PathLength
   | x == 100216 = ParametricError
   | x == 100217 = DomainDistance
   | otherwise = error ("unmarshalSamplingMethod: illegal value " ++ show x)

--------------------------------------------------------------------------------

data NURBSMode =
     NurbsTessellator
   | NurbsRenderer

marshalNURBSMode :: NURBSMode -> GLenum
marshalNURBSMode x = case x of
   NurbsTessellator -> 100161
   NurbsRenderer -> 100162

unmarshalNURBSMode :: GLenum -> NURBSMode
unmarshalNURBSMode x
   | x == 100161 = NurbsTessellator
   | x == 100162 = NurbsRenderer
   | otherwise = error ("unmarshalNURBSMode: illegal value " ++ show x)

--------------------------------------------------------------------------------

-- GLAPI void GLAPIENTRY gluNurbsProperty (GLUnurbs* nurb, GLenum property, GLfloat value);
foreign import CALLCONV unsafe "gluNurbsProperty"
   gluNurbsProperty :: NURBSObj -> GLenum -> GLfloat -> IO ()

-- GLAPI void GLAPIENTRY gluGetNurbsProperty (GLUnurbs* nurb, GLenum property, GLfloat* data);
foreign import CALLCONV unsafe "gluGetNurbsProperty"
   gluGetNurbsProperty :: NURBSObj -> GLenum -> Ptr GLfloat -> IO ()

-- GLAPI void GLAPIENTRY gluLoadSamplingMatrices (GLUnurbs* nurb, const GLfloat* model, const GLfloat* perspective, const GLint* view);
foreign import CALLCONV unsafe "gluLoadSamplingMatrices"
   gluLoadSamplingMatrices :: NURBSObj -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLint -> IO ()
