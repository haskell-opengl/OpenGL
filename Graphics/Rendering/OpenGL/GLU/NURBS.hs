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
   withNURBSObj,
   checkForError,
   nurbsBeginEndCurve, gluNurbsCurve,
   nurbsBeginEndSurface, gluNurbsSurface,
   nurbsBeginEndTrim, gluPwlCurve,
   NURBSProperty(..), setNURBSProperty,
   DisplayMode'(..), SamplingMethod(..), NURBSMode(..)
) where

import Control.Monad ( unless )
import Foreign.Ptr ( Ptr, nullPtr, FunPtr, freeHaskellFunPtr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLint, GLfloat )
import Graphics.Rendering.OpenGL.GL.Exception ( bracket, bracket_ )
import Graphics.Rendering.OpenGL.GL.GLboolean ( marshalGLboolean )
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal (
   recordErrorCode, recordOutOfMemory )

--------------------------------------------------------------------------------

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
-- chapter 7.1: The NURBS Object

-- an opaque pointer to a NURBS object
newtype NURBSObj = NURBSObj (Ptr NURBSObj)

isNullNURBSObj :: NURBSObj -> Bool
isNullNURBSObj (NURBSObj ptr) = ptr == nullPtr

withNURBSObj :: a -> (NURBSObj -> IO a) -> IO a
withNURBSObj failureValue action =
   bracket gluNewNurbsRenderer safeDeleteNurbsRenderer
           (\nurbsObj -> if isNullNURBSObj nurbsObj
                            then do recordOutOfMemory
                                    return failureValue
                            else action nurbsObj)

foreign import CALLCONV safe "gluNewNurbsRenderer"
   gluNewNurbsRenderer :: IO NURBSObj

safeDeleteNurbsRenderer :: NURBSObj -> IO ()
safeDeleteNurbsRenderer nurbsObj =
   unless (isNullNURBSObj nurbsObj) $ gluDeleteNurbsRenderer nurbsObj

foreign import CALLCONV safe "gluDeleteNurbsRenderer"
   gluDeleteNurbsRenderer :: NURBSObj -> IO ()

--------------------------------------------------------------------------------
-- chapter 7.2: Callbacks (error)

type ErrorCallback = GLenum -> IO ()

withErrorCallback :: NURBSObj -> ErrorCallback -> IO a -> IO a
withErrorCallback nurbsObj errorCallback action =
   bracket (makeErrorCallback errorCallback)
           freeHaskellFunPtr $ \callbackPtr -> do
      setErrorCallback nurbsObj (marshalNURBSCallback Error) callbackPtr
      action

foreign import ccall "wrapper" makeErrorCallback ::
   ErrorCallback -> IO (FunPtr ErrorCallback)

foreign import CALLCONV safe "gluNurbsCallback"
   setErrorCallback :: NURBSObj -> GLenum -> FunPtr ErrorCallback -> IO ()

checkForError :: NURBSObj -> IO a -> IO a
checkForError nurbsObj = withErrorCallback nurbsObj recordErrorCode

--------------------------------------------------------------------------------
-- chapter 7.3: NURBS Curves

nurbsBeginEndCurve :: NURBSObj -> IO a -> IO a
nurbsBeginEndCurve nurbsObj =
   bracket_ (gluBeginCurve nurbsObj) (gluEndCurve nurbsObj)

foreign import CALLCONV safe "gluBeginCurve"
   gluBeginCurve :: NURBSObj -> IO ()

-- GLAPI void GLAPIENTRY gluNurbsCurve (GLUnurbs* nurb, GLint knotCount, GLfloat* knots, GLint stride, GLfloat* control, GLint order, GLenum type);
foreign import CALLCONV safe "gluNurbsCurve"
   gluNurbsCurve :: NURBSObj -> GLint -> Ptr GLfloat -> GLint -> Ptr GLfloat -> GLint -> GLenum -> IO ()

foreign import CALLCONV safe "gluEndCurve"
   gluEndCurve :: NURBSObj -> IO ()

--------------------------------------------------------------------------------
-- chapter 7.4: NURBS Surfaces

nurbsBeginEndSurface :: NURBSObj -> IO a -> IO a
nurbsBeginEndSurface nurbsObj =
   bracket_ (gluBeginSurface nurbsObj) (gluEndSurface nurbsObj)

foreign import CALLCONV safe "gluBeginSurface"
   gluBeginSurface :: NURBSObj -> IO ()

-- GLAPI void GLAPIENTRY gluNurbsSurface (GLUnurbs* nurb, GLint sKnotCount, GLfloat* sKnots, GLint tKnotCount, GLfloat* tKnots, GLint sStride, GLint tStride, GLfloat* control, GLint sOrder, GLint tOrder, GLenum type);
foreign import CALLCONV safe "gluNurbsSurface"
   gluNurbsSurface :: NURBSObj -> GLint -> Ptr GLfloat -> GLint -> Ptr GLfloat -> GLint -> GLint -> Ptr GLfloat -> GLint -> GLint -> GLenum -> IO ()

foreign import CALLCONV safe "gluEndSurface"
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

nurbsBeginEndTrim :: NURBSObj -> IO a -> IO a
nurbsBeginEndTrim nurbsObj =
   bracket_ (gluBeginTrim nurbsObj) (gluEndTrim nurbsObj)

foreign import CALLCONV safe "gluBeginTrim"
   gluBeginTrim :: NURBSObj -> IO ()

-- GLAPI void GLAPIENTRY gluPwlCurve (GLUnurbs* nurb, GLint count, GLfloat* data, GLint stride, GLenum type);
foreign import CALLCONV safe "gluPwlCurve"
   gluPwlCurve :: NURBSObj -> GLint -> Ptr GLfloat -> GLint -> GLenum -> IO ()

foreign import CALLCONV safe "gluEndTrim"
   gluEndTrim :: NURBSObj -> IO ()

--------------------------------------------------------------------------------
-- chapter 7.6: NURBS Properties

data NURBSProperty' =
     AutoLoadMatrix'
   | Culling'
   | ParametricTolerance'
   | SamplingTolerance'
   | DisplayMode'
   | SamplingMethod'
   | UStep'
   | VStep'
   | NURBSMode'

marshalNURBSProperty' :: NURBSProperty' -> GLenum
marshalNURBSProperty' x = case x of
   AutoLoadMatrix' -> 100200
   Culling' -> 100201
   ParametricTolerance' -> 100202
   SamplingTolerance' -> 100203
   DisplayMode' -> 100204
   SamplingMethod' -> 100205
   UStep' -> 100206
   VStep' -> 100207
   NURBSMode' -> 100160

--------------------------------------------------------------------------------

data NURBSProperty =
     AutoLoadMatrix Bool
   | Culling Bool
   | ParametricTolerance GLfloat
   | SamplingTolerance GLfloat
   | DisplayMode DisplayMode'
   | SamplingMethod SamplingMethod
   | UStep GLfloat
   | VStep GLfloat
   | NURBSMode NURBSMode
   deriving ( Eq, Ord, Show )

setNURBSProperty :: NURBSObj -> NURBSProperty -> IO ()
setNURBSProperty nurbsObj x = case x of
   AutoLoadMatrix a -> nurbsProperty AutoLoadMatrix' (marshalGLboolean a)
   Culling c -> nurbsProperty Culling' (marshalGLboolean c)
   ParametricTolerance p -> nurbsProperty ParametricTolerance' p
   SamplingTolerance s -> nurbsProperty SamplingTolerance' s
   DisplayMode d -> nurbsProperty DisplayMode' (marshalDisplayMode' d)
   SamplingMethod m -> nurbsProperty SamplingMethod' (marshalSamplingMethod m)
   UStep u -> nurbsProperty UStep' u
   VStep v -> nurbsProperty VStep' v
   NURBSMode m -> nurbsProperty NURBSMode' (marshalNURBSMode m)
   where nurbsProperty = gluNurbsProperty nurbsObj . marshalNURBSProperty'

--------------------------------------------------------------------------------

data DisplayMode' =
     Fill'
   | OutlinePolygon
   | OutlinePatch
   deriving ( Eq, Ord, Show )

marshalDisplayMode' :: DisplayMode' -> GLfloat
marshalDisplayMode' x = case x of
   Fill' -> 100012
   OutlinePolygon -> 100240
   OutlinePatch -> 100241

unmarshalDisplayMode' :: GLfloat -> DisplayMode'
unmarshalDisplayMode' x
   | x == 100012 = Fill'
   | x == 100240 = OutlinePolygon
   | x == 100241 = OutlinePatch
   | otherwise = error ("unmarshalDisplayMode': illegal value " ++ show x)

--------------------------------------------------------------------------------

data SamplingMethod =
     ObjectParametricError
   | ObjectPathLength
   | PathLength
   | ParametricError
   | DomainDistance
   deriving ( Eq, Ord, Show )

marshalSamplingMethod :: SamplingMethod -> GLfloat
marshalSamplingMethod x = case x of
   ObjectParametricError -> 100208
   ObjectPathLength -> 100209
   PathLength -> 100215
   ParametricError -> 100216
   DomainDistance -> 100217

unmarshalSamplingMethod :: GLfloat -> SamplingMethod
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
   deriving ( Eq, Ord, Show )

marshalNURBSMode :: NURBSMode -> GLfloat
marshalNURBSMode x = case x of
   NurbsTessellator -> 100161
   NurbsRenderer -> 100162

unmarshalNURBSMode :: GLfloat -> NURBSMode
unmarshalNURBSMode x
   | x == 100161 = NurbsTessellator
   | x == 100162 = NurbsRenderer
   | otherwise = error ("unmarshalNURBSMode: illegal value " ++ show x)

--------------------------------------------------------------------------------

foreign import CALLCONV safe "gluNurbsProperty"
   gluNurbsProperty :: NURBSObj -> GLenum -> GLfloat -> IO ()

-- GLAPI void GLAPIENTRY gluGetNurbsProperty (GLUnurbs* nurb, GLenum property, GLfloat* data);
foreign import CALLCONV safe "gluGetNurbsProperty"
   gluGetNurbsProperty :: NURBSObj -> GLenum -> Ptr GLfloat -> IO ()

--------------------------------------------------------------------------------

-- GLAPI void GLAPIENTRY gluLoadSamplingMatrices (GLUnurbs* nurb, const GLfloat* model, const GLfloat* perspective, const GLint* view);
foreign import CALLCONV safe "gluLoadSamplingMatrices"
   gluLoadSamplingMatrices :: NURBSObj -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLint -> IO ()
