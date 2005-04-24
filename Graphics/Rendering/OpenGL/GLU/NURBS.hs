--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.NURBS
-- Copyright   :  (c) Sven Panne 2002-2005
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
   withBeginCallback, withVertexCallback, withNormalCallback, withColorCallback,
   withEndCallback, checkForError,
   nurbsBeginEndCurve, gluNurbsCurve,
   nurbsBeginEndSurface, gluNurbsSurface,
   nurbsBeginEndTrim, gluPwlCurve,
   NURBSMode(..), setNURBSMode,
   setCulling,
   SamplingMethod(..), setSamplingMethod,
   loadSamplingMatrices,
   DisplayMode'(..), setDisplayMode
) where

import Control.Monad ( unless )
import Foreign.Marshal.Array ( withArray )
import Foreign.Ptr ( Ptr, nullPtr, FunPtr, freeHaskellFunPtr )
import Foreign.Storable ( Storable(..) )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLint, GLfloat, Capability )
import Graphics.Rendering.OpenGL.GL.Capability ( marshalCapability )
import Graphics.Rendering.OpenGL.GL.CoordTrans (
   Position(..), Size(..), MatrixOrder(ColumnMajor), MatrixComponent, Matrix(..) )
import Graphics.Rendering.OpenGL.GL.Exception ( bracket, bracket_ )
import Graphics.Rendering.OpenGL.GL.GLboolean ( marshalGLboolean )
import Graphics.Rendering.OpenGL.GL.PrimitiveMode ( unmarshalPrimitiveMode )
import Graphics.Rendering.OpenGL.GL.BeginEnd ( PrimitiveMode )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Vertex3, Normal3, Color4 )
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
-- chapter 7.2: Callbacks (begin)

type BeginCallback = PrimitiveMode -> IO ()

type BeginCallback' = GLenum -> IO ()

withBeginCallback :: NURBSObj -> BeginCallback -> IO a -> IO a
withBeginCallback nurbsObj beginCallback action =
   bracket (makeBeginCallback (beginCallback . unmarshalPrimitiveMode))
           freeHaskellFunPtr $ \callbackPtr -> do
      setBeginCallback nurbsObj (marshalNURBSCallback Begin) callbackPtr
      action

foreign import CALLCONV "wrapper" makeBeginCallback ::
   BeginCallback' -> IO (FunPtr BeginCallback')

foreign import CALLCONV safe "gluNurbsCallback"
   setBeginCallback :: NURBSObj -> GLenum -> FunPtr BeginCallback' -> IO ()

--------------------------------------------------------------------------------
-- chapter 7.2: Callbacks (vertex)

type VertexCallback = Vertex3 GLfloat -> IO ()

type VertexCallback' = Ptr (Vertex3 GLfloat) -> IO ()

withVertexCallback :: NURBSObj -> VertexCallback -> IO a -> IO a
withVertexCallback nurbsObj vertexCallback action =
   bracket (makeVertexCallback (\p -> peek p >>= vertexCallback))
           freeHaskellFunPtr $ \callbackPtr -> do
      setVertexCallback nurbsObj (marshalNURBSCallback Vertex) callbackPtr
      action

foreign import CALLCONV "wrapper" makeVertexCallback ::
   VertexCallback' -> IO (FunPtr VertexCallback')

foreign import CALLCONV safe "gluNurbsCallback"
   setVertexCallback :: NURBSObj -> GLenum -> FunPtr VertexCallback' -> IO ()

--------------------------------------------------------------------------------
-- chapter 7.2: Callbacks (normal)

type NormalCallback = Normal3 GLfloat -> IO ()

type NormalCallback' = Ptr (Normal3 GLfloat) -> IO ()

withNormalCallback :: NURBSObj -> NormalCallback -> IO a -> IO a
withNormalCallback nurbsObj normalCallback action =
   bracket (makeNormalCallback (\p -> peek p >>= normalCallback))
           freeHaskellFunPtr $ \callbackPtr -> do
      setNormalCallback nurbsObj (marshalNURBSCallback Normal) callbackPtr
      action

foreign import CALLCONV "wrapper" makeNormalCallback ::
   NormalCallback' -> IO (FunPtr NormalCallback')

foreign import CALLCONV safe "gluNurbsCallback"
   setNormalCallback :: NURBSObj -> GLenum -> FunPtr NormalCallback' -> IO ()

--------------------------------------------------------------------------------
-- chapter 7.2: Callbacks (color)

type ColorCallback = Color4 GLfloat -> IO ()

type ColorCallback' = Ptr (Color4 GLfloat) -> IO ()

withColorCallback :: NURBSObj -> ColorCallback -> IO a -> IO a
withColorCallback nurbsObj colorCallback action =
   bracket (makeColorCallback (\p -> peek p >>= colorCallback))
           freeHaskellFunPtr $ \callbackPtr -> do
      setColorCallback nurbsObj (marshalNURBSCallback Color) callbackPtr
      action

foreign import CALLCONV "wrapper" makeColorCallback ::
   ColorCallback' -> IO (FunPtr ColorCallback')

foreign import CALLCONV safe "gluNurbsCallback"
   setColorCallback :: NURBSObj -> GLenum -> FunPtr ColorCallback' -> IO ()

--------------------------------------------------------------------------------
-- chapter 7.2: Callbacks (end)

type EndCallback = IO ()

withEndCallback :: NURBSObj -> EndCallback -> IO a -> IO a
withEndCallback nurbsObj endCallback action =
   bracket (makeEndCallback endCallback)
           freeHaskellFunPtr $ \callbackPtr -> do
      setEndCallback nurbsObj (marshalNURBSCallback End) callbackPtr
      action

foreign import CALLCONV "wrapper" makeEndCallback ::
   EndCallback -> IO (FunPtr EndCallback)

foreign import CALLCONV safe "gluNurbsCallback"
   setEndCallback :: NURBSObj -> GLenum -> FunPtr EndCallback -> IO ()

--------------------------------------------------------------------------------
-- chapter 7.2: Callbacks (error)

type ErrorCallback = GLenum -> IO ()

withErrorCallback :: NURBSObj -> ErrorCallback -> IO a -> IO a
withErrorCallback nurbsObj errorCallback action =
   bracket (makeErrorCallback errorCallback)
           freeHaskellFunPtr $ \callbackPtr -> do
      setErrorCallback nurbsObj (marshalNURBSCallback Error) callbackPtr
      action

foreign import CALLCONV "wrapper" makeErrorCallback ::
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

data NURBSProperty =
     AutoLoadMatrix
   | Culling
   | ParametricTolerance
   | SamplingTolerance
   | DisplayMode'
   | SamplingMethod
   | UStep
   | VStep
   | NURBSMode

marshalNURBSProperty :: NURBSProperty -> GLenum
marshalNURBSProperty x = case x of
   AutoLoadMatrix -> 100200
   Culling -> 100201
   ParametricTolerance -> 100202
   SamplingTolerance -> 100203
   DisplayMode' -> 100204
   SamplingMethod -> 100205
   UStep -> 100206
   VStep -> 100207
   NURBSMode -> 100160

--------------------------------------------------------------------------------

setNURBSProperty :: NURBSObj -> NURBSProperty -> GLfloat -> IO ()
setNURBSProperty nurbsObj = gluNurbsProperty nurbsObj . marshalNURBSProperty

foreign import CALLCONV safe "gluNurbsProperty"
   gluNurbsProperty :: NURBSObj -> GLenum -> GLfloat -> IO ()

--------------------------------------------------------------------------------

data NURBSMode =
     NURBSTessellator
   | NURBSRenderer
   deriving ( Eq, Ord, Show )

marshalNURBSMode :: NURBSMode -> GLfloat
marshalNURBSMode x = case x of
   NURBSTessellator -> 100161
   NURBSRenderer -> 100162

setNURBSMode :: NURBSObj -> NURBSMode -> IO ()
setNURBSMode nurbsObj = setNURBSProperty nurbsObj NURBSMode . marshalNURBSMode

--------------------------------------------------------------------------------

setCulling :: NURBSObj -> Capability -> IO ()
setCulling nurbsObj = setNURBSProperty nurbsObj Culling . fromIntegral . marshalCapability

--------------------------------------------------------------------------------

data SamplingMethod' =
     PathLength'
   | ParametricError'
   | DomainDistance'
   | ObjectPathLength'
   | ObjectParametricError'

marshalSamplingMethod' :: SamplingMethod' -> GLfloat
marshalSamplingMethod' x = case x of
   PathLength' -> 100215
   ParametricError' -> 100216
   DomainDistance' -> 100217
   ObjectPathLength' -> 100209
   ObjectParametricError' -> 100208

setSamplingMethod' :: NURBSObj -> SamplingMethod' -> IO ()
setSamplingMethod' nurbsObj = setNURBSProperty nurbsObj SamplingMethod . marshalSamplingMethod'

--------------------------------------------------------------------------------

data SamplingMethod =
     PathLength GLfloat
   | ParametricError GLfloat
   | DomainDistance GLfloat GLfloat
   | ObjectPathLength GLfloat
   | ObjectParametricError GLfloat
   deriving ( Eq, Ord, Show )

setSamplingMethod :: NURBSObj -> SamplingMethod -> IO ()
setSamplingMethod nurbsObj x = case x of
   PathLength s -> do
      setNURBSProperty nurbsObj SamplingTolerance s
      setSamplingMethod' nurbsObj PathLength'
   ParametricError p -> do
      setNURBSProperty nurbsObj ParametricTolerance p
      setSamplingMethod' nurbsObj ParametricError'
   DomainDistance u v -> do
      setNURBSProperty nurbsObj UStep u
      setNURBSProperty nurbsObj VStep v
      setSamplingMethod' nurbsObj DomainDistance'
   ObjectPathLength s -> do
      setNURBSProperty nurbsObj SamplingTolerance s
      setSamplingMethod' nurbsObj ObjectPathLength'
   ObjectParametricError p -> do
      setNURBSProperty nurbsObj ParametricTolerance p
      setSamplingMethod' nurbsObj ObjectParametricError'

--------------------------------------------------------------------------------

setAutoLoadMatrix :: NURBSObj -> Bool -> IO ()
setAutoLoadMatrix nurbsObj = setNURBSProperty nurbsObj AutoLoadMatrix . marshalGLboolean

loadSamplingMatrices :: (Matrix m1, Matrix m2) => NURBSObj -> Maybe (m1 GLfloat, m2 GLfloat, (Position, Size)) -> IO ()
loadSamplingMatrices nurbsObj =
   maybe
      (setAutoLoadMatrix nurbsObj True)
      (\(mv, proj, (Position x y, Size w h)) -> do
          withMatrixColumnMajor mv $ \mvBuf ->
             withMatrixColumnMajor proj $ \projBuf ->
                withArray [x, y, w, h] $ \viewportBuf ->
                  gluLoadSamplingMatrices nurbsObj mvBuf projBuf viewportBuf
          setAutoLoadMatrix nurbsObj False)

withMatrixColumnMajor :: (Matrix m, MatrixComponent c) => m c -> (Ptr c -> IO a) -> IO a
withMatrixColumnMajor mat act =
   withMatrix mat $ \order p ->
      if order == ColumnMajor
         then act p
         else do
            elems <- mapM (peekElemOff p) [ 0, 4,  8, 12,
                                            1, 5,  9, 13,
                                            2, 6, 10, 14,
                                            3, 7, 11, 15 ]
            withArray elems act

foreign import CALLCONV safe "gluLoadSamplingMatrices"
   gluLoadSamplingMatrices :: NURBSObj -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLint -> IO ()

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

setDisplayMode :: NURBSObj -> DisplayMode' -> IO ()
setDisplayMode nurbsObj = setNURBSProperty nurbsObj DisplayMode' . marshalDisplayMode'
