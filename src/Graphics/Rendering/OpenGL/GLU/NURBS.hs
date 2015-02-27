--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.NURBS
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to chapter 7 (NURBS) of the GLU specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GLU.NURBS (
   NURBSObj, withNURBSObj,
   NURBSBeginCallback, withNURBSBeginCallback,
   NURBSVertexCallback, withNURBSVertexCallback,
   NURBSNormalCallback, withNURBSNormalCallback,
   NURBSColorCallback, withNURBSColorCallback,
   NURBSEndCallback, withNURBSEndCallback,
   checkForNURBSError,
   nurbsBeginEndCurve, nurbsCurve,
   nurbsBeginEndSurface, nurbsSurface,
   TrimmingPoint, nurbsBeginEndTrim, pwlCurve, trimmingCurve,
   NURBSMode(..), setNURBSMode,
   setNURBSCulling,
   SamplingMethod(..), setSamplingMethod,
   loadSamplingMatrices,
   DisplayMode'(..), setDisplayMode'
) where

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.GLU.Raw hiding (
   NURBSBeginCallback, NURBSVertexCallback, NURBSNormalCallback,
   NURBSColorCallback, NURBSEndCallback )
import Graphics.Rendering.OpenGL.GL.Tensor
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.ControlPoint
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.Exception
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.PrimitiveMode
import Graphics.Rendering.OpenGL.GL.PrimitiveModeInternal
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------
-- chapter 7.1: The NURBS Object

-- an opaque pointer to a NURBS object
type NURBSObj = Ptr GLUnurbs

isNullNURBSObj :: NURBSObj -> Bool
isNullNURBSObj = (nullPtr ==)

withNURBSObj :: a -> (NURBSObj -> IO a) -> IO a
withNURBSObj failureValue action =
   bracket gluNewNurbsRenderer safeDeleteNurbsRenderer
           (\nurbsObj -> if isNullNURBSObj nurbsObj
                            then do recordOutOfMemory
                                    return failureValue
                            else action nurbsObj)

safeDeleteNurbsRenderer :: NURBSObj -> IO ()
safeDeleteNurbsRenderer nurbsObj =
   unless (isNullNURBSObj nurbsObj) $ gluDeleteNurbsRenderer nurbsObj

--------------------------------------------------------------------------------
-- chapter 7.2: Callbacks (begin)

type NURBSBeginCallback = PrimitiveMode -> IO ()

withNURBSBeginCallback :: NURBSObj -> NURBSBeginCallback -> IO a -> IO a
withNURBSBeginCallback nurbsObj beginCallback action =
   bracket (makeNURBSBeginCallback (beginCallback . unmarshalPrimitiveMode))
           freeHaskellFunPtr $ \callbackPtr -> do
      gluNurbsCallback nurbsObj glu_NURBS_BEGIN callbackPtr
      action

--------------------------------------------------------------------------------
-- chapter 7.2: Callbacks (vertex)

type NURBSVertexCallback = Vertex3 GLfloat -> IO ()

withNURBSVertexCallback :: NURBSObj -> NURBSVertexCallback -> IO a -> IO a
withNURBSVertexCallback nurbsObj vertexCallback action =
   bracket (makeNURBSVertexCallback (\p -> peek (castPtr p) >>= vertexCallback))
           freeHaskellFunPtr $ \callbackPtr -> do
      gluNurbsCallback nurbsObj glu_NURBS_VERTEX callbackPtr
      action

--------------------------------------------------------------------------------
-- chapter 7.2: Callbacks (normal)

type NURBSNormalCallback = Normal3 GLfloat -> IO ()

withNURBSNormalCallback :: NURBSObj -> NURBSNormalCallback -> IO a -> IO a
withNURBSNormalCallback nurbsObj normalCallback action =
   bracket (makeNURBSNormalCallback (\p -> peek (castPtr p) >>= normalCallback))
           freeHaskellFunPtr $ \callbackPtr -> do
      gluNurbsCallback nurbsObj glu_NURBS_NORMAL callbackPtr
      action

--------------------------------------------------------------------------------
-- chapter 7.2: Callbacks (color)

type NURBSColorCallback = Color4 GLfloat -> IO ()

withNURBSColorCallback :: NURBSObj -> NURBSColorCallback -> IO a -> IO a
withNURBSColorCallback nurbsObj colorCallback action =
   bracket (makeNURBSColorCallback (\p -> peek (castPtr p) >>= colorCallback))
           freeHaskellFunPtr $ \callbackPtr -> do
      gluNurbsCallback nurbsObj glu_NURBS_COLOR callbackPtr
      action

--------------------------------------------------------------------------------
-- chapter 7.2: Callbacks (end)

type NURBSEndCallback = IO ()

withNURBSEndCallback :: NURBSObj -> NURBSEndCallback -> IO a -> IO a
withNURBSEndCallback nurbsObj endCallback action =
   bracket (makeNURBSEndCallback endCallback)
           freeHaskellFunPtr $ \callbackPtr -> do
      gluNurbsCallback nurbsObj glu_NURBS_END callbackPtr
      action

--------------------------------------------------------------------------------
-- chapter 7.2: Callbacks (error)

type ErrorCallback = GLenum -> IO ()

withErrorCallback :: NURBSObj -> ErrorCallback -> IO a -> IO a
withErrorCallback nurbsObj errorCallback action =
   bracket (makeNURBSErrorCallback errorCallback)
           freeHaskellFunPtr $ \callbackPtr -> do
      gluNurbsCallback nurbsObj glu_NURBS_ERROR callbackPtr
      action

checkForNURBSError :: NURBSObj -> IO a -> IO a
checkForNURBSError nurbsObj = withErrorCallback nurbsObj recordErrorCode

--------------------------------------------------------------------------------
-- chapter 7.3: NURBS Curves

nurbsBeginEndCurve :: NURBSObj -> IO a -> IO a
nurbsBeginEndCurve nurbsObj =
   bracket_ (gluBeginCurve nurbsObj) (gluEndCurve nurbsObj)


nurbsCurve :: ControlPoint c => NURBSObj -> GLint -> Ptr GLfloat -> GLint -> Ptr (c GLfloat) -> GLint -> IO ()
nurbsCurve nurbsObj knotCount knots stride control order =
   gluNurbsCurve nurbsObj knotCount knots stride (castPtr control) order (map1Target (pseudoPeek control))

pseudoPeek :: Ptr (c GLfloat) -> c GLfloat
pseudoPeek _ = undefined

--------------------------------------------------------------------------------
-- chapter 7.4: NURBS Surfaces

nurbsBeginEndSurface :: NURBSObj -> IO a -> IO a
nurbsBeginEndSurface nurbsObj =
   bracket_ (gluBeginSurface nurbsObj) (gluEndSurface nurbsObj)


nurbsSurface :: ControlPoint c => NURBSObj -> GLint -> Ptr GLfloat -> GLint -> Ptr GLfloat -> GLint -> GLint -> Ptr (c GLfloat) -> GLint -> GLint -> IO ()
nurbsSurface nurbsObj sKnotCount sKnots tKnotCount tKnots sStride tStride control sOrder tOrder =
   gluNurbsSurface nurbsObj sKnotCount sKnots tKnotCount tKnots sStride tStride (castPtr control) sOrder tOrder (map2Target (pseudoPeek control))

--------------------------------------------------------------------------------
-- chapter 7.5: Trimming

class TrimmingPoint p where
   trimmingTarget :: p GLfloat -> GLenum

instance TrimmingPoint Vertex2 where
   trimmingTarget = const glu_MAP1_TRIM_2

instance TrimmingPoint Vertex3 where
   trimmingTarget = const glu_MAP1_TRIM_3

nurbsBeginEndTrim :: NURBSObj -> IO a -> IO a
nurbsBeginEndTrim nurbsObj =
   bracket_ (gluBeginTrim nurbsObj) (gluEndTrim nurbsObj)

pwlCurve :: TrimmingPoint p => NURBSObj -> GLint -> Ptr (p GLfloat) -> GLint -> IO ()
pwlCurve nurbsObj count points stride =
   gluPwlCurve nurbsObj count (castPtr points) stride (trimmingTarget (pseudoPeek points))

trimmingCurve :: TrimmingPoint c => NURBSObj -> GLint -> Ptr GLfloat -> GLint -> Ptr (c GLfloat) -> GLint -> IO ()
trimmingCurve nurbsObj knotCount knots stride control order =
   gluNurbsCurve nurbsObj knotCount knots stride (castPtr control) order (trimmingTarget (pseudoPeek control))

--------------------------------------------------------------------------------

data NURBSMode =
     NURBSTessellator
   | NURBSRenderer
   deriving ( Eq, Ord, Show )

marshalNURBSMode :: NURBSMode -> GLfloat
marshalNURBSMode x = fromIntegral $ case x of
   NURBSTessellator -> glu_NURBS_TESSELLATOR
   NURBSRenderer -> glu_NURBS_RENDERER

setNURBSMode :: NURBSObj -> NURBSMode -> IO ()
setNURBSMode nurbsObj = gluNurbsProperty nurbsObj glu_NURBS_MODE . marshalNURBSMode

--------------------------------------------------------------------------------

setNURBSCulling :: NURBSObj -> Capability -> IO ()
setNURBSCulling nurbsObj = gluNurbsProperty nurbsObj glu_CULLING . fromIntegral . marshalCapability

--------------------------------------------------------------------------------

data SamplingMethod' =
     PathLength'
   | ParametricError'
   | DomainDistance'
   | ObjectPathLength'
   | ObjectParametricError'

marshalSamplingMethod' :: SamplingMethod' -> GLfloat
marshalSamplingMethod' x = fromIntegral $ case x of
   PathLength' -> glu_PATH_LENGTH
   ParametricError' -> glu_PARAMETRIC_TOLERANCE
   DomainDistance' -> glu_DOMAIN_DISTANCE
   ObjectPathLength' -> glu_OBJECT_PATH_LENGTH
   ObjectParametricError' -> glu_OBJECT_PARAMETRIC_ERROR

setSamplingMethod' :: NURBSObj -> SamplingMethod' -> IO ()
setSamplingMethod' nurbsObj = gluNurbsProperty nurbsObj glu_SAMPLING_METHOD . marshalSamplingMethod'

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
      gluNurbsProperty nurbsObj glu_SAMPLING_TOLERANCE s
      setSamplingMethod' nurbsObj PathLength'
   ParametricError p -> do
      gluNurbsProperty nurbsObj glu_PARAMETRIC_TOLERANCE p
      setSamplingMethod' nurbsObj ParametricError'
   DomainDistance u v -> do
      gluNurbsProperty nurbsObj glu_U_STEP u
      gluNurbsProperty nurbsObj glu_V_STEP v
      setSamplingMethod' nurbsObj DomainDistance'
   ObjectPathLength s -> do
      gluNurbsProperty nurbsObj glu_SAMPLING_TOLERANCE s
      setSamplingMethod' nurbsObj ObjectPathLength'
   ObjectParametricError p -> do
      gluNurbsProperty nurbsObj glu_PARAMETRIC_TOLERANCE p
      setSamplingMethod' nurbsObj ObjectParametricError'

--------------------------------------------------------------------------------

setAutoLoadMatrix :: NURBSObj -> Bool -> IO ()
setAutoLoadMatrix nurbsObj = gluNurbsProperty nurbsObj glu_AUTO_LOAD_MATRIX . marshalGLboolean

loadSamplingMatrices :: (Matrix m1, Matrix m2) => NURBSObj -> Maybe (m1 GLfloat, m2 GLfloat, (Position, Size)) -> IO ()
loadSamplingMatrices nurbsObj =
   maybe
      (setAutoLoadMatrix nurbsObj True)
      (\(mv, proj, (Position x y, Size w h)) -> do
          withMatrixColumnMajor mv $ \mvBuf ->
             withMatrixColumnMajor proj $ \projBuf ->
                withArray [x, y, fromIntegral w, fromIntegral h] $ \viewportBuf ->
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

--------------------------------------------------------------------------------

data DisplayMode' =
     Fill'
   | OutlinePolygon
   | OutlinePatch
   deriving ( Eq, Ord, Show )

marshalDisplayMode' :: DisplayMode' -> GLfloat
marshalDisplayMode' x = fromIntegral $ case x of
   Fill' -> glu_FILL
   OutlinePolygon -> glu_OUTLINE_POLYGON
   OutlinePatch -> glu_OUTLINE_PATCH

setDisplayMode' :: NURBSObj -> DisplayMode' -> IO ()
setDisplayMode' nurbsObj = gluNurbsProperty nurbsObj glu_DISPLAY_MODE . marshalDisplayMode'
