--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Feedback
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 5.3 (Feedback) of the OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Feedback (
   FeedbackToken(..), VertexInfo(..), ColorInfo, FeedbackType(..),
   getFeedbackTokens, PassThroughValue(..), passThrough
) where

import Control.Monad ( liftM, liftM2, liftM3, liftM4 )
import Foreign.Marshal.Array ( allocaArray )
import Foreign.Ptr ( Ptr, plusPtr )
import Foreign.Storable ( Storable(sizeOf) )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLint, GLsizei, GLfloat )
import Graphics.Rendering.OpenGL.GL.IOState (
   IOState, getIOState, peekIOState, evalIOState, nTimes )
import Graphics.Rendering.OpenGL.GL.RenderMode ( withRenderMode )
import Graphics.Rendering.OpenGL.GL.Selection ( RenderMode(Feedback) )
import Graphics.Rendering.OpenGL.GL.StateVar ( HasGetter(get) )
import Graphics.Rendering.OpenGL.GL.VertexSpec (
   Vertex2(..), Vertex3(..), Vertex4(..), Index1(..), Color4(..),
   TexCoord4(..), rgbaMode )

--------------------------------------------------------------------------------

data FeedbackToken =
     PointToken VertexInfo
   | LineToken VertexInfo VertexInfo
   | LineResetToken VertexInfo VertexInfo
   | PolygonToken [VertexInfo]
   | BitmapToken VertexInfo
   | DrawPixelToken VertexInfo
   | CopyPixelToken VertexInfo
   | PassThroughToken PassThroughValue
   deriving ( Eq, Ord, Show )

data VertexInfo =
     Vertex2D             (Vertex2 GLfloat)
   | Vertex3D             (Vertex3 GLfloat)
   | Vertex3DColor        (Vertex3 GLfloat) ColorInfo
   | Vertex3DColorTexture (Vertex3 GLfloat) ColorInfo (TexCoord4 GLfloat)
   | Vertex4DColorTexture (Vertex4 GLfloat) ColorInfo (TexCoord4 GLfloat)
   deriving ( Eq, Ord, Show )

type ColorInfo = Either (Index1 GLint) (Color4 GLfloat)

--------------------------------------------------------------------------------

data FeedbackTag =
     PointTag
   | LineTag
   | LineResetTag
   | PolygonTag
   | BitmapTag
   | DrawPixelTag
   | CopyPixelTag
   | PassThroughTag

unmarshalFeedbackTag :: GLenum -> FeedbackTag
unmarshalFeedbackTag x
   | x == 0x701 = PointTag
   | x == 0x702 = LineTag
   | x == 0x707 = LineResetTag
   | x == 0x703 = PolygonTag
   | x == 0x704 = BitmapTag
   | x == 0x705 = DrawPixelTag
   | x == 0x706 = CopyPixelTag
   | x == 0x700 = PassThroughTag
   | otherwise = error ("unmarshalFeedbackTag: illegal value " ++ show x)

--------------------------------------------------------------------------------

data FeedbackType =
     TwoD
   | ThreeD
   | ThreeDColor
   | ThreeDColorTexture
   | FourDColorTexture
   deriving ( Eq, Ord, Show )

marshalFeedbackType :: FeedbackType -> GLenum
marshalFeedbackType x = case x of
   TwoD -> 0x600
   ThreeD -> 0x601
   ThreeDColor -> 0x602
   ThreeDColorTexture -> 0x603
   FourDColorTexture -> 0x604

--------------------------------------------------------------------------------

getFeedbackTokens ::
   GLsizei -> FeedbackType -> IO a -> IO (a, Maybe [FeedbackToken])
getFeedbackTokens bufSize feedbackType action =
   allocaArray (fromIntegral bufSize) $ \buf -> do
      glFeedbackBuffer bufSize (marshalFeedbackType feedbackType) buf
      (value, numValues) <- withRenderMode Feedback action
      tokens <- parseFeedbackBuffer numValues buf feedbackType
      return (value, tokens)

foreign import CALLCONV unsafe "glFeedbackBuffer" glFeedbackBuffer ::
   GLsizei -> GLenum -> Ptr GLfloat -> IO ()

--------------------------------------------------------------------------------

parseFeedbackBuffer ::
   GLint -> Ptr GLfloat -> FeedbackType -> IO (Maybe [FeedbackToken])
parseFeedbackBuffer numValues buf feedbackType
   | numValues < 0 = return Nothing
   | otherwise     = do
      rgba <- get rgbaMode
      let end = buf `plusPtr`
                  (sizeOf (undefined :: GLfloat) * fromIntegral numValues)
          infoParser = calcInfoParser feedbackType (calcColorParser rgba)
          loop tokens = do
             ptr <- getIOState
             if ptr == end
                then return (reverse tokens)
                else do token <- tokenParser infoParser
                        loop (token : tokens)
      liftM Just $ evalIOState (loop []) buf

type Parser a = IOState GLfloat a

tokenParser :: Parser VertexInfo -> Parser FeedbackToken
tokenParser infoParser = do
   tag <- parseGLenum
   case unmarshalFeedbackTag tag of
      PointTag -> liftM PointToken infoParser
      LineTag -> liftM2 LineToken infoParser infoParser
      LineResetTag -> liftM2 LineResetToken infoParser infoParser
      PolygonTag -> do n <- parseGLint; liftM PolygonToken (nTimes n infoParser)
      BitmapTag -> liftM BitmapToken infoParser
      DrawPixelTag -> liftM DrawPixelToken infoParser
      CopyPixelTag -> liftM CopyPixelToken infoParser
      PassThroughTag -> liftM PassThroughToken parsePassThroughValue

calcInfoParser :: FeedbackType -> Parser ColorInfo -> Parser VertexInfo
calcInfoParser feedbackType colorParser = case feedbackType of
   TwoD ->
      liftM Vertex2D parseVertex2
   ThreeD ->
      liftM Vertex3D parseVertex3
   ThreeDColor ->
      liftM2 Vertex3DColor parseVertex3 colorParser
   ThreeDColorTexture ->
      liftM3 Vertex3DColorTexture parseVertex3 colorParser parseTexCoord4
   FourDColorTexture ->
      liftM3 Vertex4DColorTexture parseVertex4 colorParser parseTexCoord4

parseVertex2 :: Parser (Vertex2 GLfloat)
parseVertex2 = liftM2 Vertex2 parseGLfloat parseGLfloat

parseVertex3 :: Parser (Vertex3 GLfloat)
parseVertex3 = liftM3 Vertex3 parseGLfloat parseGLfloat parseGLfloat

parseVertex4 :: Parser (Vertex4 GLfloat)
parseVertex4 =
   liftM4 Vertex4 parseGLfloat parseGLfloat parseGLfloat parseGLfloat

calcColorParser :: Bool -> Parser ColorInfo
calcColorParser False = liftM Left parseIndex1
calcColorParser True  = liftM Right parseColor4

parseIndex1 :: Parser (Index1 GLint)
parseIndex1 = liftM Index1 parseGLint

parseColor4 :: Parser (Color4 GLfloat)
parseColor4 = liftM4 Color4 parseGLfloat parseGLfloat parseGLfloat parseGLfloat

parseTexCoord4 :: Parser (TexCoord4 GLfloat)
parseTexCoord4 =
   liftM4 TexCoord4 parseGLfloat parseGLfloat parseGLfloat parseGLfloat

parsePassThroughValue :: Parser PassThroughValue
parsePassThroughValue = liftM PassThroughValue parseGLfloat

parseGLenum :: Parser GLenum
parseGLenum = liftM round parseGLfloat

parseGLint :: Parser GLint
parseGLint = liftM round parseGLfloat

parseGLfloat :: Parser GLfloat
parseGLfloat = peekIOState

--------------------------------------------------------------------------------

newtype PassThroughValue = PassThroughValue GLfloat
   deriving ( Eq, Ord, Show )

foreign import CALLCONV unsafe "glPassThrough" passThrough ::
   PassThroughValue -> IO ()
