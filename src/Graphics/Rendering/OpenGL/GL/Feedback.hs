--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Feedback
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 5.3 (Feedback) of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Feedback (
   FeedbackToken(..), VertexInfo(..), ColorInfo, FeedbackType(..),
   getFeedbackTokens, PassThroughValue(..), passThrough
) where

import Control.Monad
import Data.StateVar
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.IOState
import Graphics.Rendering.OpenGL.GL.RenderMode
import Graphics.Rendering.OpenGL.GL.Tensor
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.Raw

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
   | x == gl_POINT_TOKEN = PointTag
   | x == gl_LINE_TOKEN = LineTag
   | x == gl_LINE_RESET_TOKEN = LineResetTag
   | x == gl_POLYGON_TOKEN = PolygonTag
   | x == gl_BITMAP_TOKEN = BitmapTag
   | x == gl_DRAW_PIXEL_TOKEN = DrawPixelTag
   | x == gl_COPY_PIXEL_TOKEN = CopyPixelTag
   | x == gl_PASS_THROUGH_TOKEN = PassThroughTag
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
   TwoD -> gl_2D
   ThreeD -> gl_3D
   ThreeDColor -> gl_3D_COLOR
   ThreeDColorTexture -> gl_3D_COLOR_TEXTURE
   FourDColorTexture -> gl_4D_COLOR_TEXTURE

--------------------------------------------------------------------------------

getFeedbackTokens ::
   GLsizei -> FeedbackType -> IO a -> IO (a, Maybe [FeedbackToken])
getFeedbackTokens bufSize feedbackType action =
   allocaArray (fromIntegral bufSize) $ \buf -> do
      glFeedbackBuffer bufSize (marshalFeedbackType feedbackType) buf
      (value, numValues) <- withRenderMode Feedback action
      tokens <- parseFeedbackBuffer numValues buf feedbackType
      return (value, tokens)

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
      fmap Just $ evalIOState (loop []) buf

type Parser a = IOState GLfloat a

tokenParser :: Parser VertexInfo -> Parser FeedbackToken
tokenParser infoParser = do
   tag <- parseGLenum
   case unmarshalFeedbackTag tag of
      PointTag -> fmap PointToken infoParser
      LineTag -> liftM2 LineToken infoParser infoParser
      LineResetTag -> liftM2 LineResetToken infoParser infoParser
      PolygonTag -> do n <- parseGLint; fmap PolygonToken (nTimes n infoParser)
      BitmapTag -> fmap BitmapToken infoParser
      DrawPixelTag -> fmap DrawPixelToken infoParser
      CopyPixelTag -> fmap CopyPixelToken infoParser
      PassThroughTag -> fmap PassThroughToken parsePassThroughValue

calcInfoParser :: FeedbackType -> Parser ColorInfo -> Parser VertexInfo
calcInfoParser feedbackType colorParser = case feedbackType of
   TwoD ->
      fmap Vertex2D parseVertex2
   ThreeD ->
      fmap Vertex3D parseVertex3
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
calcColorParser False = fmap Left parseIndex1
calcColorParser True  = fmap Right parseColor4

parseIndex1 :: Parser (Index1 GLint)
parseIndex1 = fmap Index1 parseGLint

parseColor4 :: Parser (Color4 GLfloat)
parseColor4 = liftM4 Color4 parseGLfloat parseGLfloat parseGLfloat parseGLfloat

parseTexCoord4 :: Parser (TexCoord4 GLfloat)
parseTexCoord4 =
   liftM4 TexCoord4 parseGLfloat parseGLfloat parseGLfloat parseGLfloat

parsePassThroughValue :: Parser PassThroughValue
parsePassThroughValue = fmap PassThroughValue parseGLfloat

parseGLenum :: Parser GLenum
parseGLenum = fmap round parseGLfloat

parseGLint :: Parser GLint
parseGLint = fmap round parseGLfloat

parseGLfloat :: Parser GLfloat
parseGLfloat = peekIOState

--------------------------------------------------------------------------------

newtype PassThroughValue = PassThroughValue GLfloat
   deriving ( Eq, Ord, Show )

passThrough :: PassThroughValue -> IO ()
passThrough (PassThroughValue ptv) = glPassThrough ptv
