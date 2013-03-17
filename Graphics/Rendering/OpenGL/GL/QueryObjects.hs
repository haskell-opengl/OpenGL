-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Rendering.OpenGL.GL.QueryObject
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <sven.panne@aedion.de>
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.QueryObjects (
QueryObject, QueryTarget(..), marshalQueryTarget,

beginQuery, endQuery, withQuery,

queryCounterBits, currentQuery,

queryResult, queryResultAvailable,
-- * Conditional rendering
ConditionalRenderMode(..),

beginConditionalRender, endConditionalRender, withConditionalRender
) where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Graphics.Rendering.OpenGL.GL.ObjectName
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.Exception
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.Raw.Core31

newtype QueryObject = QueryObject { queryID :: GLuint }
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

instance ObjectName QueryObject where
   genObjectNames n =
      allocaArray n $ \buf -> do
        glGenQueries (fromIntegral n) buf
        fmap (map QueryObject) $ peekArray n buf

   deleteObjectNames queryObjects =
      withArrayLen (map queryID queryObjects) $
         glDeleteQueries . fromIntegral

   isObjectName = fmap unmarshalGLboolean . glIsQuery . queryID

--------------------------------------------------------------------------------

data QueryTarget =
     SamplesPassed
   | TransformFeedbackPrimitivesWritten
   | PrimitivesGenerated
   deriving ( Eq, Ord, Show )

marshalQueryTarget :: QueryTarget -> GLenum
marshalQueryTarget x = case x of
   SamplesPassed -> gl_SAMPLES_PASSED
   TransformFeedbackPrimitivesWritten -> gl_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN
   PrimitivesGenerated -> gl_PRIMITIVES_GENERATED

--------------------------------------------------------------------------------

beginQuery :: QueryTarget -> QueryObject -> IO ()
beginQuery t = glBeginQuery (marshalQueryTarget t) . queryID

endQuery :: QueryTarget -> IO ()
endQuery = glEndQuery . marshalQueryTarget

-- | Convenience function for an exception-safe combination of 'beginQuery' and
-- 'endQuery'.
withQuery :: QueryTarget -> QueryObject -> IO a -> IO a
withQuery t q = bracket_ (beginQuery t q) (endQuery t)

--------------------------------------------------------------------------------

data GetQueryPName =
     QueryCounterBits
   | CurrentQuery

marshalGetQueryPName :: GetQueryPName -> GLenum
marshalGetQueryPName x = case x of
   QueryCounterBits -> gl_QUERY_COUNTER_BITS
   CurrentQuery -> gl_CURRENT_QUERY

--------------------------------------------------------------------------------

queryCounterBits :: QueryTarget -> GettableStateVar GLsizei
queryCounterBits = getQueryi fromIntegral QueryCounterBits

currentQuery :: QueryTarget -> GettableStateVar (Maybe QueryObject)
currentQuery =
   getQueryi
      (\q -> if q == 0 then Nothing else Just (QueryObject (fromIntegral q)))
      CurrentQuery

getQueryi :: (GLint -> a) -> GetQueryPName -> QueryTarget -> GettableStateVar a
getQueryi f p t =
   makeGettableStateVar $
      alloca $ \buf -> do
         glGetQueryiv (marshalQueryTarget t) (marshalGetQueryPName p) buf
         peek1 f buf

--------------------------------------------------------------------------------

data GetQueryObjectPName =
     QueryResult
   | QueryResultAvailable

marshalGetQueryObjectPName :: GetQueryObjectPName -> GLenum
marshalGetQueryObjectPName x = case x of
   QueryResult -> gl_QUERY_RESULT
   QueryResultAvailable -> gl_QUERY_RESULT_AVAILABLE

--------------------------------------------------------------------------------

queryResult :: QueryObject -> GettableStateVar GLuint
queryResult = getQueryObjectui id QueryResult

queryResultAvailable :: QueryObject -> GettableStateVar Bool
queryResultAvailable = getQueryObjectui unmarshalGLboolean QueryResultAvailable

getQueryObjectui ::
   (GLuint -> a) -> GetQueryObjectPName -> QueryObject -> GettableStateVar a
getQueryObjectui f p q =
   makeGettableStateVar $
      alloca $ \buf -> do
         glGetQueryObjectuiv (queryID q) (marshalGetQueryObjectPName p) buf
         peek1 f buf

--------------------------------------------------------------------------------

data ConditionalRenderMode =
     QueryWait
   | QueryNoWait
   | QueryByRegionWait
   | QueryByRegionNoWait

marshalConditionalRenderMode :: ConditionalRenderMode -> GLenum
marshalConditionalRenderMode x = case x of
   QueryWait -> gl_QUERY_WAIT
   QueryNoWait -> gl_QUERY_NO_WAIT
   QueryByRegionWait -> gl_QUERY_BY_REGION_WAIT
   QueryByRegionNoWait -> gl_QUERY_BY_REGION_NO_WAIT

--------------------------------------------------------------------------------

beginConditionalRender :: QueryObject -> ConditionalRenderMode -> IO ()
beginConditionalRender q m = glBeginConditionalRender (queryID q) (marshalConditionalRenderMode m)

endConditionalRender :: IO ()
endConditionalRender = glEndConditionalRender

withConditionalRender :: QueryObject -> ConditionalRenderMode -> IO a -> IO a
withConditionalRender q m = bracket_ (beginConditionalRender q m) endConditionalRender




