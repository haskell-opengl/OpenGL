-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.QueryObjects
-- Copyright   :  (c) Sven Panne, Lars Corbijn 2004-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 4.2 (Query Objects and Asynchronous
-- Queries) of the OpenGL 4.4 specs.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.QueryObjects (
   -- * Creating and Delimiting Queries
   QueryObject, QueryIndex, QueryTarget(..),
   beginQuery, endQuery, withQuery,

   -- * Query Object Queries
   currentQuery, queryCounterBits,
   queryResult, queryResultAvailable
) where

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Graphics.Rendering.OpenGL.GL.Exception
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.QueryObject
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.Raw.ARB.ES3Compatibility
import Graphics.Rendering.OpenGL.Raw.ARB.OcclusionQuery2
import Graphics.Rendering.OpenGL.Raw.ARB.TimerQuery
import Graphics.Rendering.OpenGL.Raw.ARB.TransformFeedback3
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

type QueryIndex = GLuint

data QueryTarget =
     SamplesPassed
   | AnySamplesPassed
   | AnySamplesPassedConservative
   | TimeElapsed
   | PrimitivesGenerated QueryIndex
   | TransformFeedbackPrimitivesWritten QueryIndex
   deriving ( Eq, Ord, Show )

marshalQueryTarget :: QueryTarget -> (GLenum, QueryIndex)
marshalQueryTarget x = case x of
   SamplesPassed -> (gl_SAMPLES_PASSED, 0)
   AnySamplesPassed -> (gl_ANY_SAMPLES_PASSED, 0)
   AnySamplesPassedConservative -> (gl_ANY_SAMPLES_PASSED_CONSERVATIVE, 0)
   TimeElapsed -> (gl_TIME_ELAPSED, 0)
   PrimitivesGenerated n -> (gl_PRIMITIVES_GENERATED, n)
   TransformFeedbackPrimitivesWritten n ->
      (gl_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN, n)

--------------------------------------------------------------------------------

beginQuery :: QueryTarget -> QueryObject -> IO ()
beginQuery target = case marshalQueryTarget target of
   (t, 0) -> glBeginQuery t . queryID
   (t, n) -> glBeginQueryIndexed t n . queryID

endQuery :: QueryTarget -> IO ()
endQuery target = case marshalQueryTarget target of
   (t, 0) -> glEndQuery t
   (t, n) -> glEndQueryIndexed t n

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

currentQuery :: QueryTarget -> GettableStateVar (Maybe QueryObject)
currentQuery = getQueryi (toMaybeQueryObject . toQueryObject) CurrentQuery
   where toQueryObject = QueryObject . fromIntegral
         toMaybeQueryObject q = if q == noQueryObject then Nothing else Just q

queryCounterBits :: QueryTarget -> GettableStateVar GLsizei
queryCounterBits = getQueryi fromIntegral QueryCounterBits

getQueryi :: (GLint -> a) -> GetQueryPName -> QueryTarget -> GettableStateVar a
getQueryi f p t =
   makeGettableStateVar $
      alloca $ \buf -> do
         getQueryiv' t p buf
         peek1 f buf

getQueryiv' :: QueryTarget -> GetQueryPName -> Ptr GLint -> IO ()
getQueryiv' target = case marshalQueryTarget target of
   (t, 0) -> glGetQueryiv t . marshalGetQueryPName
   (t, n) -> glGetQueryIndexediv t n . marshalGetQueryPName

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
