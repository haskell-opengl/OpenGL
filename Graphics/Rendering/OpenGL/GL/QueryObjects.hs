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
   QueryObject, QueryTarget(..),
   beginQuery, endQuery, withQuery,

   -- * Query Object Queries
   queryCounterBits, currentQuery,
   queryResult, queryResultAvailable
) where

import Foreign.Marshal.Alloc
import Graphics.Rendering.OpenGL.GL.Exception
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.QueryObject
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.Raw.ARB.ES3Compatibility
import Graphics.Rendering.OpenGL.Raw.ARB.OcclusionQuery2
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

data QueryTarget =
     SamplesPassed
   | AnySamplesPassed
   | AnySamplesPassedConservative
   | TransformFeedbackPrimitivesWritten
   | PrimitivesGenerated
   deriving ( Eq, Ord, Show )

marshalQueryTarget :: QueryTarget -> GLenum
marshalQueryTarget x = case x of
   SamplesPassed -> gl_SAMPLES_PASSED
   AnySamplesPassed -> gl_ANY_SAMPLES_PASSED
   AnySamplesPassedConservative -> gl_ANY_SAMPLES_PASSED_CONSERVATIVE
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
