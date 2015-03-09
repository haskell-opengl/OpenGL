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

{-# LANGUAGE TypeSynonymInstances #-}

module Graphics.Rendering.OpenGL.GL.QueryObjects (
   -- * Creating and Delimiting Queries
   QueryObject, QueryIndex, maxVertexStreams, QueryTarget(..),
   beginQuery, endQuery, withQuery,

   -- * Query Target Queries
   currentQuery, queryCounterBits,

   -- * Query Object Queries
   queryResultAvailable, QueryResult, queryResult,

   -- * Time Queries
   timestampQuery, timestamp
) where

import Data.StateVar
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.Exception
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.QueryObject
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

type QueryIndex = GLuint

maxVertexStreams :: GettableStateVar QueryIndex
maxVertexStreams =
   makeGettableStateVar (getInteger1 fromIntegral GetMaxVertexStreams)

--------------------------------------------------------------------------------

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
      with 0 $ \buf -> do
         getQueryiv' t p buf
         peek1 f buf

getQueryiv' :: QueryTarget -> GetQueryPName -> Ptr GLint -> IO ()
getQueryiv' target = case marshalQueryTarget target of
   (t, 0) -> glGetQueryiv t . marshalGetQueryPName
   (t, n) -> glGetQueryIndexediv t n . marshalGetQueryPName

--------------------------------------------------------------------------------

data GetQueryObjectPName =
     QueryResultAvailable
   | QueryResult

marshalGetQueryObjectPName :: GetQueryObjectPName -> GLenum
marshalGetQueryObjectPName x = case x of
   QueryResultAvailable -> gl_QUERY_RESULT_AVAILABLE
   QueryResult -> gl_QUERY_RESULT

--------------------------------------------------------------------------------

queryResultAvailable :: QueryObject -> GettableStateVar Bool
queryResultAvailable =
   getQueryObject (unmarshalGLboolean :: GLuint -> Bool) QueryResultAvailable

queryResult :: QueryResult a => QueryObject -> GettableStateVar a
queryResult = getQueryObject id QueryResult

class Storable a => QueryResult a where
   getQueryObjectv :: GLuint -> GLenum -> Ptr a -> IO ()

instance QueryResult GLint where getQueryObjectv = glGetQueryObjectiv
instance QueryResult GLuint where getQueryObjectv = glGetQueryObjectuiv
instance QueryResult GLint64 where getQueryObjectv = glGetQueryObjecti64v
instance QueryResult GLuint64 where getQueryObjectv = glGetQueryObjectui64v

getQueryObject :: (QueryResult a)
               => (a -> b)
               -> GetQueryObjectPName
               -> QueryObject
               -> GettableStateVar b
getQueryObject f p q =
   makeGettableStateVar $
      alloca $ \buf -> do
         getQueryObjectv (queryID q) (marshalGetQueryObjectPName p) buf
         peek1 f buf

--------------------------------------------------------------------------------

-- | Record the time after all previous commands on the GL client and server
-- state and the framebuffer have been fully realized

timestampQuery :: QueryObject -> IO ()
timestampQuery q = glQueryCounter (queryID q) gl_TIMESTAMP

-- | Contains the GL time after all previous commands have reached the GL server
-- but have not yet necessarily executed.

timestamp :: GettableStateVar GLuint64
timestamp = makeGettableStateVar (getInteger64 fromIntegral GetTimestamp)
