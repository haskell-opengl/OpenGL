--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.SyncObjects
-- Copyright   :  (c) Sven Panne 2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 4.1 (Sync Objects and Fences) of the
-- OpenGL 4.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.SyncObjects (
   -- * Sync Objects and Fences
   SyncObject, syncGpuCommandsComplete,

   -- * Waiting for Sync Objects
   WaitTimeout, WaitFlag(..), WaitResult(..), clientWaitSync,
   waitSync, maxServerWaitTimeout,

   -- * Sync Object Queries
   SyncStatus(..), syncStatus
) where

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.ObjectName
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.Raw.ARB.Sync
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

newtype SyncObject = SyncObject { syncID :: GLsync }
   deriving ( Eq, Ord, Show )

instance ObjectName SyncObject where
   isObjectName = fmap unmarshalGLboolean . glIsSync . syncID
   deleteObjectName = glDeleteSync . syncID

syncGpuCommandsComplete :: IO SyncObject
syncGpuCommandsComplete =
   fmap SyncObject $ glFenceSync gl_SYNC_GPU_COMMANDS_COMPLETE 0

--------------------------------------------------------------------------------

type WaitTimeout = GLuint64

--------------------------------------------------------------------------------

data WaitFlag = SyncFlushCommands
   deriving ( Eq, Ord, Show )

marshalWaitFlag :: WaitFlag -> GLbitfield
marshalWaitFlag x = case x of
   SyncFlushCommands -> gl_SYNC_FLUSH_COMMANDS_BIT

--------------------------------------------------------------------------------

data WaitResult =
     AlreadySignaled
   | TimeoutExpired
   | ConditionSatisfied
   | WaitFailed
   deriving ( Eq, Ord, Show )

unmarshalWaitResult :: GLenum -> WaitResult
unmarshalWaitResult x
   | x == gl_ALREADY_SIGNALED = AlreadySignaled
   | x == gl_TIMEOUT_EXPIRED = TimeoutExpired
   | x == gl_CONDITION_SATISFIED = ConditionSatisfied
   | x == gl_WAIT_FAILED = WaitFailed
   | otherwise = error ("unmarshalWaitResult: illegal value " ++ show x)

--------------------------------------------------------------------------------

clientWaitSync :: SyncObject -> [WaitFlag] -> WaitTimeout -> IO WaitResult
clientWaitSync syncObject flags =
   fmap unmarshalWaitResult .
      glClientWaitSync (syncID syncObject) (sum (map marshalWaitFlag flags))

waitSync :: SyncObject -> IO ()
waitSync syncObject =
   glWaitSync (syncID syncObject) 0 (fromIntegral gl_TIMEOUT_IGNORED)

maxServerWaitTimeout :: GettableStateVar WaitTimeout
maxServerWaitTimeout =
   makeGettableStateVar (getInteger64 fromIntegral GetMaxServerWaitTimeout)

--------------------------------------------------------------------------------

data SyncStatus =
     Unsignaled
   | Signaled
   deriving ( Eq, Ord, Show )

unmarshalSyncStatus :: GLenum -> SyncStatus
unmarshalSyncStatus x
   | x == gl_UNSIGNALED = Unsignaled
   | x == gl_SIGNALED = Signaled
   | otherwise = error ("unmarshalSyncStatus: illegal value " ++ show x)

syncStatus :: SyncObject -> GettableStateVar SyncStatus
syncStatus syncObject =
   makeGettableStateVar $
      alloca $ \buf -> do
         glGetSynciv (syncID syncObject) gl_SYNC_STATUS 1 nullPtr buf
         peek1 (unmarshalSyncStatus . fromIntegral) buf
