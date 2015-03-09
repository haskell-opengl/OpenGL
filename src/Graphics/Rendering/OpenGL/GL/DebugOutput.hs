{-# LANGUAGE CPP #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.DebugOutput
-- Copyright   :  (c) Sven Panne 2015
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 20 (Debug Output) of the OpenGL 4.5
-- specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.DebugOutput (
  -- * Debug Messages
  debugOutput, DebugMessage(..), DebugSource(..), DebugType(..),
  DebugMessageID(DebugMessageID), DebugSeverity(..), maxDebugMessageLength,

  -- * Debug Message Callback
  debugMessageCallback,

  -- * Debug Message Log
  maxDebugLoggedMessages, debugLoggedMessages,

  -- * Controlling Debug Messages
  MessageGroup(..), debugMessageControl,

  -- * Externally Generated Messages
  debugMessageInsert,

  -- * Debug Groups
  DebugGroup(..), pushDebugGroup, popDebugGroup, withDebugGroup,
  maxDebugGroupStackDepth,

  -- * Debug Labels
  CanBeLabeled(..), maxLabelLength,

  -- * Asynchronous and Synchronous Debug Output
  debugOutputSynchronous
) where

import Control.Monad ( unless, replicateM )
import Data.StateVar
import Foreign.C.String ( peekCStringLen, withCStringLen )
import Foreign.C.Types
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( allocaArray, withArrayLen )
import Foreign.Ptr (
  nullPtr, castPtrToFunPtr, FunPtr, nullFunPtr, freeHaskellFunPtr )
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.Exception
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

debugOutput :: StateVar Capability
debugOutput = makeCapability CapDebugOutput

--------------------------------------------------------------------------------

data DebugMessage =
  DebugMessage DebugSource DebugType DebugMessageID DebugSeverity String
  deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

data DebugSource =
    DebugSourceAPI
  | DebugSourceShaderCompiler
  | DebugSourceWindowSystem
  | DebugSourceThirdParty
  | DebugSourceApplication
  | DebugSourceOther
  deriving ( Eq, Ord, Show )

marshalDebugSource :: DebugSource -> GLenum
marshalDebugSource x = case x of
  DebugSourceAPI -> gl_DEBUG_SOURCE_API
  DebugSourceShaderCompiler -> gl_DEBUG_SOURCE_SHADER_COMPILER
  DebugSourceWindowSystem -> gl_DEBUG_SOURCE_WINDOW_SYSTEM
  DebugSourceThirdParty -> gl_DEBUG_SOURCE_THIRD_PARTY
  DebugSourceApplication -> gl_DEBUG_SOURCE_APPLICATION
  DebugSourceOther -> gl_DEBUG_SOURCE_OTHER

unmarshalDebugSource :: GLenum -> DebugSource
unmarshalDebugSource x
  | x == gl_DEBUG_SOURCE_API = DebugSourceAPI
  | x == gl_DEBUG_SOURCE_SHADER_COMPILER = DebugSourceShaderCompiler
  | x == gl_DEBUG_SOURCE_WINDOW_SYSTEM = DebugSourceWindowSystem
  | x == gl_DEBUG_SOURCE_THIRD_PARTY = DebugSourceThirdParty
  | x == gl_DEBUG_SOURCE_APPLICATION = DebugSourceApplication
  | x == gl_DEBUG_SOURCE_OTHER = DebugSourceOther
  | otherwise = error ("unmarshalDebugSource: illegal value " ++ show x)

--------------------------------------------------------------------------------

data DebugType =
    DebugTypeError
  | DebugTypeDeprecatedBehavior
  | DebugTypeUndefinedBehavior
  | DebugTypePerformance
  | DebugTypePortability
  | DebugTypeMarker
  | DebugTypePushGroup
  | DebugTypePopGroup
  | DebugTypeOther
  deriving ( Eq, Ord, Show )

marshalDebugType :: DebugType -> GLenum
marshalDebugType x = case x of
  DebugTypeError -> gl_DEBUG_TYPE_ERROR
  DebugTypeDeprecatedBehavior -> gl_DEBUG_TYPE_DEPRECATED_BEHAVIOR
  DebugTypeUndefinedBehavior -> gl_DEBUG_TYPE_UNDEFINED_BEHAVIOR
  DebugTypePerformance -> gl_DEBUG_TYPE_PERFORMANCE
  DebugTypePortability -> gl_DEBUG_TYPE_PORTABILITY
  DebugTypeMarker -> gl_DEBUG_TYPE_MARKER
  DebugTypePushGroup -> gl_DEBUG_TYPE_PUSH_GROUP
  DebugTypePopGroup -> gl_DEBUG_TYPE_POP_GROUP
  DebugTypeOther -> gl_DEBUG_TYPE_OTHER

unmarshalDebugType :: GLenum -> DebugType
unmarshalDebugType x
  | x == gl_DEBUG_TYPE_ERROR = DebugTypeError
  | x == gl_DEBUG_TYPE_DEPRECATED_BEHAVIOR = DebugTypeDeprecatedBehavior
  | x == gl_DEBUG_TYPE_UNDEFINED_BEHAVIOR = DebugTypeUndefinedBehavior
  | x == gl_DEBUG_TYPE_PERFORMANCE = DebugTypePerformance
  | x == gl_DEBUG_TYPE_PORTABILITY = DebugTypePortability
  | x == gl_DEBUG_TYPE_MARKER = DebugTypeMarker
  | x == gl_DEBUG_TYPE_PUSH_GROUP = DebugTypePushGroup
  | x == gl_DEBUG_TYPE_POP_GROUP = DebugTypePopGroup
  | x == gl_DEBUG_TYPE_OTHER = DebugTypeOther
  | otherwise = error ("unmarshalDebugType: illegal value " ++ show x)

--------------------------------------------------------------------------------

newtype DebugMessageID = DebugMessageID { debugMessageID :: GLuint }
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

data DebugSeverity =
    DebugSeverityHigh
  | DebugSeverityMedium
  | DebugSeverityLow
  | DebugSeverityNotification
  deriving ( Eq, Ord, Show )

marshalDebugSeverity :: DebugSeverity -> GLenum
marshalDebugSeverity x = case x of
  DebugSeverityHigh -> gl_DEBUG_SEVERITY_HIGH
  DebugSeverityMedium -> gl_DEBUG_SEVERITY_MEDIUM
  DebugSeverityLow -> gl_DEBUG_SEVERITY_LOW
  DebugSeverityNotification -> gl_DEBUG_SEVERITY_NOTIFICATION

unmarshalDebugSeverity :: GLenum -> DebugSeverity
unmarshalDebugSeverity x
  | x == gl_DEBUG_SEVERITY_HIGH = DebugSeverityHigh
  | x == gl_DEBUG_SEVERITY_MEDIUM = DebugSeverityMedium
  | x == gl_DEBUG_SEVERITY_LOW = DebugSeverityLow
  | x == gl_DEBUG_SEVERITY_NOTIFICATION = DebugSeverityNotification
  | otherwise = error ("unmarshalDebugSeverity: illegal value " ++ show x)

--------------------------------------------------------------------------------

maxDebugMessageLength :: GettableStateVar GLsizei
maxDebugMessageLength =
  makeGettableStateVar (getSizei1 id GetMaxDebugMessageLength)

--------------------------------------------------------------------------------

debugMessageCallback :: StateVar (Maybe (DebugMessage -> IO ()))
debugMessageCallback =
  makeStateVar getDebugMessageCallback setDebugMessageCallback

getDebugMessageCallback :: IO (Maybe (DebugMessage -> IO ()))
getDebugMessageCallback = do
  cb <- getDebugCallbackFunction
  return $ if (cb == nullFunPtr)
             then Nothing
             else Just . toDebugProc . dyn_debugProc $ cb

foreign import CALLCONV "dynamic" dyn_debugProc
  :: FunPtr GLDEBUGPROCFunc -> GLDEBUGPROCFunc

toDebugProc:: GLDEBUGPROCFunc -> DebugMessage -> IO ()
toDebugProc debugFunc (DebugMessage source typ msgID severity message) =
  withCStringLen message $ \(msg, len) -> do
    debugFunc (marshalDebugSource source)
              (marshalDebugType typ)
              (marshalDebugSeverity severity)
              (debugMessageID msgID)
              (fromIntegral len)
              msg
              nullPtr

setDebugMessageCallback :: Maybe (DebugMessage -> IO ()) -> IO ()
setDebugMessageCallback maybeDebugProc = do
  oldCB <- getDebugCallbackFunction
  unless (oldCB == nullFunPtr) $
    freeHaskellFunPtr oldCB
  newCB <-
    maybe (return nullFunPtr) (makeGLDEBUGPROC . fromDebugProc) maybeDebugProc
  glDebugMessageCallbackARB newCB  nullPtr

fromDebugProc:: (DebugMessage -> IO ()) -> GLDEBUGPROCFunc
fromDebugProc debugProc source typ msgID severity len message _userParam = do
  msg <- peekCStringLen (message, fromIntegral len)
  debugProc (DebugMessage (unmarshalDebugSource source)
                          (unmarshalDebugType typ)
                          (DebugMessageID msgID)
                          (unmarshalDebugSeverity severity)
                          msg)

getDebugCallbackFunction :: IO (FunPtr GLDEBUGPROCFunc)
getDebugCallbackFunction =
  castPtrToFunPtr `fmap` getPointer DebugCallbackFunction

--------------------------------------------------------------------------------

maxDebugLoggedMessages :: GettableStateVar GLsizei
maxDebugLoggedMessages =
  makeGettableStateVar (getSizei1 id GetMaxDebugLoggedMessages)

debugLoggedMessages :: IO [DebugMessage]
debugLoggedMessages = do
  count <- getSizei1 fromIntegral GetDebugLoggedMessages
  replicateM count debugNextLoggedMessage

debugNextLoggedMessage :: IO DebugMessage
debugNextLoggedMessage = do
  len <- getSizei1 id GetDebugNextLoggedMessageLength
  alloca $ \sourceBuf ->
    alloca $ \typeBuf ->
      alloca $ \idBuf ->
        alloca $ \severityBuf ->
          allocaArray (fromIntegral len) $ \messageBuf -> do
            _ <- glGetDebugMessageLog 1 len sourceBuf typeBuf idBuf
                                      severityBuf nullPtr messageBuf
            source <- peek1 unmarshalDebugSource sourceBuf
            typ <- peek1 unmarshalDebugType typeBuf
            msgID <- peek1 DebugMessageID idBuf
            severity <- peek1 unmarshalDebugSeverity severityBuf
            message <- peekCStringLen (messageBuf, fromIntegral len)
            return $ DebugMessage source typ msgID severity message

--------------------------------------------------------------------------------

data MessageGroup =
    MessageGroup (Maybe DebugSource) (Maybe DebugType) (Maybe DebugSeverity)
  | MessageGroupWithIDs DebugSource DebugType [DebugMessageID]
  deriving ( Eq, Ord, Show )

debugMessageControl :: MessageGroup -> SettableStateVar Capability
debugMessageControl x = case x of
  MessageGroup maybeSource maybeType maybeSeverity ->
    doDebugMessageControl maybeSource maybeType maybeSeverity []
  MessageGroupWithIDs source typ messageIDs ->
    doDebugMessageControl (Just source) (Just typ) Nothing messageIDs

doDebugMessageControl :: Maybe DebugSource
                      -> Maybe DebugType
                      -> Maybe DebugSeverity
                      -> [DebugMessageID]
                      -> SettableStateVar Capability
doDebugMessageControl maybeSource maybeType maybeSeverity messageIDs =
  makeSettableStateVar $ \cap ->
    withArrayLen (map debugMessageID messageIDs) $ \len idsBuf ->
      glDebugMessageControl (maybe gl_DONT_CARE marshalDebugSource maybeSource)
                            (maybe gl_DONT_CARE marshalDebugType maybeType)
                            (maybe gl_DONT_CARE marshalDebugSeverity maybeSeverity)
                            (fromIntegral len)
                            idsBuf
                            (marshalCapability cap)

--------------------------------------------------------------------------------

debugMessageInsert :: DebugMessage -> IO ()
debugMessageInsert (DebugMessage source typ msgID severity message) =
  withCStringLen message $ \(msg, len) ->
    glDebugMessageInsert (marshalDebugSource source)
                         (marshalDebugType typ)
                         (debugMessageID msgID)
                         (marshalDebugSeverity severity)
                         (fromIntegral len)
                         msg

--------------------------------------------------------------------------------

data DebugGroup = DebugGroup DebugSource DebugMessageID String

pushDebugGroup :: DebugSource -> DebugMessageID -> String -> IO ()
pushDebugGroup source msgID message =
  withCStringLen message $ \(msg, len) ->
    glPushDebugGroup (marshalDebugSource source)
                     (debugMessageID msgID)
                     (fromIntegral len)
                     msg

popDebugGroup :: IO ()
popDebugGroup = glPopDebugGroup

withDebugGroup :: DebugSource -> DebugMessageID -> String -> IO a -> IO a
withDebugGroup source msgID message =
  bracket_ (pushDebugGroup source msgID message) popDebugGroup

maxDebugGroupStackDepth :: GettableStateVar GLsizei
maxDebugGroupStackDepth =
  makeGettableStateVar (getSizei1 id GetMaxDebugGroupStackDepth)

--------------------------------------------------------------------------------

-- TODO: Make instances for the following features when we have them:
--   * PROGRAM_PIPELINE / glGenProgramPipelines
--   * SAMPLER / glGenSamplers
--   * TRANSFORM_FEEDBACK / glGenTransformFeedbacks

class CanBeLabeled a where
  objectLabel :: a -> StateVar (Maybe String)

--------------------------------------------------------------------------------

debugOutputSynchronous :: StateVar Capability
debugOutputSynchronous = makeCapability CapDebugOutputSynchronous
