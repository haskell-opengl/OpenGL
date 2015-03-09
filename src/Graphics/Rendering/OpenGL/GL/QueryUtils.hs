{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.QueryUtils
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module with utilities to query OpenGL state.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.QueryUtils (
   module Graphics.Rendering.OpenGL.GL.QueryUtils.PName,
   module Graphics.Rendering.OpenGL.GL.QueryUtils.VertexAttrib,

   lightIndexToEnum,
   modelviewIndexToEnum, modelviewEnumToIndex,

   maybeNullPtr,

   objectNameLabel, objectPtrLabel, maxLabelLength
) where

import Data.StateVar
import Foreign.C.String ( peekCStringLen, withCStringLen )
import Foreign.Ptr ( Ptr, nullPtr )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( allocaArray )
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.QueryUtils.PName
import Graphics.Rendering.OpenGL.GL.QueryUtils.VertexAttrib
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

-- 0x4000 through 0x4FFF are reserved for light numbers

lightIndexToEnum :: GLsizei -> Maybe GLenum
lightIndexToEnum i
   | 0 <= i && i <= maxLightIndex = Just (gl_LIGHT0 + fromIntegral i)
   | otherwise = Nothing

maxLightIndex :: GLsizei
maxLightIndex = 0xFFF

--------------------------------------------------------------------------------

-- 0x1700, 0x850a, and 0x8722 through 0x873f are reserved for modelview matrices

modelviewIndexToEnum :: GLsizei -> Maybe GLenum
modelviewIndexToEnum 0 = Just gl_MODELVIEW
modelviewIndexToEnum 1 = Just gl_MODELVIEW1_ARB
modelviewIndexToEnum i
   | 2 <= i && i <= 31 = Just (gl_MODELVIEW2_ARB - 2 + fromIntegral i)
   | otherwise = Nothing

modelviewEnumToIndex :: GLenum -> Maybe GLsizei
modelviewEnumToIndex x
   | x == gl_MODELVIEW = Just 0
   | x == gl_MODELVIEW1_ARB = Just 1
   | gl_MODELVIEW2_ARB <= x && x <= gl_MODELVIEW31_ARB = Just (fromIntegral (x - (gl_MODELVIEW2_ARB - 2)))
   | otherwise = Nothing

--------------------------------------------------------------------------------

maybeNullPtr :: b -> (Ptr a -> b) -> Ptr a -> b
maybeNullPtr n f ptr | ptr == nullPtr = n
                     | otherwise      = f ptr

--------------------------------------------------------------------------------

objectNameLabel :: GLuint -> GLenum -> StateVar (Maybe String)
objectNameLabel name ident =
 makeStateVar
   (getObjectLabelWith (glGetObjectLabel ident name))
   (setObjectLabelWith (glObjectLabel ident name))

objectPtrLabel :: Ptr () -> StateVar (Maybe String)
objectPtrLabel ptr =
  makeStateVar
    (getObjectLabelWith (glGetObjectPtrLabel ptr))
    (setObjectLabelWith (glObjectPtrLabel ptr))

getObjectLabelWith :: (GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
                   -> IO (Maybe String)
getObjectLabelWith getLabel = do
  maxLen <- get maxLabelLength
  alloca $ \lenBuf ->
    allocaArray (fromIntegral maxLen) $ \labelBuf -> do
      getLabel maxLen lenBuf labelBuf
      actualLen <- peek1 fromIntegral lenBuf
      label <- peekCStringLen (labelBuf, actualLen)
      return $ if label == "" then Nothing else Just label

setObjectLabelWith :: (GLsizei -> Ptr GLchar -> IO ()) -> Maybe String -> IO ()
setObjectLabelWith setLabel =
  maybe (set (nullPtr, (0 :: Int))) (flip withCStringLen set)
  where set (labelBuf, len) = setLabel (fromIntegral len) labelBuf

maxLabelLength :: GettableStateVar GLsizei
maxLabelLength =
  makeGettableStateVar (getSizei1 id GetMaxLabelLength)
