-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Shaders.ProgramBinaries
-- Copyright   :  (c) Sven Panne 2006-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 7.5 (Program Binaries) of the OpenGL 4.4
-- spec.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Shaders.ProgramBinaries (
   ProgramBinaryFormat(..), programBinaryFormats,
   ProgramBinary(..), programBinary
) where

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Graphics.Rendering.OpenGL.GL.ByteString
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.Raw.ARB.GetProgramBinary
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

newtype ProgramBinaryFormat = ProgramBinaryFormat GLenum
   deriving ( Eq, Ord, Show )

programBinaryFormats :: GettableStateVar [ProgramBinaryFormat]
programBinaryFormats =
   makeGettableStateVar $ do
      n <- getInteger1 fromIntegral GetNumProgramBinaryFormats
      getEnumN ProgramBinaryFormat GetProgramBinaryFormats n

data ProgramBinary = ProgramBinary ProgramBinaryFormat ByteString
   deriving ( Eq, Ord, Show )

programBinary :: Program -> StateVar ProgramBinary
programBinary program =
   makeStateVar (getProgramBinary program) (setProgramBinary program)

getProgramBinary :: Program -> IO ProgramBinary
getProgramBinary program = do
   len <- get (programBinaryLength program)
   alloca $ \formatBuf -> do
      bs <- createByteString len $
         glGetProgramBinary (programID program) len nullPtr formatBuf
      format <- peek1 ProgramBinaryFormat formatBuf
      return $ ProgramBinary format bs

setProgramBinary :: Program -> ProgramBinary -> IO ()
setProgramBinary program (ProgramBinary (ProgramBinaryFormat format) bs) = do
   withByteString bs $ glProgramBinary (programID program) format

programBinaryLength :: Program -> GettableStateVar GLsizei
programBinaryLength = programVar1 fromIntegral ProgramBinaryLength
