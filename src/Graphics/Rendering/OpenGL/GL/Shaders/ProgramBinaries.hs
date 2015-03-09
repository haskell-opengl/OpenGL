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

import Data.StateVar
import Foreign.Marshal.Alloc
import Graphics.Rendering.OpenGL.GL.ByteString
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import Graphics.Rendering.OpenGL.Raw

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
getProgramBinary program =
   alloca $ \formatBuf -> do
      let getBin = bind4th formatBuf (glGetProgramBinary . programID)
      bs <- stringQuery programBinaryLength getBin program
      format <- peek1 ProgramBinaryFormat formatBuf
      return $ ProgramBinary format bs

bind4th :: d -> (a -> b -> c -> d -> e) -> (a -> b -> c -> e)
bind4th x = ((.) . (.) . (.)) ($ x)

setProgramBinary :: Program -> ProgramBinary -> IO ()
setProgramBinary program (ProgramBinary (ProgramBinaryFormat format) bs) = do
   withByteString bs $ glProgramBinary (programID program) format

programBinaryLength :: Program -> GettableStateVar GLsizei
programBinaryLength = programVar1 fromIntegral ProgramBinaryLength
