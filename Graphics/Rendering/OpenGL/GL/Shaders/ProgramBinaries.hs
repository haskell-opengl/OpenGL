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
   ProgramBinaryFormat(..), programBinaryFormats
) where

import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

newtype ProgramBinaryFormat = ProgramBinaryFormat GLenum
   deriving ( Eq, Ord, Show )

programBinaryFormats :: GettableStateVar [ProgramBinaryFormat]
programBinaryFormats =
   makeGettableStateVar $ do
      n <- getInteger1 fromIntegral GetNumProgramBinaryFormats
      getEnumN ProgramBinaryFormat GetProgramBinaryFormats n
