--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Antialiasing
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 3.2 (Antialiasing) of the OpenGL 2.1
-- specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Antialiasing (
   sampleBuffers, samples, multisample, subpixelBits
) where

import Data.StateVar
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

sampleBuffers :: GettableStateVar GLsizei
sampleBuffers = antialiasingInfo GetSampleBuffers

samples :: GettableStateVar GLsizei
samples = antialiasingInfo GetSamples

multisample :: StateVar Capability
multisample = makeCapability CapMultisample

subpixelBits :: GettableStateVar GLsizei
subpixelBits = antialiasingInfo GetSubpixelBits

antialiasingInfo :: GetPName1I p => p -> GettableStateVar GLsizei
antialiasingInfo = makeGettableStateVar . getSizei1 id
