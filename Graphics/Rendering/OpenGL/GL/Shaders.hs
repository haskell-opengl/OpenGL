--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Shaders
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to sections 2.15 (Vertex Shaders) and section 3.11
-- (Fragment Shaders) of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Shaders (
  module Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects,

   -- * Program Objects
   Program, createProgram, programDeleteStatus,
   attachShader, detachShader, attachedShaders, linkProgram, linkStatus,
   validateProgram, validateStatus, programInfoLog, currentProgram,

   -- * FragmentData
   bindFragDataLocation,
   getFragDataLocation,

   -- * Vertex attributes
   attribLocation, VariableType(..), activeAttribs,

   -- * Uniform variables
   UniformLocation, uniformLocation, activeUniforms, Uniform(..),
   UniformComponent,

   -- * Implementation limits related to GLSL
   maxVertexTextureImageUnits, maxTextureImageUnits,
   maxCombinedTextureImageUnits, maxTextureCoords, maxVertexUniformComponents,
   maxFragmentUniformComponents, maxVertexAttribs, maxVaryingFloats
) where

import Graphics.Rendering.OpenGL.GL.Shaders.Attribs
import Graphics.Rendering.OpenGL.GL.Shaders.Limits
import Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects
import Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects
import Graphics.Rendering.OpenGL.GL.Shaders.Uniform
import Graphics.Rendering.OpenGL.GL.Shaders.Variables
