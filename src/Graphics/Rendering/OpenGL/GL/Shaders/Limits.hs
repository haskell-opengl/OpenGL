-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Shaders.Limits
-- Copyright   :  (c) Sven Panne 2006-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module contains functions related to shader limits.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Shaders.Limits (
   maxVertexTextureImageUnits, maxTextureImageUnits,
   maxCombinedTextureImageUnits, maxTextureCoords, maxVertexUniformComponents,
   maxFragmentUniformComponents, maxVertexAttribs, maxVaryingFloats,
   maxTessGenLevel
) where

import Data.StateVar
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw

-----------------------------------------------------------------------------

-- | Contains the number of hardware units that can be used to access texture
-- maps from the vertex processor. The minimum legal value is 0.

maxVertexTextureImageUnits :: GettableStateVar GLsizei
maxVertexTextureImageUnits = getLimit GetMaxVertexTextureImageUnits

-- | Contains the total number of hardware units that can be used to access
-- texture maps from the fragment processor. The minimum legal value is 2.

maxTextureImageUnits :: GettableStateVar GLsizei
maxTextureImageUnits = getLimit GetMaxTextureImageUnits

-- | Contains the total number of hardware units that can be used to access
-- texture maps from the vertex processor and the fragment processor combined.
-- Note: If the vertex shader and the fragment processing stage access the same
-- texture image unit, then that counts as using two texture image units. The
-- minimum legal value is 2.

maxCombinedTextureImageUnits :: GettableStateVar GLsizei
maxCombinedTextureImageUnits = getLimit GetMaxCombinedTextureImageUnits

-- | Contains the number of texture coordinate sets that are available. The
-- minimum legal value is 2.

maxTextureCoords :: GettableStateVar GLsizei
maxTextureCoords = getLimit GetMaxTextureCoords

-- | Contains the number of individual components (i.e., floating-point, integer
-- or boolean values) that are available for vertex shader uniform variables.
-- The minimum legal value is 512.
maxVertexUniformComponents :: GettableStateVar GLsizei
maxVertexUniformComponents = getLimit GetMaxVertexUniformComponents

-- | Contains the number of individual components (i.e., floating-point, integer
-- or boolean values) that are available for fragment shader uniform variables.
-- The minimum legal value is 64.

maxFragmentUniformComponents :: GettableStateVar GLsizei
maxFragmentUniformComponents = getLimit GetMaxFragmentUniformComponents

-- | Contains the number of active vertex attributes that are available. The
-- minimum legal value is 16.

maxVertexAttribs :: GettableStateVar GLsizei
maxVertexAttribs = getLimit GetMaxVertexAttribs

-- | Contains the number of individual floating-point values available for
-- varying variables. The minimum legal value is 32.

maxVaryingFloats :: GettableStateVar GLsizei
maxVaryingFloats = getLimit GetMaxVaryingFloats

-- | Contains the maximum allowed tessellation level.

maxTessGenLevel :: GettableStateVar GLsizei
maxTessGenLevel = getLimit GetMaxTessGenLevel

getLimit :: PName1I -> GettableStateVar GLsizei
getLimit = makeGettableStateVar . getSizei1 id
