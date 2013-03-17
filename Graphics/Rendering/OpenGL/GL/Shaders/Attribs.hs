-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Shaders.Attribs
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <sven.panne@aedion.de>
-- Stability   :
-- Portability :
--
-- This module contains functions related to shader attributes, this corresponds
-- to section 2.20.3 of the OpenGL 3.1 spec (Shader Variables).
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Shaders.Attribs (
   attribLocation, activeAttribs,
) where

import Graphics.Rendering.OpenGL.GL.GLstring


import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw.Core31

import Graphics.Rendering.OpenGL.GL.Shaders.Program
import Graphics.Rendering.OpenGL.GL.Shaders.Variables

--------------------------------------------------------------------------------

activeAttributes :: Program -> GettableStateVar GLuint
activeAttributes = programVar fromIntegral ActiveAttributes

activeAttributeMaxLength :: Program -> GettableStateVar GLsizei
activeAttributeMaxLength = programVar fromIntegral ActiveAttributeMaxLength

--------------------------------------------------------------------------------

attribLocation :: Program -> String -> StateVar AttribLocation
attribLocation program name =
   makeStateVar (getAttribLocation program name)
                (\location -> bindAttribLocation program location name)

getAttribLocation :: Program -> String -> IO AttribLocation
getAttribLocation (Program program) name =
   fmap (AttribLocation . fromIntegral) $
      withGLString name $
         glGetAttribLocation program

bindAttribLocation :: Program -> AttribLocation -> String -> IO ()
bindAttribLocation (Program program) (AttribLocation location) name =
   withGLString name $
      glBindAttribLocation program location

--------------------------------------------------------------------------------

activeAttribs :: Program -> GettableStateVar [(GLint,VariableType,String)]
activeAttribs = activeVars activeAttributes activeAttributeMaxLength glGetActiveAttrib
