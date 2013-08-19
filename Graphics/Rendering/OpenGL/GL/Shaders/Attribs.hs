-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Shaders.Attribs
-- Copyright   :  (c) Sven Panne 2006-2013
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
--
-- Maintainer  :  svenpanne@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- This module contains functions related to shader attributes, corresponding
-- to section 2.20.3 of the OpenGL 3.1 spec (Shader Variables).
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Shaders.Attribs (
   attribLocation, VariableType(..), activeAttribs,
) where

import Graphics.Rendering.OpenGL.GL.GLstring
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import Graphics.Rendering.OpenGL.GL.Shaders.Variables
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.Raw.Core31

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
      withGLstring name $
         glGetAttribLocation program

bindAttribLocation :: Program -> AttribLocation -> String -> IO ()
bindAttribLocation (Program program) (AttribLocation location) name =
   withGLstring name $
      glBindAttribLocation program location

--------------------------------------------------------------------------------

activeAttribs :: Program -> GettableStateVar [(GLint,VariableType,String)]
activeAttribs = activeVars activeAttributes activeAttributeMaxLength glGetActiveAttrib
