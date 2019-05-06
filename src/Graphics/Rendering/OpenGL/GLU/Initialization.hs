--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.Initialization
-- Copyright   :  (c) Sven Panne 2002-2019
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to chapter 2 (Initialization) of the GLU specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GLU.Initialization (
   gluVersion, gluExtensions
) where

import Data.StateVar
import Graphics.GLU
import Graphics.Rendering.OpenGL.GL.ByteString
import Graphics.GL

--------------------------------------------------------------------------------

gluVersion :: GettableStateVar String
gluVersion = makeGettableStateVar (getString GLU_VERSION)

gluExtensions :: GettableStateVar [String]
gluExtensions = makeGettableStateVar (fmap words $ getString GLU_EXTENSIONS)

getString :: GLenum -> IO String
getString = getStringWith . gluGetString
