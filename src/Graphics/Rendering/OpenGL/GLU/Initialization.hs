--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.Initialization
-- Copyright   :  (c) Sven Panne 2002-2013
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
import Foreign.C.String
import Foreign.Ptr
import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

gluVersion :: GettableStateVar String
gluVersion = makeGettableStateVar (getString glu_VERSION)

gluExtensions :: GettableStateVar [String]
gluExtensions = makeGettableStateVar (fmap words $ getString glu_EXTENSIONS)

getString :: GLenum -> IO String
getString n = gluGetString n >>=
              maybeNullPtr (return "") (peekCString . castPtr)
