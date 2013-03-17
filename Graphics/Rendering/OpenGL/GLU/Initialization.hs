--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.Initialization
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to chapter 2 (Initialization) of the GLU specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GLU.Initialization (
   gluVersion, gluExtensions
) where

import Foreign.C.String
import Foreign.Ptr
import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

gluVersion :: GettableStateVar String
gluVersion = makeGettableStateVar (getString glu_VERSION)

gluExtensions :: GettableStateVar [String]
gluExtensions = makeGettableStateVar (fmap words $ getString glu_EXTENSIONS)

getString :: GLenum -> IO String
getString n = gluGetString n >>=
              maybeNullPtr (return "") (peekCString . castPtr)
