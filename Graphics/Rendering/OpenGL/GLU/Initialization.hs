--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.Initialization
-- Copyright   :  (c) Sven Panne 2002
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module corresponds to chapter 2 (Initialization) of the GLU specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GLU.Initialization (
   gluVersion, gluExtensions
) where

import Control.Monad ( liftM )
import Foreign.C.String ( CString, peekCString )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )
import Graphics.Rendering.OpenGL.GL.Query (
   VersionInfo, parseVersionString, ExtensionsInfo(..) )
import Graphics.Rendering.OpenGL.GL.StateVariable (
   GettableStateVariable, makeGettableStateVariable )

--------------------------------------------------------------------------------

#define HOPENGL_IMPORT_StringName
#define HOPENGL_IMPORT_marshalStringName

#include "Constants.incl"

--------------------------------------------------------------------------------

gluVersion :: GettableStateVariable VersionInfo
gluVersion =
   makeGettableStateVariable $
      liftM (parseVersionString "queryGLUVersion") (getString Version)

gluExtensions :: GettableStateVariable ExtensionsInfo
gluExtensions =
   makeGettableStateVariable $
      liftM (ExtensionsInfo . words) (getString Extensions)

getString :: StringName -> IO String
getString n = gluGetString (marshalStringName n) >>= peekCString

foreign import CALLCONV unsafe "gluGetString" gluGetString ::
   GLenum -> IO CString
