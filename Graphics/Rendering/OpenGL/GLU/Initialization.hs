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
   getVersion, getExtensions
) where

import Control.Monad ( liftM )
import Foreign.C.String ( CString, peekCString )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )
import Graphics.Rendering.OpenGL.GL.Query (
   VersionInfo, parseVersionString, ExtensionsInfo(..) )
import Graphics.Rendering.OpenGL.GLU.Constants (
   StringName(..), marshalStringName )

--------------------------------------------------------------------------------

getVersion :: IO VersionInfo
getVersion = liftM (parseVersionString "queryGLUVersion") (getString Version)

getExtensions :: IO ExtensionsInfo
getExtensions = liftM (ExtensionsInfo . words) (getString Extensions)

getString :: StringName -> IO String
getString n = gluGetString (marshalStringName n) >>= peekCString

foreign import CALLCONV unsafe "gluGetString" gluGetString ::
   GLenum -> IO CString
