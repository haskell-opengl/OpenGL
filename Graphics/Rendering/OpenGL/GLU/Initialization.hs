--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.Initialization
-- Copyright   :  (c) Sven Panne 2003
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
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar )

--------------------------------------------------------------------------------

data StringName =
     Version
   | Extensions
   deriving ( Eq, Ord, Show )

marshalStringName :: StringName -> GLenum
marshalStringName x = case x of
   Version -> 0x189c0
   Extensions -> 0x189c1

--------------------------------------------------------------------------------

gluVersion :: GettableStateVar VersionInfo
gluVersion =
   makeGettableStateVar $
      liftM (parseVersionString "queryGLUVersion") (getString Version)

gluExtensions :: GettableStateVar ExtensionsInfo
gluExtensions =
   makeGettableStateVar $
      liftM (ExtensionsInfo . words) (getString Extensions)

getString :: StringName -> IO String
getString n = gluGetString (marshalStringName n) >>= peekCString

foreign import CALLCONV unsafe "gluGetString" gluGetString ::
   GLenum -> IO CString
