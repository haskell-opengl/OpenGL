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

import Foreign.Ptr ( Ptr, castPtr )
import Foreign.C.String ( peekCString )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLubyte )
import Graphics.Rendering.OpenGL.GL.QueryUtils ( maybeNullPtr )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar )

--------------------------------------------------------------------------------

gluVersion :: GettableStateVar String
gluVersion = makeGettableStateVar (getString Version)

gluExtensions :: GettableStateVar [String]
gluExtensions = makeGettableStateVar (fmap words $ getString Extensions)

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

getString :: StringName -> IO String
getString n = gluGetString (marshalStringName n) >>=
              maybeNullPtr (return "") (peekCString . castPtr)

foreign import CALLCONV unsafe "gluGetString" gluGetString ::
   GLenum -> IO (Ptr GLubyte)
