--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.Initialization
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to chapter 2 (Initialization) of the GLU specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GLU.Initialization (
   gluVersion, gluExtensions
) where

import Control.Monad ( liftM )
import Foreign.Ptr ( Ptr, nullPtr, castPtr )
import Foreign.C.String ( peekCString )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLubyte )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar )

--------------------------------------------------------------------------------

gluVersion :: GettableStateVar String
gluVersion = makeGettableStateVar (getString Version)

gluExtensions :: GettableStateVar [String]
gluExtensions = makeGettableStateVar (liftM words $ getString Extensions)

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
getString n = do
   ptr <- gluGetString (marshalStringName n)
   if ptr == nullPtr
      then return ""
      else peekCString (castPtr ptr)

foreign import CALLCONV unsafe "gluGetString" gluGetString ::
   GLenum -> IO (Ptr GLubyte)
