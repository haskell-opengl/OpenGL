--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.StringQueries
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to parts of section 6.1.11 (Pointer and String
-- Queries) of the OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.StringQueries (
   vendor, renderer, glVersion, glExtensions
) where

import Control.Monad ( liftM )
import Foreign.C.String ( peekCString )
import Foreign.Ptr ( Ptr, nullPtr, castPtr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLubyte )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar )

--------------------------------------------------------------------------------

vendor :: GettableStateVar String
vendor = makeGettableStateVar (getString Vendor)

renderer :: GettableStateVar String
renderer = makeGettableStateVar (getString Renderer)

glVersion :: GettableStateVar String
glVersion = makeGettableStateVar (getString Version)

glExtensions :: GettableStateVar [String]
glExtensions = makeGettableStateVar (liftM words $ getString Extensions)

--------------------------------------------------------------------------------

data StringName =
     Vendor
   | Renderer
   | Version
   | Extensions
   deriving ( Eq, Ord, Show )

marshalStringName :: StringName -> GLenum
marshalStringName x = case x of
   Vendor -> 0x1f00
   Renderer -> 0x1f01
   Version -> 0x1f02
   Extensions -> 0x1f03

--------------------------------------------------------------------------------

getString :: StringName -> IO String
getString n = do
   ptr <- glGetString (marshalStringName n)
   if ptr == nullPtr
      then return ""
      else peekCString (castPtr ptr)

foreign import CALLCONV unsafe "glGetString" glGetString ::
   GLenum -> IO (Ptr GLubyte)
