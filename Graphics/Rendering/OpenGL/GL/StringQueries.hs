--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.StringQueries
-- Copyright   :  (c) Sven Panne 2002-2006
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to parts of section 6.1.11 (Pointer and String
-- Queries) of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.StringQueries (
   vendor, renderer, glVersion, glExtensions, shadingLanguageVersion
) where

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
glExtensions = makeGettableStateVar (fmap words $ getString Extensions)

shadingLanguageVersion :: GettableStateVar String
shadingLanguageVersion = makeGettableStateVar (getString ShadingLanguageVersion)

--------------------------------------------------------------------------------

data StringName =
     Vendor
   | Renderer
   | Version
   | Extensions
   | ShadingLanguageVersion

marshalStringName :: StringName -> GLenum
marshalStringName x = case x of
   Vendor -> 0x1f00
   Renderer -> 0x1f01
   Version -> 0x1f02
   Extensions -> 0x1f03
   ShadingLanguageVersion -> 0x8b8c

--------------------------------------------------------------------------------

getString :: StringName -> IO String
getString n = do
   ptr <- glGetString (marshalStringName n)
   if ptr == nullPtr
      then return ""
      else peekCString (castPtr ptr)

foreign import CALLCONV unsafe "glGetString" glGetString ::
   GLenum -> IO (Ptr GLubyte)
