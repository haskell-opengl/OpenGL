--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.StringQueries
-- Copyright   :  (c) Sven Panne 2002-2009
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
   vendor, renderer, glVersion, glExtensions, shadingLanguageVersion, majorMinor
) where

import Data.Char ( isDigit )
import Foreign.C.String ( peekCString )
import Foreign.Ptr ( Ptr, castPtr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLubyte )
import Graphics.Rendering.OpenGL.GL.QueryUtils ( maybeNullPtr )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasGetter(get), GettableStateVar, makeGettableStateVar )

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
getString n = glGetString (marshalStringName n) >>=
              maybeNullPtr (return "") (peekCString . castPtr)

foreign import CALLCONV unsafe "glGetString" glGetString ::
   GLenum -> IO (Ptr GLubyte)

--------------------------------------------------------------------------------

-- | A utility function to be used with e.g. 'glVersion' or
-- 'shadingLanguageVersion', transforming a variable containing a string of the
-- form /major.minor[optional rest]/ into a variable containing a numeric
-- major\/minor version. If the string is malformed, which should never happen
-- with a sane OpenGL implementation, it is transformed to @(-1,-1)@.

majorMinor :: GettableStateVar String -> GettableStateVar (Int, Int)
majorMinor = makeGettableStateVar . fmap parse . get
   where defaultVersion = (-1, -1)
         parse str =
            case span isDigit str of
               (major@(_:_), '.':rest) ->
                  case span isDigit rest of
                     (minor@(_:_), _) -> (read major, read minor)
                     _ -> defaultVersion
               _ -> defaultVersion
