--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.StringQueries
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to parts of section 6.1.5 (String Queries) of the
-- OpenGL 3.2 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.StringQueries (
   vendor, renderer, glVersion, glExtensions, shadingLanguageVersion,
   majorMinor, ContextProfile'(..), contextProfile
) where

import Data.Bits
import Data.Char
import Data.StateVar
import Foreign.C.String
import Foreign.Ptr
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

vendor :: GettableStateVar String
vendor = makeGettableStateVar (getString gl_VENDOR)

renderer :: GettableStateVar String
renderer = makeGettableStateVar (getString gl_RENDERER)

glVersion :: GettableStateVar String
glVersion = makeGettableStateVar (getString gl_VERSION)

glExtensions :: GettableStateVar [String]
glExtensions = makeGettableStateVar (fmap words $ getString gl_EXTENSIONS)

shadingLanguageVersion :: GettableStateVar String
shadingLanguageVersion = makeGettableStateVar (getString gl_SHADING_LANGUAGE_VERSION)

--------------------------------------------------------------------------------

data ContextProfile'
   = CoreProfile'
   | CompatibilityProfile'
   deriving ( Eq, Ord, Show )

marshalContextProfile' :: ContextProfile' -> GLbitfield
marshalContextProfile' x = case x of
   CoreProfile' -> gl_CONTEXT_CORE_PROFILE_BIT
   CompatibilityProfile' -> gl_CONTEXT_COMPATIBILITY_PROFILE_BIT

contextProfile :: GettableStateVar [ContextProfile']
contextProfile = makeGettableStateVar (getInteger1 i2cps GetContextProfileMask)

i2cps :: GLint -> [ContextProfile']
i2cps bitfield =
   [ c | c <- [ CoreProfile', CompatibilityProfile' ]
       , (fromIntegral bitfield .&. marshalContextProfile' c) /= 0 ]

--------------------------------------------------------------------------------

getString :: GLenum -> IO String
getString n = glGetString n >>= maybeNullPtr (return "") (peekCString . castPtr)

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
