{-# LANGUAGE CPP #-}
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
   vendor, renderer, glVersion, glExtensions, extensionSupported,
   shadingLanguageVersion, majorMinor, ContextProfile'(..), contextProfile
) where

import Data.Bits
import Data.Char
#if !MIN_VERSION_base(4,8,0)
import Data.Functor( (<$>), (<$) )
#endif
import Data.Set ( member, toList )
import Data.StateVar as S
import Graphics.Rendering.OpenGL.GL.ByteString
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw
import Text.ParserCombinators.ReadP as R

--------------------------------------------------------------------------------

vendor :: GettableStateVar String
vendor = makeStringVar gl_VENDOR

renderer :: GettableStateVar String
renderer = makeStringVar gl_RENDERER

glVersion :: GettableStateVar String
glVersion = makeStringVar gl_VERSION

glExtensions :: GettableStateVar [String]
glExtensions = makeGettableStateVar (toList <$> getExtensions)

extensionSupported :: String -> GettableStateVar Bool
extensionSupported ext =
  makeGettableStateVar (getExtensions >>= (return . member ext))

shadingLanguageVersion :: GettableStateVar String
shadingLanguageVersion = makeStringVar gl_SHADING_LANGUAGE_VERSION

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

makeStringVar :: GLenum -> GettableStateVar String
makeStringVar = makeGettableStateVar . getStringWith . glGetString

--------------------------------------------------------------------------------

-- | A utility function to be used with e.g. 'glVersion' or
-- 'shadingLanguageVersion', transforming a variable containing a string of the
-- form /major.minor[optional rest]/ into a variable containing a numeric
-- major\/minor version. If the string is malformed, which should never happen
-- with a sane OpenGL implementation, it is transformed to @(-1,-1)@.

majorMinor :: GettableStateVar String -> GettableStateVar (Int, Int)
majorMinor =
  makeGettableStateVar . (runParser parseVersion (-1, -1) <$>) . S.get

--------------------------------------------------------------------------------
-- Copy from Graphics.Rendering.OpenGL.Raw.GetProcAddress... :-/

runParser :: ReadP a -> a -> String -> a
runParser parser failed str =
  case readP_to_S parser str of
    [(v, "")] -> v
    _ -> failed

-- This does quite a bit more than we need for "normal" OpenGL, but at least it
-- documents the convoluted format of the version string in detail.
parseVersion :: ReadP (Int, Int)
parseVersion = do
  _prefix <-
    -- Too lazy to define a type for the API...
    ("CL" <$ string "OpenGL ES-CL ") <++  -- OpenGL ES 1.x Common-Lite
    ("CM" <$ string "OpenGL ES-CM ") <++  -- OpenGL ES 1.x Common
    ("ES" <$ string "OpenGL ES "   ) <++  -- OpenGL ES 2.x or 3.x
    ("GL" <$ string ""             )      -- OpenGL
  major <- read <$> munch1 isDigit
  minor <- char '.' >> read <$> munch1 isDigit
  _release <- (char '.' >> munch1 (/= ' ')) <++ return ""
  _vendorStuff <- (char ' ' >> R.get `manyTill` eof) <++ ("" <$ eof)
  return (major, minor)
