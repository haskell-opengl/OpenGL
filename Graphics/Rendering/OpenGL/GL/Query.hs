-- #prune
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Query
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module corresponds to section 6.1 (Querying GL State) of the OpenGL 1.4
-- specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Query (
   VersionInfo(..),
   parseVersionString,                          -- used only internally
   ExtensionsInfo(..)
) where

--------------------------------------------------------------------------------

-- Sigh... Once again Microsoft does what it always does to standards, i.e.
-- it ignores them. The GL/GLU specs dictate that the version number should
-- either have the form major.minor or major.minor.release, where major,
-- minor, and release consist of digits only. But alas, M$'s GLU on Win98
-- returns 1.2.2.0, which is invalid. So we revert to a simple string for
-- the version number.
-- ToDo: Handle this differently by ignoring the trailing part.

-- | A version consists of a /major/./minor/[./release/] part and a vendor
-- name.

data VersionInfo
   = VersionInfo String        -- major.minor[.release]
                 String        -- vendor
   deriving ( Eq, Ord, Show )

-- Nevertheless, we keep this function, just in case...

parseVersionString :: String -> String -> VersionInfo
parseVersionString _ versionString =
   uncurry VersionInfo . break (' ' ==) $ versionString

--------------------------------------------------------------------------------

-- | A list of extension names.

data ExtensionsInfo = ExtensionsInfo [String]
   deriving ( Eq, Ord, Show )
