module Graphics.Rendering.OpenGL.GL.Query where

---------------------------------------------------------------------------

-- Sigh... Once again Microsoft does what it always does to standards, i.e.
-- it ignores them. The GL/GLU specs dictate that the version number should
-- either have the form major.minor or major.minor.release, where major,
-- minor, and release consist of digits only. But alas, M$'s GLU on Win98
-- returns 1.2.2.0, which is invalid. So we revert to a simple string for
-- the version number.

data VersionInfo
   = VersionInfo String        -- major.minor[.release]
                 String        -- vendor
   deriving ( Eq, Ord )

-- Nevertheless, we keep this function, just in case...

parseVersionString :: String -> String -> VersionInfo
parseVersionString _ versionString =
   uncurry VersionInfo . break (' ' ==) $ versionString

---------------------------------------------------------------------------
-- We split the extensions string into words, so there is no need
-- for gluCheckExtension anymore.

data ExtensionsInfo = ExtensionsInfo [String]
   deriving ( Eq, Ord )
