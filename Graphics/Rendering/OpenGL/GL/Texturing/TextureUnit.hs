-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.TextureUnit
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling TextureUnit.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing.TextureUnit (
   TextureUnit(..), marshalTextureUnit, unmarshalTextureUnit
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLuint )

--------------------------------------------------------------------------------

-- | Identifies a texture unit via its number, which must be in the range of
-- (0 .. 'maxTextureUnit').

newtype TextureUnit = TextureUnit GLuint
   deriving ( Eq, Ord, Show )

marshalTextureUnit :: TextureUnit -> GLenum
marshalTextureUnit (TextureUnit x) = 0x84c0 + fromIntegral x

unmarshalTextureUnit :: GLenum -> TextureUnit
unmarshalTextureUnit x = TextureUnit (fromIntegral (x - 0x84c0))
