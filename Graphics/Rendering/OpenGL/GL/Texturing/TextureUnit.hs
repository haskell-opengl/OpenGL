-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.TextureUnit
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling TextureUnit.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing.TextureUnit (
   TextureUnit(..), marshalTextureUnit, unmarshalTextureUnit
) where

import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

-- | Identifies a texture unit via its number, which must be in the range of
-- (0 .. 'maxTextureUnit').

newtype TextureUnit = TextureUnit GLuint
   deriving ( Eq, Ord, Show )

marshalTextureUnit :: TextureUnit -> GLenum
marshalTextureUnit (TextureUnit x) = gl_TEXTURE0 + fromIntegral x

unmarshalTextureUnit :: GLenum -> TextureUnit
unmarshalTextureUnit x = TextureUnit (fromIntegral (x - gl_TEXTURE0))
