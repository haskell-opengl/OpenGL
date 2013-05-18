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

import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

-- | Identifies a texture unit via its number, which must be in the range of
-- (0 .. 'maxTextureUnit').

newtype TextureUnit = TextureUnit GLuint
   deriving ( Eq, Ord, Show )

-- Internal note, when setting a sampler (TextureUnit) uniform the GLint
-- functions should be used.

instance Storable TextureUnit where
    sizeOf _                 = sizeOf    (undefined :: GLuint)
    alignment _              = alignment (undefined :: GLuint)
    peek pt                  = peek (castPtr pt) >>= return . TextureUnit
    poke pt (TextureUnit tu) = poke (castPtr pt) tu
    peekByteOff pt off       = peekByteOff pt off >>= return . TextureUnit
    pokeByteOff pt off (TextureUnit tu)
                             = pokeByteOff pt off tu



marshalTextureUnit :: TextureUnit -> GLenum
marshalTextureUnit (TextureUnit x) = gl_TEXTURE0 + fromIntegral x

unmarshalTextureUnit :: GLenum -> TextureUnit
unmarshalTextureUnit x = TextureUnit (fromIntegral (x - gl_TEXTURE0))
