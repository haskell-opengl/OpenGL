-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Filter
-- Copyright   :  (c) Sven Panne 2013
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
--
-- Maintainer  :  svenpanne@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling texture filtering modes.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing.Filter (
   TextureFilter(..),
   MinificationFilter, marshalMinificationFilter, unmarshalMinificationFilter,
   MagnificationFilter, marshalMagnificationFilter, unmarshalMagnificationFilter
) where

import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

data TextureFilter =
     Nearest
   | Linear'
   deriving ( Eq, Ord, Show )

type MinificationFilter = (TextureFilter, Maybe TextureFilter)

type MagnificationFilter = TextureFilter

-- We treat MagnificationFilter as a degenerated case of MinificationFilter
magToMin :: MagnificationFilter -> MinificationFilter
magToMin magFilter = (magFilter, Nothing)

minToMag :: MinificationFilter -> MagnificationFilter
minToMag (magFilter, Nothing) = magFilter
minToMag minFilter = error ("minToMag: illegal value " ++ show minFilter)

marshalMinificationFilter :: MinificationFilter -> GLint
marshalMinificationFilter x = fromIntegral $ case x of
   (Nearest, Nothing     ) -> gl_NEAREST
   (Linear', Nothing     ) -> gl_LINEAR
   (Nearest, Just Nearest) -> gl_NEAREST_MIPMAP_NEAREST
   (Linear', Just Nearest) -> gl_LINEAR_MIPMAP_NEAREST
   (Nearest, Just Linear') -> gl_NEAREST_MIPMAP_LINEAR
   (Linear', Just Linear') -> gl_LINEAR_MIPMAP_LINEAR

marshalMagnificationFilter :: MagnificationFilter -> GLint
marshalMagnificationFilter = marshalMinificationFilter . magToMin

unmarshalMinificationFilter :: GLint -> MinificationFilter
unmarshalMinificationFilter x
   | y ==  gl_NEAREST = (Nearest, Nothing)
   | y ==  gl_LINEAR = (Linear', Nothing)
   | y ==  gl_NEAREST_MIPMAP_NEAREST = (Nearest, Just Nearest)
   | y ==  gl_LINEAR_MIPMAP_NEAREST = (Linear', Just Nearest)
   | y ==  gl_NEAREST_MIPMAP_LINEAR = (Nearest, Just Linear')
   | y ==  gl_LINEAR_MIPMAP_LINEAR = (Linear', Just Linear')
   | otherwise = error ("unmarshalMinificationFilter: illegal value " ++ show x)
   where y = fromIntegral x

unmarshalMagnificationFilter :: GLint -> MagnificationFilter
unmarshalMagnificationFilter = minToMag . unmarshalMinificationFilter
