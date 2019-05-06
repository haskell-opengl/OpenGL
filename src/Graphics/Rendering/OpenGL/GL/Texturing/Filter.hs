{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Filter
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
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

import Graphics.GL

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
   (Nearest, Nothing     ) -> GL_NEAREST
   (Linear', Nothing     ) -> GL_LINEAR
   (Nearest, Just Nearest) -> GL_NEAREST_MIPMAP_NEAREST
   (Linear', Just Nearest) -> GL_LINEAR_MIPMAP_NEAREST
   (Nearest, Just Linear') -> GL_NEAREST_MIPMAP_LINEAR
   (Linear', Just Linear') -> GL_LINEAR_MIPMAP_LINEAR

marshalMagnificationFilter :: MagnificationFilter -> GLint
marshalMagnificationFilter = marshalMinificationFilter . magToMin

unmarshalMinificationFilter :: GLint -> MinificationFilter
unmarshalMinificationFilter x
   | y == GL_NEAREST = (Nearest, Nothing)
   | y == GL_LINEAR = (Linear', Nothing)
   | y == GL_NEAREST_MIPMAP_NEAREST = (Nearest, Just Nearest)
   | y == GL_LINEAR_MIPMAP_NEAREST = (Linear', Just Nearest)
   | y == GL_NEAREST_MIPMAP_LINEAR = (Nearest, Just Linear')
   | y == GL_LINEAR_MIPMAP_LINEAR = (Linear', Just Linear')
   | otherwise = error ("unmarshalMinificationFilter: illegal value " ++ show x)
   where y = fromIntegral x

unmarshalMagnificationFilter :: GLint -> MagnificationFilter
unmarshalMagnificationFilter = minToMag . unmarshalMinificationFilter
