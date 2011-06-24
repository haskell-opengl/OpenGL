-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Rendering.OpenGL.GL.PixellikeObject
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <sven.panne@aedion.de>
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PixellikeObject (
  PixellikeObjectGetPName(..),
  PixellikeObjectTarget(..),
) where

import Data.StateVar
import Graphics.Rendering.OpenGL.Raw.Core31

data PixellikeObjectGetPName =
     RedSize
   | BlueSize
   | GreenSize
   | AlphaSize
   | DepthSize
   | StencilSize

class PixellikeObjectTarget t where
   --dummy t to include it in the type class
   marshalPixellikeOT :: t -> PixellikeObjectGetPName -> GLenum
   getterFuncPOT :: t -> GLenum -> IO GLint
   pixellikeOTParam :: t -> PixellikeObjectGetPName -> GettableStateVar GLint
   pixellikeOTParam t p = makeGettableStateVar (getterFuncPOT t $ marshalPixellikeOT t p)
