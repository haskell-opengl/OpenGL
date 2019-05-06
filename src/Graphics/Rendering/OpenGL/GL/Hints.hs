--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Hints
-- Copyright   :  (c) Sven Panne 2002-2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 5.6 (Hints) of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Hints (
   HintTarget(..), HintMode(..), hint
) where

import Data.StateVar
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.GL

--------------------------------------------------------------------------------

data HintTarget =
     PerspectiveCorrection
   | PointSmooth
   | LineSmooth
   | PolygonSmooth
   | Fog
   | GenerateMipmap
   | TextureCompression
   | PackCMYK
   | UnpackCMYK
   deriving ( Eq, Ord, Show )

marshalHintTarget :: HintTarget -> GLenum
marshalHintTarget x = case x of
   PerspectiveCorrection -> GL_PERSPECTIVE_CORRECTION_HINT
   PointSmooth -> GL_POINT_SMOOTH_HINT
   LineSmooth -> GL_LINE_SMOOTH_HINT
   PolygonSmooth -> GL_POLYGON_SMOOTH_HINT
   Fog -> GL_FOG_HINT
   GenerateMipmap -> GL_GENERATE_MIPMAP_HINT
   TextureCompression -> GL_TEXTURE_COMPRESSION_HINT
   PackCMYK -> GL_PACK_CMYK_HINT_EXT
   UnpackCMYK -> GL_UNPACK_CMYK_HINT_EXT

hintTargetToGetPName :: HintTarget -> PName1I
hintTargetToGetPName x = case x of
   PerspectiveCorrection -> GetPerspectiveCorrectionHint
   PointSmooth -> GetPointSmoothHint
   LineSmooth -> GetLineSmoothHint
   PolygonSmooth -> GetPolygonSmoothHint
   Fog -> GetFogHint
   GenerateMipmap -> GetGenerateMipmapHint
   TextureCompression -> GetTextureCompressionHint
   PackCMYK -> GetPackCMYKHint
   UnpackCMYK -> GetUnpackCMYKHint

--------------------------------------------------------------------------------

data HintMode =
     DontCare
   | Fastest
   | Nicest
   deriving ( Eq, Ord, Show )

marshalHintMode :: HintMode -> GLenum
marshalHintMode x = case x of
   DontCare -> GL_DONT_CARE
   Fastest -> GL_FASTEST
   Nicest -> GL_NICEST

unmarshalHintMode :: GLenum -> HintMode
unmarshalHintMode x
   | x == GL_DONT_CARE = DontCare
   | x == GL_FASTEST = Fastest
   | x == GL_NICEST = Nicest
   | otherwise = error ("unmarshalHintMode: illegal value " ++ show x)

--------------------------------------------------------------------------------

hint ::  HintTarget -> StateVar HintMode
hint t =
   makeStateVar
      (getEnum1 unmarshalHintMode (hintTargetToGetPName t))
      (glHint (marshalHintTarget t) . marshalHintMode)
