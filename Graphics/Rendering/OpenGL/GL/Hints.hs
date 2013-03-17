--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Hints
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 5.6 (Hints) of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Hints (
   HintTarget(..), HintMode(..), hint
) where

import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (
   gl_FOG_HINT, gl_GENERATE_MIPMAP_HINT, gl_PERSPECTIVE_CORRECTION_HINT,
   gl_POINT_SMOOTH_HINT )
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.EXT.Cmyka (
   gl_PACK_CMYK_HINT, gl_UNPACK_CMYK_HINT )

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
   PerspectiveCorrection -> gl_PERSPECTIVE_CORRECTION_HINT
   PointSmooth -> gl_POINT_SMOOTH_HINT
   LineSmooth -> gl_LINE_SMOOTH_HINT
   PolygonSmooth -> gl_POLYGON_SMOOTH_HINT
   Fog -> gl_FOG_HINT
   GenerateMipmap -> gl_GENERATE_MIPMAP_HINT
   TextureCompression -> gl_TEXTURE_COMPRESSION_HINT
   PackCMYK -> gl_PACK_CMYK_HINT
   UnpackCMYK -> gl_UNPACK_CMYK_HINT

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
   DontCare -> gl_DONT_CARE
   Fastest -> gl_FASTEST
   Nicest -> gl_NICEST

unmarshalHintMode :: GLenum -> HintMode
unmarshalHintMode x
   | x == gl_DONT_CARE = DontCare
   | x == gl_FASTEST = Fastest
   | x == gl_NICEST = Nicest
   | otherwise = error ("unmarshalHintMode: illegal value " ++ show x)

--------------------------------------------------------------------------------

hint ::  HintTarget -> StateVar HintMode
hint t =
   makeStateVar
      (getEnum1 unmarshalHintMode (hintTargetToGetPName t))
      (glHint (marshalHintTarget t) . marshalHintMode)
