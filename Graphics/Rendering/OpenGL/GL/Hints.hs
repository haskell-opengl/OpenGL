--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Hints
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 5.6 (Hints) of the OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Hints (
   HintTarget(..), HintMode(..), hint
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetPerspectiveCorrectionHint,GetPointSmoothHint,GetLineSmoothHint,
            GetPolygonSmoothHint,GetFogHint,GetGenerateMipmapHint,
            GetTextureCompressionHint,GetPackCMYKHint,GetUnpackCMYKHint),
   getEnum1 )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar, makeStateVar )

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
   PerspectiveCorrection -> 0xc50
   PointSmooth -> 0xc51
   LineSmooth -> 0xc52
   PolygonSmooth -> 0xc53
   Fog -> 0xc54
   GenerateMipmap -> 0x8192
   TextureCompression -> 0x84ef
   PackCMYK -> 0x800e
   UnpackCMYK -> 0x800f

hintTargetToGetPName :: HintTarget -> GetPName
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
   DontCare -> 0x1100
   Fastest -> 0x1101
   Nicest -> 0x1102

unmarshalHintMode :: GLenum -> HintMode
unmarshalHintMode x
   | x == 0x1100 = DontCare
   | x == 0x1101 = Fastest
   | x == 0x1102 = Nicest
   | otherwise = error ("unmarshalHintMode: illegal value " ++ show x)

--------------------------------------------------------------------------------

hint ::  HintTarget -> StateVar HintMode
hint t =
   makeStateVar
      (getEnum1 unmarshalHintMode (hintTargetToGetPName t))
      (glHint (marshalHintTarget t) . marshalHintMode)

foreign import CALLCONV unsafe "glHint" glHint :: GLenum -> GLenum -> IO ()
