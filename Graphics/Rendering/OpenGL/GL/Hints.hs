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
   HintMode(..),
   perspectiveCorrectionHint, pointSmoothHint, lineSmoothHint,
   polygonSmoothHint, fogHint, generateMipmapHint, textureCompressionHint
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetPerspectiveCorrectionHint,GetPointSmoothHint,GetLineSmoothHint,
            GetPolygonSmoothHint,GetFogHint,GetGenerateMipmapHint,
            GetTextureCompressionHint),
   getInteger1 )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar, makeStateVar )

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

data HintTarget =
     PerspectiveCorrectionHint
   | PointSmoothHint
   | LineSmoothHint
   | PolygonSmoothHint
   | FogHint
   | GenerateMipmapHint
   | TextureCompressionHint

marshalHintTarget :: HintTarget -> GLenum
marshalHintTarget x = case x of
   PerspectiveCorrectionHint -> 0xc50
   PointSmoothHint -> 0xc51
   LineSmoothHint -> 0xc52
   PolygonSmoothHint -> 0xc53
   FogHint -> 0xc54
   GenerateMipmapHint -> 0x8192
   TextureCompressionHint -> 0x84ef

--------------------------------------------------------------------------------

makeHint ::  GetPName -> HintTarget -> StateVar HintMode
makeHint p t =
   makeStateVar
      (getInteger1 (unmarshalHintMode . fromIntegral) p)
      (glHint (marshalHintTarget t) . marshalHintMode)

foreign import CALLCONV unsafe "glHint" glHint :: GLenum -> GLenum -> IO ()

--------------------------------------------------------------------------------

perspectiveCorrectionHint :: StateVar HintMode
perspectiveCorrectionHint =
   makeHint GetPerspectiveCorrectionHint PerspectiveCorrectionHint

pointSmoothHint :: StateVar HintMode
pointSmoothHint = makeHint GetPointSmoothHint PointSmoothHint

lineSmoothHint :: StateVar HintMode
lineSmoothHint = makeHint GetLineSmoothHint LineSmoothHint

polygonSmoothHint :: StateVar HintMode
polygonSmoothHint = makeHint GetPolygonSmoothHint PolygonSmoothHint

fogHint :: StateVar HintMode
fogHint = makeHint GetFogHint FogHint

generateMipmapHint :: StateVar HintMode
generateMipmapHint = makeHint GetGenerateMipmapHint GenerateMipmapHint

textureCompressionHint :: StateVar HintMode
textureCompressionHint =
   makeHint GetTextureCompressionHint TextureCompressionHint
