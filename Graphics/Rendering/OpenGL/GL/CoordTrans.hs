--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.CoordTrans
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module corresponds to section 2.10 (Coordinate Transformations) of the
-- OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.CoordTrans(
   -- * Misc
   GLcolumn4(..), GLmatrix(..),

   -- * Controlling the Viewport
   depthRange, viewport, maxViewportDims,

   -- * Matrices
   MatrixMode(..), matrixMode
) where

import Control.Monad ( zipWithM_ )
import Foreign.Ptr ( castPtr )
import Foreign.Storable ( Storable(..) )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLint, GLsizei, GLclampd )
import Graphics.Rendering.OpenGL.GL.Query (
   GetPName(GetDepthRange,GetViewport,GetMaxViewportDims,GetMatrixMode),
   getInteger1, getInteger2, getInteger4, getDouble2 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )

--------------------------------------------------------------------------------

-- | After clipping and division by /w/, depth coordinates range from -1 to 1,
-- corresponding to the near and far clipping planes. 'depthRange' specifies a
-- linear mapping of the normalized depth coordinates in this range to window
-- depth coordinates. Regardless of the actual depth buffer implementation,
-- window coordinate depth values are treated as though they range from 0
-- through 1 (like color components). Thus, the values accepted by 'depthRange'
-- are both clamped to this range before they are accepted.
--
-- The initial setting of (0, 1) maps the near plane to 0 and the far plane to
-- 1. With this mapping, the depth buffer range is fully utilized.
--
-- It is not necessary that the near value be less than the far value. Reverse
-- mappings such as (1, 0) are acceptable.

depthRange :: StateVar (GLclampd, GLclampd)
depthRange = makeStateVar (getDouble2 (,) GetDepthRange) (uncurry glDepthRange)

foreign import CALLCONV unsafe "glDepthRange" glDepthRange ::
   GLclampd -> GLclampd -> IO ()

--------------------------------------------------------------------------------

-- | Controls the affine transformation from normalized device coordinates to
-- window coordinates. The viewport state variable consists of the coordinates
-- (/x/, /y/) of the lower left corner of the viewport rectangle, (in pixels,
-- initial value (0,0)), and the size (/width/, /height/) of the viewport. When
-- a GL context is first attached to a window, /width/ and /height/ are set to
-- the dimensions of that window.
--
-- Let (/xnd/, /ynd/) be normalized device- coordinates. Then the window
-- coordinates (/xw/, /yw/) are computed as follows:
--
-- /xw/ = (/xnd/ + 1) (/width/  \/ 2) + /x/
--
-- /yw/ = (/ynd/ + 1) (/heigth/ \/ 2) + /y/
--
-- Viewport width and height are silently clamped to a range that depends on the
-- implementation, see 'maxViewportDims'.

viewport :: StateVar ((GLint, GLint), (GLsizei, GLsizei))
viewport = makeStateVar (getInteger4 (\x y w h -> ((x,y), (w,h))) GetViewport)
                        (\((x,y), (w,h)) -> glViewport x y w h)

foreign import CALLCONV unsafe "glViewport" glViewport ::
   GLint -> GLint -> GLsizei -> GLsizei -> IO ()

-- | The implementation-dependent maximum viewport width and height.

maxViewportDims :: GettableStateVar (GLsizei, GLsizei)
maxViewportDims = makeGettableStateVar (getInteger2 (,) GetMaxViewportDims)

--------------------------------------------------------------------------------

-- | A matrix stack.

data MatrixMode =
     Modelview GLsizei  -- ^ The modelview matrix stack of the specified vertex unit.
   | Projection         -- ^ The projection matrix stack.
   | Texture            -- ^ The texture matrix stack.
   | MatrixPalette      -- ^ The palette matrix stack.
   deriving ( Eq, Ord, Show )

marshalMatrixMode :: MatrixMode -> GLenum
marshalMatrixMode x = case x of
   Modelview i
      | i == 0    -> 0x1700
      | i == 1    -> 0x850a
      | i <= 31   -> 0x8722 + fromIntegral i
      | otherwise -> error ("marshalMatrixMode: illegal value" ++ show i)
   Projection -> 0x1701
   Texture -> 0x1702
   MatrixPalette -> 0x8840

unmarshalMatrixMode :: GLenum -> MatrixMode
unmarshalMatrixMode x
   | x == 0x1700 = Modelview 0
   | x == 0x850a = Modelview 1
   | 0x8722 <= x && x <= 0x873f = Modelview (fromIntegral x - 0x8722)
   | x == 0x1701 = Projection
   | x == 0x1702 = Texture
   | x == 0x8840 = MatrixPalette
   | otherwise = error ("unmarshalMatrixMode: illegal value " ++ show x)

--------------------------------------------------------------------------------

-- | Controls which matrix stack is the target for subsequent matrix operations.
-- The initial value is ('Modelview' 0).

matrixMode :: StateVar MatrixMode
matrixMode =
   makeStateVar (getInteger1 (unmarshalMatrixMode . fromIntegral) GetMatrixMode)
                (glMatrixMode . marshalMatrixMode)

foreign import CALLCONV unsafe "glMatrixMode" glMatrixMode :: GLenum -> IO ()

--------------------------------------------------------------------------------

data GLcolumn4 a = GLcolumn4 a a a a

data GLmatrix a = GLmatrix (GLcolumn4 a) (GLcolumn4 a) (GLcolumn4 a) (GLcolumn4 a)

instance Storable a => Storable (GLmatrix a) where
   sizeOf    ~(GLmatrix (GLcolumn4 x _ _ _) _ _ _) = 16 * sizeOf x
   alignment ~(GLmatrix (GLcolumn4 x _ _ _) _ _ _) = alignment x

   peek ptr = do
      [ a00, a01, a02, a03,
        a04, a05, a06, a07,
        a08, a09, a10, a11,
        a12, a13, a14, a15 ] <- mapM (peekElemOff (castPtr ptr)) [ 0 .. 15 ]
      return $ GLmatrix (GLcolumn4 a00 a01 a02 a03)
                        (GLcolumn4 a04 a05 a06 a07)
                        (GLcolumn4 a08 a09 a10 a11)
                        (GLcolumn4 a12 a13 a14 a15)

   poke ptr (GLmatrix (GLcolumn4 a00 a01 a02 a03)
                      (GLcolumn4 a04 a05 a06 a07)
                      (GLcolumn4 a08 a09 a10 a11)
                      (GLcolumn4 a12 a13 a14 a15)) =
      zipWithM_ (pokeElemOff (castPtr ptr)) [ 0 .. ]
                [ a00, a01, a02, a03,
                  a04, a05, a06, a07,
                  a08, a09, a10, a11,
                  a12, a13, a14, a15 ]
