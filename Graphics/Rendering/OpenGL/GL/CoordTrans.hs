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

module Graphics.Rendering.OpenGL.GL.CoordTrans (
   -- * Misc
   GLcolumn4(..), GLmatrix(..),

   -- * Controlling the Viewport
   depthRange, viewport, maxViewportDims,

   -- * Matrices
   MatrixMode(..), matrixMode,
   glLoadMatrixf, glLoadMatrixd, loadTransposeMatrixf, loadTransposeMatrixd,
   glMultMatrixf, glMultMatrixd, multTransposeMatrixf, multTransposeMatrixd,
   loadIdentity,
   glRotatef, glRotated, glTranslatef, glTranslated, glScalef, glScaled,
   ortho, frustum,
   activeTexture,
   matrixExcursion,

   -- * Normal Transformation
   rescaleNormal, normalize,

   -- * Generating Texture Coordinates
   glTexGeni, glTexGenf, glTexGend, 
   glTexGeniv, glTexGenfv, glTexGendv, 
   glGetTexGeniv, glGetTexGenfv, glGetTexGendv,
) where

import Control.Monad ( zipWithM_ )
import Foreign.Ptr ( Ptr, castPtr )
import Foreign.Storable ( Storable(..) )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLint, GLsizei, GLfloat, GLdouble, GLclampd )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapRescaleNormal,CapNormalize), makeCapability )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.Query (
   GetPName(GetDepthRange,GetViewport,GetMaxViewportDims,GetMatrixMode,
            GetActiveTexture),
   getInteger1, getInteger2, getInteger4, getDouble2 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.VertexSpec (
   TextureUnit(TextureUnit) )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"

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

foreign import CALLCONV unsafe "glLoadMatrixf" glLoadMatrixf :: Ptr GLfloat -> IO ()
foreign import CALLCONV unsafe "glLoadMatrixd" glLoadMatrixd :: Ptr GLdouble -> IO ()

loadTransposeMatrixf :: Ptr GLfloat -> IO ()
loadTransposeMatrixf = dynLoadTransposeMatrixf ptrLoadTransposeMatrixf

EXTENSION_ENTRY("GL_ARB_transpose_matrix","glLoadTransposeMatrixf",dynLoadTransposeMatrixf,ptrLoadTransposeMatrixf,Ptr GLfloat -> IO ())

loadTransposeMatrixd :: Ptr GLdouble -> IO ()
loadTransposeMatrixd = dynLoadTransposeMatrixd ptrLoadTransposeMatrixd

EXTENSION_ENTRY("GL_ARB_transpose_matrix","glLoadTransposeMatrixd",dynLoadTransposeMatrixd,ptrLoadTransposeMatrixd,Ptr GLdouble -> IO ())

foreign import CALLCONV unsafe "glMultMatrixf" glMultMatrixf :: Ptr GLfloat -> IO ()
foreign import CALLCONV unsafe "glMultMatrixd" glMultMatrixd :: Ptr GLdouble -> IO ()

multTransposeMatrixf :: Ptr GLfloat -> IO ()
multTransposeMatrixf = dynMultTransposeMatrixf ptrMultTransposeMatrixf

EXTENSION_ENTRY("GL_ARB_transpose_matrix","glMultTransposeMatrixf",dynMultTransposeMatrixf,ptrMultTransposeMatrixf,Ptr GLfloat -> IO ())

multTransposeMatrixd :: Ptr GLdouble -> IO ()
multTransposeMatrixd = dynMultTransposeMatrixd ptrMultTransposeMatrixd

EXTENSION_ENTRY("GL_ARB_transpose_matrix","glMultTransposeMatrixd",dynMultTransposeMatrixd,ptrMultTransposeMatrixd,Ptr GLdouble -> IO ())

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glLoadIdentity" loadIdentity :: IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glRotatef" glRotatef :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
foreign import CALLCONV unsafe "glRotated" glRotated :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glTranslatef" glTranslatef :: GLfloat -> GLfloat -> GLfloat -> IO ()
foreign import CALLCONV unsafe "glTranslated" glTranslated :: GLdouble -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glScalef" glScalef :: GLfloat -> GLfloat -> GLfloat -> IO ()
foreign import CALLCONV unsafe "glScaled" glScaled :: GLdouble -> GLdouble -> GLdouble -> IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glOrtho" ortho ::
   GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glFrustum" frustum ::
   GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

--------------------------------------------------------------------------------

activeTexture :: StateVar TextureUnit
activeTexture =
   makeStateVar
     (getInteger1 (TextureUnit . fromIntegral) GetActiveTexture)
     (\(TextureUnit u) -> glActiveTexture u)

foreign import CALLCONV unsafe "glActiveTexture" glActiveTexture ::
   GLenum -> IO ()

--------------------------------------------------------------------------------

-- ToDo: Use bracket? Guard against overflow?

matrixExcursion :: IO a -> IO a
matrixExcursion act = do
   glPushMatrix
   ret <- act
   glPopMatrix
   return ret

foreign import CALLCONV unsafe "glPushMatrix" glPushMatrix :: IO ()

foreign import CALLCONV unsafe "glPopMatrix" glPopMatrix :: IO ()

--------------------------------------------------------------------------------

rescaleNormal :: StateVar Bool
rescaleNormal = makeCapability CapRescaleNormal

normalize :: StateVar Bool
normalize = makeCapability CapNormalize

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glTexGeni" glTexGeni :: GLenum -> GLenum -> GLint -> IO ()
foreign import CALLCONV unsafe "glTexGenf" glTexGenf :: GLenum -> GLenum -> GLfloat -> IO ()
foreign import CALLCONV unsafe "glTexGend" glTexGend :: GLenum -> GLenum -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glTexGeniv" glTexGeniv :: GLenum -> GLenum -> Ptr GLint -> IO ()
foreign import CALLCONV unsafe "glTexGenfv" glTexGenfv :: GLenum -> GLenum -> Ptr GLfloat -> IO ()
foreign import CALLCONV unsafe "glTexGendv" glTexGendv :: GLenum -> GLenum -> Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glGetTexGeniv" glGetTexGeniv :: GLenum -> GLenum -> Ptr GLint -> IO ()
foreign import CALLCONV unsafe "glGetTexGenfv" glGetTexGenfv :: GLenum -> GLenum -> Ptr GLfloat -> IO ()
foreign import CALLCONV unsafe "glGetTexGendv" glGetTexGendv :: GLenum -> GLenum -> Ptr GLdouble -> IO ()

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
