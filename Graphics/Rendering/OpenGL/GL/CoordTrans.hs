--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.CoordTrans
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 2.10 (Coordinate Transformations) of the
-- OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.CoordTrans (
   -- * Controlling the Viewport
   depthRange,
   Position(..), Size(..), viewport, maxViewportDims,

   -- * Matrices
   MatrixMode(..), matrixMode,
   Vector3(..),
   MatrixOrder(..), MatrixComponent(rotate,translate,scale), Matrix(..),
   currentMatrix, multMatrix, GLmatrix, loadIdentity,
   ortho, frustum, depthClamp,
   activeTexture,
   preservingMatrix, unsafePreservingMatrix,
   stackDepth, maxStackDepth,

   -- * Normal Transformation
   rescaleNormal, normalize,

   -- * Generating Texture Coordinates
   Plane(..), TextureCoordName(..), TextureGenMode(..), textureGenMode
) where

import Control.Monad ( liftM )
import Foreign.ForeignPtr ( ForeignPtr, mallocForeignPtrArray, withForeignPtr )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( allocaArray, peekArray, pokeArray, withArray )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr, castPtr )
import Foreign.Storable ( Storable(..) )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapRescaleNormal, CapNormalize,CapDepthClamp,
             CapTextureGenS, CapTextureGenT,
             CapTextureGenR, CapTextureGenQ),
   makeCapability, makeStateVarMaybe )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLint, GLsizei, GLfloat, GLdouble, GLclampd, Capability(..) )
import Graphics.Rendering.OpenGL.GL.Exception ( finally )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.PeekPoke (
   peek1, peek3, peek4, poke3, poke4 )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetDepthRange,GetViewport,GetMaxViewportDims,GetMatrixMode,
            GetModelviewMatrix,GetProjectionMatrix,GetTextureMatrix,
            GetColorMatrix,GetMatrixPalette,GetActiveTexture,
            GetModelviewStackDepth,GetMaxModelviewStackDepth,
            GetProjectionStackDepth,GetMaxProjectionStackDepth,
            GetTextureStackDepth,GetMaxTextureStackDepth,
            GetColorMatrixStackDepth,GetMaxColorMatrixStackDepth,
            GetMaxMatrixPaletteStackDepth),
   getInteger2, getInteger4, getEnum1, getSizei1, getFloatv,
   getDouble2, getDoublev )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasGetter(get), GettableStateVar, makeGettableStateVar,
   StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.VertexSpec (
   TextureUnit(TextureUnit) )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"
#include "HsOpenGLTypes.h"

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

-- | A 2-dimensional position, measured in pixels.
data Position = Position GLint GLint
   deriving ( Eq, Ord, Show )

-- | A 2-dimensional size, measured in pixels.
data Size = Size GLsizei GLsizei
   deriving ( Eq, Ord, Show )

-- | Controls the affine transformation from normalized device coordinates to
-- window coordinates. The viewport state variable consists of the coordinates
-- (/x/, /y/) of the lower left corner of the viewport rectangle, (in pixels,
-- initial value (0,0)), and the size (/width/, /height/) of the viewport. When
-- a GL context is first attached to a window, /width/ and /height/ are set to
-- the dimensions of that window.
--
-- Let (/xnd/, /ynd/) be normalized device coordinates. Then the window
-- coordinates (/xw/, /yw/) are computed as follows:
--
-- /xw/ = (/xnd/ + 1) (/width/  \/ 2) + /x/
--
-- /yw/ = (/ynd/ + 1) (/heigth/ \/ 2) + /y/
--
-- Viewport width and height are silently clamped to a range that depends on the
-- implementation, see 'maxViewportDims'.

viewport :: StateVar (Position, Size)
viewport = makeStateVar (getInteger4 makeVp GetViewport)
                        (\(Position x y, Size w h) -> glViewport x y w h)
   where makeVp x y w h = (Position x y, Size (fromIntegral w) (fromIntegral h))

foreign import CALLCONV unsafe "glViewport" glViewport ::
   GLint -> GLint -> GLsizei -> GLsizei -> IO ()

-- | The implementation-dependent maximum viewport width and height.

maxViewportDims :: GettableStateVar Size
maxViewportDims = makeGettableStateVar (getInteger2 Size GetMaxViewportDims)

--------------------------------------------------------------------------------

-- | A matrix stack.

data MatrixMode =
     Modelview GLsizei  -- ^ The modelview matrix stack of the specified vertex unit.
   | Projection         -- ^ The projection matrix stack.
   | Texture            -- ^ The texture matrix stack.
   | Color              -- ^ The color matrix stack.
   | MatrixPalette      -- ^ The matrix palette stack.
   deriving ( Eq, Ord, Show )

marshalMatrixMode :: MatrixMode -> GLenum
marshalMatrixMode x = case x of
   Modelview i
      | i == 0    -> 0x1700
      | i == 1    -> 0x850a
      | i <= 31   -> 0x8722 + fromIntegral i
      | otherwise -> error ("marshalMatrixMode: illegal value " ++ show i)
   Projection -> 0x1701
   Texture -> 0x1702
   Color -> 0x1800
   MatrixPalette -> 0x8840

unmarshalMatrixMode :: GLenum -> MatrixMode
unmarshalMatrixMode x
   | x == 0x1700 = Modelview 0
   | x == 0x850a = Modelview 1
   | 0x8722 <= x && x <= 0x873f = Modelview (fromIntegral x - 0x8722)
   | x == 0x1701 = Projection
   | x == 0x1702 = Texture
   | x == 0x1800 = Color
   | x == 0x8840 = MatrixPalette
   | otherwise = error ("unmarshalMatrixMode: illegal value " ++ show x)

matrixModeToGetMatrix :: MatrixMode -> GetPName
matrixModeToGetMatrix x = case x of
   Modelview _   -> GetModelviewMatrix -- ???
   Projection    -> GetProjectionMatrix
   Texture       -> GetTextureMatrix
   Color         -> GetColorMatrix
   MatrixPalette -> GetMatrixPalette

matrixModeToGetStackDepth :: MatrixMode -> GetPName
matrixModeToGetStackDepth x =  case x of
   Modelview _   -> GetModelviewStackDepth
   Projection    -> GetProjectionStackDepth
   Texture       -> GetTextureStackDepth
   Color         -> GetColorMatrixStackDepth
   MatrixPalette ->
    error "OpenGL provides no way to get the current matrix palette stack depth"

matrixModeToGetMaxStackDepth :: MatrixMode -> GetPName
matrixModeToGetMaxStackDepth x = case x of
   Modelview _    -> GetMaxModelviewStackDepth
   Projection     -> GetMaxProjectionStackDepth
   Texture        -> GetMaxTextureStackDepth
   Color          -> GetMaxColorMatrixStackDepth
   MatrixPalette  -> GetMaxMatrixPaletteStackDepth

--------------------------------------------------------------------------------

-- | Controls which matrix stack is the target for subsequent matrix operations.
-- The initial value is ('Modelview' 0).

matrixMode :: StateVar MatrixMode
matrixMode =
   makeStateVar (getEnum1 unmarshalMatrixMode GetMatrixMode)
                (glMatrixMode . marshalMatrixMode)

foreign import CALLCONV unsafe "glMatrixMode" glMatrixMode :: GLenum -> IO ()

--------------------------------------------------------------------------------

data Vector3 a = Vector3 a a a
   deriving ( Eq, Ord, Show )

instance Storable a => Storable (Vector3 a) where
   sizeOf    ~(Vector3 x _ _) = 3 * sizeOf x
   alignment ~(Vector3 x _ _) = alignment x
   peek                       = peek3 Vector3 . castPtr
   poke ptr   (Vector3 x y z) = poke3 (castPtr ptr) x y z

--------------------------------------------------------------------------------

data MatrixOrder = ColumnMajor | RowMajor
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

class Storable c => MatrixComponent c where
   getMatrix :: GetPName -> Ptr c -> IO ()
   loadMatrix :: Ptr c -> IO ()
   loadTransposeMatrix :: Ptr c -> IO ()
   multMatrix_ :: Ptr c -> IO ()
   multTransposeMatrix :: Ptr c -> IO ()
   rotate :: c -> Vector3 c -> IO ()
   translate :: Vector3 c -> IO ()
   scale :: c -> c -> c -> IO ()

instance MatrixComponent GLfloat_ where
   getMatrix = getFloatv
   loadMatrix = glLoadMatrixf
   loadTransposeMatrix = glLoadTransposeMatrixfARB
   multMatrix_ = glMultMatrixf
   multTransposeMatrix = glMultTransposeMatrixfARB
   rotate a (Vector3 x y z) = glRotatef a x y z
   translate (Vector3 x y z) = glTranslatef x y z
   scale = glScalef

instance MatrixComponent GLdouble_ where
   getMatrix = getDoublev
   loadMatrix = glLoadMatrixd
   loadTransposeMatrix = glLoadTransposeMatrixdARB
   multMatrix_ = glMultMatrixd
   multTransposeMatrix = glMultTransposeMatrixdARB
   rotate a (Vector3 x y z) = glRotated a x y z
   translate (Vector3 x y z) = glTranslated x y z
   scale = glScaled

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glLoadMatrixf" glLoadMatrixf :: Ptr GLfloat -> IO ()
foreign import CALLCONV unsafe "glLoadMatrixd" glLoadMatrixd :: Ptr GLdouble -> IO ()

EXTENSION_ENTRY("GL_ARB_transpose_matrix or OpenGL 1.3",glLoadTransposeMatrixfARB,Ptr GLfloat -> IO ())
EXTENSION_ENTRY("GL_ARB_transpose_matrix or OpenGL 1.3",glLoadTransposeMatrixdARB,Ptr GLdouble -> IO ())

foreign import CALLCONV unsafe "glMultMatrixf" glMultMatrixf :: Ptr GLfloat -> IO ()
foreign import CALLCONV unsafe "glMultMatrixd" glMultMatrixd :: Ptr GLdouble -> IO ()

EXTENSION_ENTRY("GL_ARB_transpose_matrix or OpenGL 1.3",glMultTransposeMatrixfARB,Ptr GLfloat -> IO ())
EXTENSION_ENTRY("GL_ARB_transpose_matrix or OpenGL 1.3",glMultTransposeMatrixdARB,Ptr GLdouble -> IO ())

foreign import CALLCONV unsafe "glRotatef" glRotatef :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
foreign import CALLCONV unsafe "glRotated" glRotated :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glTranslatef" glTranslatef :: GLfloat -> GLfloat -> GLfloat -> IO ()
foreign import CALLCONV unsafe "glTranslated" glTranslated :: GLdouble -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glScalef" glScalef :: GLfloat -> GLfloat -> GLfloat -> IO ()
foreign import CALLCONV unsafe "glScaled" glScaled :: GLdouble -> GLdouble -> GLdouble -> IO ()

--------------------------------------------------------------------------------

class Matrix m where
   -- | Create a new matrix of the given order (containing undefined elements)
   -- and call the action to fill it with 4x4 elements.
   withNewMatrix ::
      MatrixComponent c => MatrixOrder -> (Ptr c -> IO ()) -> IO (m c)

   -- | Call the action with the given matrix. /Note:/ The action is /not/
   -- allowed to modify the matrix elements!
   withMatrix ::
      MatrixComponent c => m c -> (MatrixOrder -> Ptr c -> IO a) -> IO a

   newMatrix :: MatrixComponent c => MatrixOrder -> [c] -> IO (m c)
   getMatrixComponents :: MatrixComponent c => MatrixOrder -> m c -> IO [c]

   withNewMatrix order act =
      allocaArray 16 $ \p -> do
         act p
         components <- peekArray 16 p
         newMatrix order components

   withMatrix mat act = do
      components <- getMatrixComponents ColumnMajor mat
      withArray components $ act ColumnMajor

   newMatrix order components =
      withNewMatrix order $ flip pokeArray (take 16 components)

   getMatrixComponents desiredOrder mat =
      withMatrix mat $ \order p ->
        if desiredOrder == order
           then peekArray 16 p
           else mapM (peekElemOff p) [ 0, 4,  8, 12,
                                       1, 5,  9, 13,
                                       2, 6, 10, 14,
                                       3, 7, 11, 15 ]

--------------------------------------------------------------------------------

currentMatrix :: (Matrix m, MatrixComponent c) => StateVar (m c)
currentMatrix =
   makeStateVar
      (do mode <- get matrixMode
          withNewMatrix ColumnMajor $ getMatrix (matrixModeToGetMatrix mode))
      (\mat -> withMatrix mat $ \order ->
         case order of
            ColumnMajor -> loadMatrix
            RowMajor    -> loadTransposeMatrix)

multMatrix :: (Matrix m, MatrixComponent c) => m c -> IO ()
multMatrix mat =
   withMatrix mat $ \order ->
      case order of
         ColumnMajor -> multMatrix_
         RowMajor    -> multTransposeMatrix

--------------------------------------------------------------------------------

data GLmatrix a = GLmatrix MatrixOrder (ForeignPtr a)
#ifdef __HADDOCK__
-- Help Haddock a bit, because it doesn't do any instance inference.
instance Eq (GLmatrix a)
instance Ord (GLmatrix a)
instance Show (GLmatrix a)
#else
   deriving ( Eq, Ord, Show )
#endif

instance Matrix GLmatrix where
   withNewMatrix order f = do
      fp <- mallocForeignPtrArray 16
      withForeignPtr fp f
      return $ GLmatrix order fp

   withMatrix (GLmatrix order fp) f = withForeignPtr fp (f order)

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glLoadIdentity" loadIdentity :: IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glOrtho" ortho ::
   GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glFrustum" frustum ::
   GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

--------------------------------------------------------------------------------

depthClamp :: StateVar Capability
depthClamp = makeCapability CapDepthClamp

--------------------------------------------------------------------------------

activeTexture :: StateVar TextureUnit
activeTexture = makeStateVar (getEnum1 TextureUnit GetActiveTexture)
                             (\(TextureUnit u) -> glActiveTextureARB u)

EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glActiveTextureARB,GLenum -> IO ())   

--------------------------------------------------------------------------------

-- | Push the current matrix stack down by one, duplicating the current matrix,
-- excute the given action, and pop the current matrix stack, replacing the
-- current matrix with the one below it on the stack (i.e. restoring it to its
-- previous state). The returned value is that of the given action.

preservingMatrix :: IO a -> IO a
preservingMatrix action = do
   -- performance paranoia: No (un-)marshaling by avoiding matrixMode
   mode <- getEnum1 id GetMatrixMode
   (do glPushMatrix ; action) `finally` (do glMatrixMode mode ; glPopMatrix)

foreign import CALLCONV unsafe "glPushMatrix" glPushMatrix :: IO ()

foreign import CALLCONV unsafe "glPopMatrix" glPopMatrix :: IO ()

-- | A more efficient, but potentially dangerous version of 'preservingMatrix':
-- The given action is not allowed to throw an exception or change the
-- current matrix mode permanently.

unsafePreservingMatrix :: IO a -> IO a
unsafePreservingMatrix action = do
   glPushMatrix
   ret <- action
   glPopMatrix
   return ret

--------------------------------------------------------------------------------

stackDepth :: MatrixMode -> GettableStateVar GLsizei
stackDepth = makeGettableStateVar . getSizei1 id . matrixModeToGetStackDepth

maxStackDepth :: MatrixMode -> GettableStateVar GLsizei
maxStackDepth =
   makeGettableStateVar . getSizei1 id . matrixModeToGetMaxStackDepth

--------------------------------------------------------------------------------

-- | If 'rescaleNormal' contains 'Enabled', normal vectors specified with
-- 'Graphics.Rendering.OpenGL.GL.VertexSpec.normal' are scaled by a scaling
-- factor derived from the modelview matrix. 'rescaleNormal' requires that the
-- originally specified normals were of unit length, and that the modelview
-- matrix contains only uniform scales for proper results. The initial value of
-- 'rescaleNormal' is 'Disabled'.

rescaleNormal :: StateVar Capability
rescaleNormal = makeCapability CapRescaleNormal

-- | If 'normalize' contains 'Enabled', normal vectors specified with
-- 'Graphics.Rendering.OpenGL.GL.VertexSpec.normal' are scaled to unit length
-- after transformation. The initial value of 'normalize' is 'Disabled'.

normalize :: StateVar Capability
normalize = makeCapability CapNormalize

--------------------------------------------------------------------------------

data Plane a = Plane a a a a
   deriving ( Eq, Ord, Show )

instance Storable a => Storable (Plane a) where
   sizeOf    ~(Plane a _ _ _) = 4 * sizeOf a
   alignment ~(Plane a _ _ _) = alignment a
   peek                       = peek4 Plane . castPtr
   poke ptr   (Plane a b c d) = poke4 (castPtr ptr) a b c d

--------------------------------------------------------------------------------

data TextureCoordName =
     S
   | T
   | R
   | Q
   deriving ( Eq, Ord, Show )

marshalTextureCoordName :: TextureCoordName -> GLenum
marshalTextureCoordName x = case x of
   S -> 0x2000
   T -> 0x2001
   R -> 0x2002
   Q -> 0x2003

--------------------------------------------------------------------------------

data TextureGenParameter =
     TextureGenMode
   | ObjectPlane
   | EyePlane

marshalTextureGenParameter :: TextureGenParameter -> GLenum
marshalTextureGenParameter x = case x of
   TextureGenMode -> 0x2500
   ObjectPlane -> 0x2501
   EyePlane -> 0x2502

--------------------------------------------------------------------------------

data TextureGenMode' =
     EyeLinear'
   | ObjectLinear'
   | SphereMap'
   | NormalMap'
   | ReflectionMap'

marshalTextureGenMode' :: TextureGenMode' -> GLint
marshalTextureGenMode' x = case x of
   EyeLinear' -> 0x2400
   ObjectLinear' -> 0x2401
   SphereMap' -> 0x2402
   NormalMap' -> 0x8511
   ReflectionMap' -> 0x8512

unmarshalTextureGenMode' :: GLint -> TextureGenMode'
unmarshalTextureGenMode' x
   | x == 0x2400 = EyeLinear'
   | x == 0x2401 = ObjectLinear'
   | x == 0x2402 = SphereMap'
   | x == 0x8511 = NormalMap'
   | x == 0x8512 = ReflectionMap'
   | otherwise = error ("unmarshalTextureGenMode': illegal value " ++ show x)

--------------------------------------------------------------------------------

data TextureGenMode =
     EyeLinear    (Plane GLdouble)
   | ObjectLinear (Plane GLdouble)
   | SphereMap
   | NormalMap
   | ReflectionMap
   deriving ( Eq, Ord, Show )

marshalTextureGenMode :: TextureGenMode -> GLint
marshalTextureGenMode = marshalTextureGenMode' . convertMode
   where convertMode (EyeLinear    _) = EyeLinear'
         convertMode (ObjectLinear _) = ObjectLinear'
         convertMode SphereMap        = SphereMap'
         convertMode NormalMap        = NormalMap'
         convertMode ReflectionMap    = ReflectionMap'

--------------------------------------------------------------------------------

textureGenMode :: TextureCoordName -> StateVar (Maybe TextureGenMode)
textureGenMode coord =
   makeStateVarMaybe
      (return $ textureCoordNameToEnableCap coord)
      (do mode <- getMode coord
          case mode of
             EyeLinear'     -> liftM EyeLinear $ getPlane coord EyePlane
             ObjectLinear'  -> liftM ObjectLinear $ getPlane coord ObjectPlane
             SphereMap'     -> return SphereMap
             NormalMap'     -> return NormalMap
             ReflectionMap' -> return ReflectionMap)
      (\mode -> do
         setMode coord mode
         case mode of
            EyeLinear    plane -> setPlane coord EyePlane    plane
            ObjectLinear plane -> setPlane coord ObjectPlane plane
            _ -> return ())

--------------------------------------------------------------------------------

textureCoordNameToEnableCap :: TextureCoordName -> EnableCap
textureCoordNameToEnableCap coord = case coord of
   S -> CapTextureGenS
   T -> CapTextureGenT
   R -> CapTextureGenR
   Q -> CapTextureGenQ

--------------------------------------------------------------------------------

getMode :: TextureCoordName -> IO TextureGenMode'
getMode coord = alloca $ \buf -> do
   glGetTexGeniv (marshalTextureCoordName coord)
                 (marshalTextureGenParameter TextureGenMode)
                 buf
   peek1 unmarshalTextureGenMode' buf

foreign import CALLCONV unsafe "glGetTexGeniv" glGetTexGeniv ::
   GLenum -> GLenum -> Ptr GLint -> IO ()

setMode :: TextureCoordName -> TextureGenMode -> IO ()
setMode coord mode =
   glTexGeni (marshalTextureCoordName coord)
             (marshalTextureGenParameter TextureGenMode)
             (marshalTextureGenMode mode)

foreign import CALLCONV unsafe "glTexGeni" glTexGeni ::
    GLenum -> GLenum -> GLint -> IO ()

--------------------------------------------------------------------------------

getPlane :: TextureCoordName -> TextureGenParameter -> IO (Plane GLdouble)
getPlane coord param = alloca $ \planeBuffer -> do
   glGetTexGendv (marshalTextureCoordName coord)
                 (marshalTextureGenParameter param)
                 planeBuffer
   peek planeBuffer

foreign import CALLCONV unsafe "glGetTexGendv" glGetTexGendv ::
   GLenum -> GLenum -> Ptr (Plane GLdouble) -> IO ()

setPlane :: TextureCoordName -> TextureGenParameter -> Plane GLdouble -> IO ()
setPlane coord param plane =
   with plane $ \planeBuffer ->
      glTexGendv (marshalTextureCoordName coord)
                 (marshalTextureGenParameter param)
                 planeBuffer

foreign import CALLCONV unsafe "glTexGendv" glTexGendv ::
   GLenum -> GLenum -> Ptr (Plane GLdouble) -> IO ()
