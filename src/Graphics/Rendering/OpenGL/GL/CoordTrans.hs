--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.CoordTrans
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 2.11 (Coordinate Transformations) of the
-- OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances #-}

module Graphics.Rendering.OpenGL.GL.CoordTrans (
   -- * Controlling the Viewport
   depthRange,
   Position(..), Size(..), viewport, maxViewportDims,

   -- * Matrices
   MatrixMode(..), matrixMode,
   MatrixOrder(..), MatrixComponent(rotate,translate,scale), Matrix(..),
   matrix, multMatrix, GLmatrix, loadIdentity,
   ortho, frustum, depthClamp,
   activeTexture,
   preservingMatrix, unsafePreservingMatrix,
   stackDepth, maxStackDepth,

   -- * Normal Transformation
   rescaleNormal, normalize,

   -- * Generating Texture Coordinates
   Plane(..), TextureCoordName(..), TextureGenMode(..), textureGenMode
) where

import Data.StateVar
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.Tensor
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.Exception
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.Texturing.TextureUnit
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.Rendering.OpenGL.Raw

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
depthRange = makeStateVar (getClampd2 (,) GetDepthRange) (uncurry glDepthRange)

--------------------------------------------------------------------------------

-- | A 2-dimensional position, measured in pixels.
data Position = Position !GLint !GLint
   deriving ( Eq, Ord, Show )

-- | A 2-dimensional size, measured in pixels.
data Size = Size !GLsizei !GLsizei
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

-- | The implementation-dependent maximum viewport width and height.

maxViewportDims :: GettableStateVar Size
maxViewportDims = makeGettableStateVar (getSizei2 Size GetMaxViewportDims)

--------------------------------------------------------------------------------

-- | A matrix stack.

data MatrixMode =
     Modelview GLsizei  -- ^ The modelview matrix stack of the specified vertex unit.
   | Projection         -- ^ The projection matrix stack.
   | Texture            -- ^ The texture matrix stack.
   | Color              -- ^ The color matrix stack.
   | MatrixPalette      -- ^ The matrix palette stack.
   deriving ( Eq, Ord, Show )

marshalMatrixMode :: MatrixMode -> Maybe GLenum
marshalMatrixMode x = case x of
   Modelview i -> modelviewIndexToEnum i
   Projection -> Just gl_PROJECTION
   Texture -> Just gl_TEXTURE
   Color -> Just gl_COLOR
   MatrixPalette -> Just gl_MATRIX_PALETTE_ARB

unmarshalMatrixMode :: GLenum -> MatrixMode
unmarshalMatrixMode x
   | x == gl_PROJECTION = Projection
   | x == gl_TEXTURE = Texture
   | x == gl_COLOR = Color
   | x == gl_MATRIX_PALETTE_ARB = MatrixPalette
   | otherwise =
        case modelviewEnumToIndex x of
           Just i -> Modelview i
           Nothing -> error ("unmarshalMatrixMode: illegal value " ++ show x)

matrixModeToGetMatrix :: MatrixMode -> PNameMatrix
matrixModeToGetMatrix x = case x of
   Modelview _   -> GetModelviewMatrix -- ???
   Projection    -> GetProjectionMatrix
   Texture       -> GetTextureMatrix
   Color         -> GetColorMatrix
   MatrixPalette -> GetMatrixPalette

matrixModeToGetStackDepth :: MatrixMode -> PName1I
matrixModeToGetStackDepth x =  case x of
   Modelview _   -> GetModelviewStackDepth
   Projection    -> GetProjectionStackDepth
   Texture       -> GetTextureStackDepth
   Color         -> GetColorMatrixStackDepth
   MatrixPalette -> error "matrixModeToGetStackDepth: impossible"

matrixModeToGetMaxStackDepth :: MatrixMode -> PName1I
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
                (maybe recordInvalidValue glMatrixMode . marshalMatrixMode)

--------------------------------------------------------------------------------

data MatrixOrder = ColumnMajor | RowMajor
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

class Storable c => MatrixComponent c where
   getMatrix :: GetPNameMatrix p => p -> Ptr c -> IO ()
   loadMatrix :: Ptr c -> IO ()
   loadTransposeMatrix :: Ptr c -> IO ()
   multMatrix_ :: Ptr c -> IO ()
   multTransposeMatrix :: Ptr c -> IO ()
   rotate :: c -> Vector3 c -> IO ()
   translate :: Vector3 c -> IO ()
   scale :: c -> c -> c -> IO ()

instance MatrixComponent GLfloat where
   getMatrix = getMatrixf
   loadMatrix = glLoadMatrixf
   loadTransposeMatrix = glLoadTransposeMatrixf
   multMatrix_ = glMultMatrixf
   multTransposeMatrix = glMultTransposeMatrixf
   rotate a (Vector3 x y z) = glRotatef a x y z
   translate (Vector3 x y z) = glTranslatef x y z
   scale = glScalef

instance MatrixComponent GLdouble where
   getMatrix = getMatrixd
   loadMatrix = glLoadMatrixd
   loadTransposeMatrix = glLoadTransposeMatrixd
   multMatrix_ = glMultMatrixd
   multTransposeMatrix = glMultTransposeMatrixd
   rotate a (Vector3 x y z) = glRotated a x y z
   translate (Vector3 x y z) = glTranslated x y z
   scale = glScaled

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

matrix :: (Matrix m, MatrixComponent c) => Maybe MatrixMode -> StateVar (m c)
matrix maybeMode =
   makeStateVar
      (maybe (get matrixMode) return maybeMode >>= (getMatrix' . matrixModeToGetMatrix))
      (maybe id withMatrixMode maybeMode . setMatrix)

withMatrixMode :: MatrixMode -> IO a -> IO a
withMatrixMode mode act =
   preservingMatrixMode $ do
      matrixMode $= mode
      act

getMatrix' :: (Matrix m, MatrixComponent c) => PNameMatrix -> IO (m c)
getMatrix' = withNewMatrix ColumnMajor . getMatrix

setMatrix :: (Matrix m, MatrixComponent c) => m c -> IO ()
setMatrix mat =
   withMatrix mat $ \order ->
      case order of
         ColumnMajor -> loadMatrix
         RowMajor    -> loadTransposeMatrix

multMatrix :: (Matrix m, MatrixComponent c) => m c -> IO ()
multMatrix mat =
   withMatrix mat $ \order ->
      case order of
         ColumnMajor -> multMatrix_
         RowMajor    -> multTransposeMatrix

--------------------------------------------------------------------------------

data GLmatrix a = GLmatrix MatrixOrder (ForeignPtr a)
   deriving ( Eq, Ord, Show )

instance Matrix GLmatrix where
   withNewMatrix order f = do
      fp <- mallocForeignPtrArray 16
      withForeignPtr fp f
      return $ GLmatrix order fp

   withMatrix (GLmatrix order fp) f = withForeignPtr fp (f order)

--------------------------------------------------------------------------------

loadIdentity :: IO ()
loadIdentity = glLoadIdentity

--------------------------------------------------------------------------------

ortho :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()
ortho = glOrtho

frustum :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()
frustum = glFrustum

--------------------------------------------------------------------------------

depthClamp :: StateVar Capability
depthClamp = makeCapability CapDepthClamp

--------------------------------------------------------------------------------

activeTexture :: StateVar TextureUnit
activeTexture = makeStateVar (getEnum1 unmarshalTextureUnit GetActiveTexture)
                             (glActiveTexture . marshalTextureUnit)

--------------------------------------------------------------------------------

-- | Push the current matrix stack down by one, duplicating the current matrix,
-- excute the given action, and pop the current matrix stack, replacing the
-- current matrix with the one below it on the stack (i.e. restoring it to its
-- previous state). The returned value is that of the given action. Note that
-- a round-trip to the server is probably required. For a more efficient
-- version, see 'unsafePreservingMatrix'.

preservingMatrix :: IO a -> IO a
preservingMatrix = unsafePreservingMatrix . preservingMatrixMode

-- performance paranoia: No (un-)marshaling by avoiding matrixMode
preservingMatrixMode :: IO a -> IO a
preservingMatrixMode = bracket (getEnum1 id GetMatrixMode) glMatrixMode . const

-- | A more efficient, but potentially dangerous version of 'preservingMatrix':
-- The given action is not allowed to throw an exception or change the
-- current matrix mode permanently.

unsafePreservingMatrix :: IO a -> IO a
unsafePreservingMatrix = unsafeBracket_ glPushMatrix glPopMatrix

--------------------------------------------------------------------------------

stackDepth :: Maybe MatrixMode -> GettableStateVar GLsizei
stackDepth maybeMode =
   makeGettableStateVar $
      case maybeMode of
         Nothing -> getSizei1 id GetCurrentMatrixStackDepth -- only with ARB_fragment_program
         Just MatrixPalette -> do recordInvalidEnum ; return 0
         Just mode -> getSizei1 id (matrixModeToGetStackDepth mode)

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

data Plane a = Plane !a !a !a !a
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
   S -> gl_S
   T -> gl_T
   R -> gl_R
   Q -> gl_Q

--------------------------------------------------------------------------------

data TextureGenParameter =
     TextureGenMode
   | ObjectPlane
   | EyePlane

marshalTextureGenParameter :: TextureGenParameter -> GLenum
marshalTextureGenParameter x = case x of
   TextureGenMode -> gl_TEXTURE_GEN_MODE
   ObjectPlane -> gl_OBJECT_PLANE
   EyePlane -> gl_EYE_PLANE

--------------------------------------------------------------------------------

data TextureGenMode' =
     EyeLinear'
   | ObjectLinear'
   | SphereMap'
   | NormalMap'
   | ReflectionMap'

marshalTextureGenMode' :: TextureGenMode' -> GLint
marshalTextureGenMode' x = fromIntegral $ case x of
   EyeLinear' -> gl_EYE_LINEAR
   ObjectLinear' -> gl_OBJECT_LINEAR
   SphereMap' -> gl_SPHERE_MAP
   NormalMap' -> gl_NORMAL_MAP
   ReflectionMap' -> gl_REFLECTION_MAP

unmarshalTextureGenMode' :: GLint -> TextureGenMode'
unmarshalTextureGenMode' x
   | y == gl_EYE_LINEAR = EyeLinear'
   | y == gl_OBJECT_LINEAR = ObjectLinear'
   | y == gl_SPHERE_MAP = SphereMap'
   | y == gl_NORMAL_MAP = NormalMap'
   | y == gl_REFLECTION_MAP = ReflectionMap'
   | otherwise = error ("unmarshalTextureGenMode': illegal value " ++ show x)
   where y = fromIntegral x

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
             EyeLinear'     -> fmap EyeLinear $ getPlane coord EyePlane
             ObjectLinear'  -> fmap ObjectLinear $ getPlane coord ObjectPlane
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

setMode :: TextureCoordName -> TextureGenMode -> IO ()
setMode coord mode =
   glTexGeni (marshalTextureCoordName coord)
             (marshalTextureGenParameter TextureGenMode)
             (marshalTextureGenMode mode)

--------------------------------------------------------------------------------

getPlane :: TextureCoordName -> TextureGenParameter -> IO (Plane GLdouble)
getPlane coord param = alloca $ \planeBuffer -> do
   glGetTexGendv (marshalTextureCoordName coord)
                 (marshalTextureGenParameter param)
                 (castPtr planeBuffer)
   peek planeBuffer

setPlane :: TextureCoordName -> TextureGenParameter -> Plane GLdouble -> IO ()
setPlane coord param plane =
   with plane $ \planeBuffer ->
      glTexGendv (marshalTextureCoordName coord)
                 (marshalTextureGenParameter param)
                 (castPtr planeBuffer)
