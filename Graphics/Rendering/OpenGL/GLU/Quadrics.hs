--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.Quadrics
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to chapter 6 (Quadrics) of the GLU specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GLU.Quadrics (
   QuadricNormal, QuadricTexture(..), QuadricOrientation(..),
   QuadricDrawStyle(..), QuadricStyle(..),
   Radius, Height, Angle, Slices, Stacks, Loops, QuadricPrimitive(..),
   renderQuadric
) where

import Data.IORef        ( newIORef, readIORef, modifyIORef )
import Foreign.Ptr       ( Ptr, nullPtr, FunPtr, freeHaskellFunPtr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLint, GLdouble )
import Graphics.Rendering.OpenGL.GL.Colors ( ShadingModel(Smooth,Flat) )
import Graphics.Rendering.OpenGL.GL.Exception ( finally )
import Graphics.Rendering.OpenGL.GL.GLboolean ( GLboolean, marshalGLboolean )
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal ( makeError )
import Graphics.Rendering.OpenGL.GLU.Errors (
   Error(Error), ErrorCategory(OutOfMemory) )

--------------------------------------------------------------------------------

data QuadricDrawStyle =
     PointStyle
   | LineStyle
   | FillStyle
   | SilhouetteStyle
   deriving ( Eq, Ord, Show )

marshalQuadricDrawStyle :: QuadricDrawStyle -> GLenum
marshalQuadricDrawStyle x = case x of
   PointStyle -> 0x186aa
   LineStyle -> 0x186ab
   FillStyle -> 0x186ac
   SilhouetteStyle -> 0x186ad

--------------------------------------------------------------------------------

data QuadricCallback =
     Error'2
   deriving ( Eq, Ord, Show )

marshalQuadricCallback :: QuadricCallback -> GLenum
marshalQuadricCallback x = case x of
   Error'2 -> 0x18707

--------------------------------------------------------------------------------

type QuadricNormal = Maybe ShadingModel

marshalQuadricNormal :: QuadricNormal -> GLenum
marshalQuadricNormal (Just Smooth) = 0x186a0
marshalQuadricNormal (Just Flat  ) = 0x186a1
marshalQuadricNormal Nothing       = 0x186a2

--------------------------------------------------------------------------------

data QuadricOrientation =
     Outside
   | Inside
   deriving ( Eq, Ord, Show )

marshalQuadricOrientation :: QuadricOrientation -> GLenum
marshalQuadricOrientation x = case x of
   Outside -> 0x186b4
   Inside -> 0x186b5

--------------------------------------------------------------------------------

data QuadricTexture
   = NoTextureCoordinates
   | GenerateTextureCoordinates
   deriving ( Eq,Ord )

marshalQuadricTexture :: QuadricTexture -> GLboolean
marshalQuadricTexture NoTextureCoordinates       = marshalGLboolean False
marshalQuadricTexture GenerateTextureCoordinates = marshalGLboolean True

--------------------------------------------------------------------------------

data QuadricStyle
   = QuadricStyle QuadricNormal
                  QuadricTexture
                  QuadricOrientation
                  QuadricDrawStyle
   deriving ( Eq,Ord )

--------------------------------------------------------------------------------

type Radius = GLdouble
type Height = GLdouble
type Angle  = GLdouble
type Slices = GLint
type Stacks = GLint
type Loops  = GLint

--------------------------------------------------------------------------------

data QuadricPrimitive
   = Sphere Radius Slices Stacks
   | Cylinder Radius Radius Height Slices Stacks
   | Disk Radius Radius Slices Loops
   | PartialDisk Radius Radius Slices Loops Angle Angle
   deriving ( Eq, Ord )

--------------------------------------------------------------------------------

renderQuadric :: QuadricStyle -> QuadricPrimitive -> IO (Maybe Error)
renderQuadric style prim =
   withState Nothing $ \modify -> do
      let registerError = modify . const . Just
          outOfMemoryError = Error OutOfMemory "out of memory"
      withQuadricObj (registerError outOfMemoryError) $ \quadricObj ->
         withErrorCallback quadricObj (\e -> makeError e >>= registerError) $ do
            setStyle quadricObj style
            renderPrimitive quadricObj prim

withState :: a -> (((a -> a) -> IO ()) -> IO ()) -> IO a
withState initialValue action = do
   ref <- newIORef initialValue
   action (modifyIORef ref)
   readIORef ref

withQuadricObj :: IO a -> (QuadricObj -> IO a) -> IO a
withQuadricObj failure success = do
   quadricObj <- gluNewQuadric
   if quadricObj == QuadricObj nullPtr
      then failure
      else success quadricObj `finally` gluDeleteQuadric quadricObj

withErrorCallback :: QuadricObj -> QuadricCallback' -> IO a -> IO a
withErrorCallback quadricObj callback action = do
   callbackPtr <- makeQuadricCallback callback
   gluQuadricCallback quadricObj (marshalQuadricCallback Error'2) callbackPtr
   action `finally` freeHaskellFunPtr callbackPtr

setStyle :: QuadricObj -> QuadricStyle -> IO ()
setStyle quadricObj (QuadricStyle n t o d) = do
   gluQuadricNormals     quadricObj (marshalQuadricNormal      n)
   gluQuadricTexture     quadricObj (marshalQuadricTexture     t)
   gluQuadricOrientation quadricObj (marshalQuadricOrientation o)
   gluQuadricDrawStyle   quadricObj (marshalQuadricDrawStyle   d)

renderPrimitive :: QuadricObj -> QuadricPrimitive -> IO ()
renderPrimitive quadricObj (Sphere r s n) =
   gluSphere quadricObj r s n
renderPrimitive quadricObj (Cylinder b t h s n) =
   gluCylinder quadricObj b t h s n
renderPrimitive quadricObj (Disk i o s l) =
   gluDisk quadricObj i o s l
renderPrimitive quadricObj (PartialDisk i o s l a w) =
   gluPartialDisk quadricObj i o s l a w

--------------------------------------------------------------------------------

-- 'Char' is a fake here, any marshalable type would do
newtype QuadricObj = QuadricObj (Ptr Char)
   deriving ( Eq )

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "gluNewQuadric" gluNewQuadric :: IO QuadricObj

foreign import CALLCONV unsafe "gluDeleteQuadric" gluDeleteQuadric ::
   QuadricObj -> IO ()

--------------------------------------------------------------------------------

type QuadricCallback' = GLenum -> IO ()

foreign import ccall "wrapper" makeQuadricCallback ::
   QuadricCallback' -> IO (FunPtr QuadricCallback')

foreign import CALLCONV unsafe "gluQuadricCallback" gluQuadricCallback ::
   QuadricObj -> GLenum -> FunPtr QuadricCallback' -> IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "gluQuadricNormals" gluQuadricNormals ::
   QuadricObj -> GLenum -> IO ()

foreign import CALLCONV unsafe "gluQuadricTexture" gluQuadricTexture ::
   QuadricObj -> GLboolean -> IO ()

foreign import CALLCONV unsafe "gluQuadricOrientation" gluQuadricOrientation ::
   QuadricObj -> GLenum -> IO ()

foreign import CALLCONV unsafe "gluQuadricDrawStyle" gluQuadricDrawStyle ::
   QuadricObj -> GLenum -> IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV safe "gluSphere" gluSphere ::
   QuadricObj -> Radius -> Slices -> Stacks -> IO ()

foreign import CALLCONV safe "gluCylinder" gluCylinder ::
  QuadricObj -> Radius -> Radius -> Height -> Slices -> Stacks -> IO ()

foreign import CALLCONV safe "gluDisk" gluDisk ::
   QuadricObj -> Radius -> Radius -> Slices -> Loops -> IO ()

foreign import CALLCONV safe "gluPartialDisk" gluPartialDisk ::
   QuadricObj -> Radius -> Radius -> Slices -> Loops -> Angle -> Angle -> IO ()
