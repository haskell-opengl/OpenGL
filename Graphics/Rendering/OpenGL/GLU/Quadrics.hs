--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GLU.Quadrics
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
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

import Control.Monad ( unless )
import Foreign.Ptr ( Ptr, nullPtr, freeHaskellFunPtr )
import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.GL.Colors ( ShadingModel(Smooth,Flat) )
import Graphics.Rendering.OpenGL.GL.Exception ( bracket )
import Graphics.Rendering.OpenGL.GL.GLboolean ( marshalGLboolean )
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal (
   recordErrorCode, recordOutOfMemory )

--------------------------------------------------------------------------------

data QuadricDrawStyle =
     PointStyle
   | LineStyle
   | FillStyle
   | SilhouetteStyle
   deriving ( Eq, Ord, Show )

marshalQuadricDrawStyle :: QuadricDrawStyle -> GLenum
marshalQuadricDrawStyle x = case x of
   PointStyle -> glu_POINT
   LineStyle -> glu_LINE
   FillStyle -> glu_FILL
   SilhouetteStyle -> glu_SILHOUETTE

--------------------------------------------------------------------------------

type QuadricNormal = Maybe ShadingModel

marshalQuadricNormal :: QuadricNormal -> GLenum
marshalQuadricNormal (Just Smooth) = glu_SMOOTH
marshalQuadricNormal (Just Flat  ) = glu_FLAT
marshalQuadricNormal Nothing       = glu_NONE

--------------------------------------------------------------------------------

data QuadricOrientation =
     Outside
   | Inside
   deriving ( Eq, Ord, Show )

marshalQuadricOrientation :: QuadricOrientation -> GLenum
marshalQuadricOrientation x = case x of
   Outside -> glu_OUTSIDE
   Inside -> glu_INSIDE

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

renderQuadric :: QuadricStyle -> QuadricPrimitive -> IO ()
renderQuadric style prim = do
   withQuadricObj recordOutOfMemory $ \quadricObj ->
      withErrorCallback quadricObj recordErrorCode $ do
         setStyle quadricObj style
         renderPrimitive quadricObj prim

withQuadricObj :: IO a -> (QuadricObj -> IO a) -> IO a
withQuadricObj failure success =
   bracket gluNewQuadric safeDeleteQuadric
           (\quadricObj -> if isNullQuadricObj quadricObj
                              then failure
                              else success quadricObj)

safeDeleteQuadric :: QuadricObj -> IO ()
safeDeleteQuadric quadricObj =
   unless (isNullQuadricObj quadricObj) $ gluDeleteQuadric quadricObj

withErrorCallback :: QuadricObj -> QuadricCallback -> IO a -> IO a
withErrorCallback quadricObj callback action =
   bracket (makeQuadricCallback callback) freeHaskellFunPtr $ \callbackPtr -> do
      gluQuadricCallback quadricObj glu_ERROR callbackPtr
      action

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

type QuadricObj = Ptr GLUquadric

isNullQuadricObj :: QuadricObj -> Bool
isNullQuadricObj = (nullPtr ==)
