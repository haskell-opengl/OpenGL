--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Colors
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module corresponds to section 2.13 (Colors and Coloring) of the
-- OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Colors (
   -- * Lighting
   FrontFaceDirection(..), frontFace,

   -- * Lighting Parameter Specification
   glMateriali, glMaterialf,
   glMaterialiv, glMaterialfv,
   glGetMaterialiv, glGetMaterialfv,

   glLighti, glLightf,
   glLightiv, glLightfv,
   glGetLightiv, glGetLightfv,

   glLightModeli, glLightModelf,
   glLightModeliv, glLightModelfv,

   -- * ColorMaterial
   glColorMaterial,

   -- * Flatshading
   ShadingModel(..), shadeModel
) where

import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLint, GLfloat )
import Graphics.Rendering.OpenGL.GL.Query (
   GetPName(GetFrontFace,GetShadeModel), getInteger1 )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar, makeStateVar )

--------------------------------------------------------------------------------

data FrontFaceDirection =
     CW
   | CCW
   deriving ( Eq, Ord, Show )

marshalFrontFaceDirection :: FrontFaceDirection -> GLenum
marshalFrontFaceDirection x = case x of
   CW -> 0x900
   CCW -> 0x901

unmarshalFrontFaceDirection :: GLenum -> FrontFaceDirection
unmarshalFrontFaceDirection x
   | x == 0x900 = CW
   | x == 0x901 = CCW
   | otherwise = error ("unmarshalFrontFaceDirection: illegal value " ++ show x)

--------------------------------------------------------------------------------

frontFace :: StateVar FrontFaceDirection
frontFace =
   makeStateVar
      (getInteger1 (unmarshalFrontFaceDirection . fromIntegral) GetFrontFace)
      (glFrontFace . marshalFrontFaceDirection)

foreign import CALLCONV unsafe "glFrontFace" glFrontFace :: GLenum -> IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glMateriali" glMateriali :: GLenum -> GLenum -> GLint -> IO ()
foreign import CALLCONV unsafe "glMaterialf" glMaterialf :: GLenum -> GLenum -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glMaterialiv" glMaterialiv :: GLenum -> GLenum -> Ptr GLint -> IO ()
foreign import CALLCONV unsafe "glMaterialfv" glMaterialfv :: GLenum -> GLenum -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glGetMaterialiv" glGetMaterialiv :: GLenum -> GLenum -> Ptr GLint -> IO ()
foreign import CALLCONV unsafe "glGetMaterialfv" glGetMaterialfv :: GLenum -> GLenum -> Ptr GLfloat -> IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glLighti" glLighti :: GLenum -> GLenum -> GLint -> IO ()
foreign import CALLCONV unsafe "glLightf" glLightf :: GLenum -> GLenum -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glLightiv" glLightiv :: GLenum -> GLenum -> Ptr GLint -> IO ()
foreign import CALLCONV unsafe "glLightfv" glLightfv :: GLenum -> GLenum -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glGetLightiv" glGetLightiv :: GLenum -> GLenum -> Ptr GLint -> IO ()
foreign import CALLCONV unsafe "glGetLightfv" glGetLightfv :: GLenum -> GLenum -> Ptr GLfloat -> IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glLightModeli" glLightModeli :: GLenum -> GLint -> IO ()
foreign import CALLCONV unsafe "glLightModelf" glLightModelf :: GLenum -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glLightModeliv" glLightModeliv :: GLenum -> Ptr GLint -> IO ()
foreign import CALLCONV unsafe "glLightModelfv" glLightModelfv :: GLenum -> Ptr GLfloat -> IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glColorMaterial" glColorMaterial :: GLenum -> GLenum -> IO ()

--------------------------------------------------------------------------------

data ShadingModel =
     Flat
   | Smooth
   deriving ( Eq, Ord, Show )

marshalShadingModel :: ShadingModel -> GLenum
marshalShadingModel x = case x of
   Flat -> 0x1d00
   Smooth -> 0x1d01

unmarshalShadingModel :: GLenum -> ShadingModel
unmarshalShadingModel x
   | x == 0x1d00 = Flat
   | x == 0x1d01 = Smooth
   | otherwise = error ("unmarshalShadingModel: illegal value " ++ show x)

--------------------------------------------------------------------------------

shadeModel :: StateVar ShadingModel
shadeModel =
   makeStateVar
      (getInteger1 (unmarshalShadingModel . fromIntegral) GetShadeModel)
      (glShadeModel . marshalShadingModel)

foreign import CALLCONV unsafe "glShadeModel" glShadeModel :: GLenum -> IO ()
