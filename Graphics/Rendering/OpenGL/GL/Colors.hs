--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Colors
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 2.13 (Colors and Coloring) of the
-- OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Colors (
   -- * Lighting
   lighting, Light(..), light, maxLights,
   FrontFaceDirection(..), frontFace,

   -- * Lighting Parameter Specification
   Face(..),
   materialAmbient, materialDiffuse, materialAmbientAndDiffuse,
   materialSpecular, materialEmission, materialShininess, maxShininess,
   materialColorIndexes,

   ambient, diffuse, specular,
   position, spotDirection, spotExponent, maxSpotExponent, spotCutoff,
   attenuation,

   lightModelAmbient, lightModelLocalViewer, lightModelTwoSide,
   LightModelColorControl(..), lightModelColorControl,

   -- * ColorMaterial
   ColorMaterialParameter(..), colorMaterial,

   -- * Flatshading
   ShadingModel(..), shadeModel
) where

import Control.Monad ( liftM2, liftM3 )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( allocaArray, withArray )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable(peek) )
import Graphics.Rendering.OpenGL.GL.Capability (
   marshalCapability, unmarshalCapability,
   EnableCap(CapLighting,CapColorMaterial,CapLight), makeCapability,
   makeStateVarMaybe )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLint, GLsizei, GLfloat,Capability )
import Graphics.Rendering.OpenGL.GL.Face (
   Face(..), marshalFace, unmarshalFace )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek3 )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetMaxLights, GetFrontFace,GetShadeModel,
            GetLightModelAmbient, GetLightModelLocalViewer,
            GetLightModelTwoSide, GetLightModelColorControl,
            GetColorMaterialFace,GetColorMaterialParameter,
            GetMaxShininess,GetMaxSpotExponent),
   getBoolean1, getEnum1, getSizei1, getFloat1, getFloat4, lightIndexToEnum )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasGetter(get), HasSetter(($=)),
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.VertexSpec (
   Color4(..), Normal3(..), Vertex4(..), Index1(..) )

--------------------------------------------------------------------------------

lighting :: StateVar Capability
lighting = makeCapability CapLighting

--------------------------------------------------------------------------------

data Light = Light GLsizei
   deriving ( Eq, Ord, Show )

marshalLight :: Light -> GLenum
marshalLight (Light l) = lightIndexToEnum l

--------------------------------------------------------------------------------

light :: Light -> StateVar Capability
light (Light l) = makeCapability (CapLight l)

maxLights :: GettableStateVar GLsizei
maxLights = makeGettableStateVar (getSizei1 id GetMaxLights)

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
      (getEnum1 unmarshalFrontFaceDirection GetFrontFace)
      (glFrontFace . marshalFrontFaceDirection)

foreign import CALLCONV unsafe "glFrontFace" glFrontFace :: GLenum -> IO ()

--------------------------------------------------------------------------------

data MaterialParameter =
     MaterialEmission
   | MaterialShininess
   | MaterialAmbientAndDiffuse
   | MaterialColorIndexes
   | MaterialAmbient
   | MaterialDiffuse
   | MaterialSpecular

marshalMaterialParameter :: MaterialParameter -> GLenum
marshalMaterialParameter x = case x of
   MaterialEmission -> 0x1600
   MaterialShininess -> 0x1601
   MaterialAmbientAndDiffuse -> 0x1602
   MaterialColorIndexes -> 0x1603
   MaterialAmbient -> 0x1200
   MaterialDiffuse -> 0x1201
   MaterialSpecular -> 0x1202

--------------------------------------------------------------------------------

materialAmbient :: Face -> StateVar (Color4 GLfloat)
materialAmbient =
   makeMaterialVar glGetMaterialfvc glMaterialfvc MaterialAmbient

materialDiffuse :: Face -> StateVar (Color4 GLfloat)
materialDiffuse =
   makeMaterialVar glGetMaterialfvc glMaterialfvc MaterialDiffuse

materialAmbientAndDiffuse :: Face -> StateVar (Color4 GLfloat)
materialAmbientAndDiffuse =
   makeMaterialVar glGetMaterialfvc glMaterialfvc MaterialAmbientAndDiffuse

materialSpecular :: Face -> StateVar (Color4 GLfloat)
materialSpecular =
   makeMaterialVar glGetMaterialfvc glMaterialfvc MaterialSpecular

materialEmission :: Face -> StateVar (Color4 GLfloat)
materialEmission =
   makeMaterialVar glGetMaterialfvc glMaterialfvc MaterialEmission

makeMaterialVar :: Storable a
                => (GLenum -> GLenum -> Ptr a -> IO ())
                -> (GLenum -> GLenum -> Ptr a -> IO ())
                -> MaterialParameter -> Face -> StateVar a
makeMaterialVar getter setter materialParameter face =
   makeStateVar (alloca $ \buf -> do getter f mp buf ; peek buf)
                (\val -> with val $ setter f mp)
   where mp = marshalMaterialParameter materialParameter
         f  = marshalFace face

foreign import CALLCONV unsafe "glGetMaterialfv" glGetMaterialfvc ::
   GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()

foreign import CALLCONV unsafe "glMaterialfv" glMaterialfvc ::
   GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()

--------------------------------------------------------------------------------

materialShininess :: Face -> StateVar GLfloat
materialShininess =
   makeMaterialVar glGetMaterialfvf glMaterialff MaterialShininess

foreign import CALLCONV unsafe "glGetMaterialfv" glGetMaterialfvf ::
   GLenum -> GLenum -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glMaterialfv" glMaterialff ::
   GLenum -> GLenum -> Ptr GLfloat -> IO ()

maxShininess :: GettableStateVar GLfloat
maxShininess = makeGettableStateVar $ getFloat1 id GetMaxShininess

--------------------------------------------------------------------------------

-- Alas, (Index1 GLint, Index1 GLint, Index1 GLint) is not an instance of
-- Storable...

materialColorIndexes ::
   Face -> StateVar (Index1 GLint, Index1 GLint, Index1 GLint)
materialColorIndexes face =
   makeStateVar (getMaterialColorIndexes face) (setMaterialColorIndexes face)

getMaterialColorIndexes :: Face -> IO (Index1 GLint, Index1 GLint, Index1 GLint)
getMaterialColorIndexes face =
   allocaArray 3 $ \buf -> do
      glGetMaterialiv (marshalFace face)
                      (marshalMaterialParameter MaterialColorIndexes)
                      buf
      peek3 (\a d s -> (Index1 a, Index1 d, Index1 s)) buf

foreign import CALLCONV unsafe "glGetMaterialiv" glGetMaterialiv ::
   GLenum -> GLenum -> Ptr GLint -> IO ()

setMaterialColorIndexes ::
   Face -> (Index1 GLint, Index1 GLint, Index1 GLint) -> IO ()
setMaterialColorIndexes face (Index1 a, Index1 d, Index1 s) =
   withArray [a, d, s] $
      glMaterialiv (marshalFace face)
                   (marshalMaterialParameter MaterialColorIndexes)

foreign import CALLCONV unsafe "glMaterialiv" glMaterialiv ::
   GLenum -> GLenum -> Ptr GLint -> IO ()

--------------------------------------------------------------------------------

data LightParameter =
     Ambient'
   | Diffuse'
   | Specular'
   | Position
   | SpotDirection
   | SpotExponent
   | SpotCutoff
   | ConstantAttenuation
   | LinearAttenuation
   | QuadraticAttenuation

marshalLightParameter :: LightParameter -> GLenum
marshalLightParameter x = case x of
   Ambient' -> 0x1200
   Diffuse' -> 0x1201
   Specular' -> 0x1202
   Position -> 0x1203
   SpotDirection -> 0x1204
   SpotExponent -> 0x1205
   SpotCutoff -> 0x1206
   ConstantAttenuation -> 0x1207
   LinearAttenuation -> 0x1208
   QuadraticAttenuation -> 0x1209

--------------------------------------------------------------------------------

ambient :: Light -> StateVar (Color4 GLfloat)
ambient = makeLightVar glGetLightfvc glLightfvc Ambient'

diffuse :: Light -> StateVar (Color4 GLfloat)
diffuse = makeLightVar glGetLightfvc glLightfvc Diffuse'

specular :: Light -> StateVar (Color4 GLfloat)
specular = makeLightVar glGetLightfvc glLightfvc Specular'

makeLightVar :: Storable a
             => (GLenum -> GLenum -> Ptr a -> IO ())
             -> (GLenum -> GLenum -> Ptr a -> IO ())
             -> LightParameter -> Light -> StateVar a
makeLightVar getter setter lightParameter theLight =
   makeStateVar (alloca $ \buf -> do getter l lp buf ; peek buf)
                (\val -> with val $ setter l lp)
   where lp = marshalLightParameter lightParameter
         l  = marshalLight theLight

foreign import CALLCONV unsafe "glGetLightfv" glGetLightfvc ::
   GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()

foreign import CALLCONV unsafe "glLightfv" glLightfvc ::
   GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()

--------------------------------------------------------------------------------

position :: Light -> StateVar (Vertex4 GLfloat)
position = makeLightVar glGetLightfvv glLightfvv Position

foreign import CALLCONV unsafe "glLightfv" glLightfvv ::
   GLenum -> GLenum -> Ptr (Vertex4 GLfloat) -> IO ()

foreign import CALLCONV unsafe "glGetLightfv" glGetLightfvv ::
   GLenum -> GLenum -> Ptr (Vertex4 GLfloat) -> IO ()

--------------------------------------------------------------------------------

spotDirection :: Light -> StateVar (Normal3 GLfloat)
spotDirection = makeLightVar glGetLightfvn glLightfvn SpotDirection

foreign import CALLCONV unsafe "glLightfv" glLightfvn ::
   GLenum -> GLenum -> Ptr (Normal3 GLfloat) -> IO ()

foreign import CALLCONV unsafe "glGetLightfv" glGetLightfvn ::
   GLenum -> GLenum -> Ptr (Normal3 GLfloat) -> IO ()

--------------------------------------------------------------------------------

spotExponent :: Light -> StateVar GLfloat
spotExponent = makeLightVar glGetLightfv glLightfv SpotExponent

foreign import CALLCONV unsafe "glLightfv" glLightfv ::
   GLenum -> GLenum -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glGetLightfv" glGetLightfv ::
   GLenum -> GLenum -> Ptr GLfloat -> IO ()

maxSpotExponent :: GettableStateVar GLfloat
maxSpotExponent = makeGettableStateVar $ getFloat1 id GetMaxSpotExponent

--------------------------------------------------------------------------------

spotCutoff :: Light -> StateVar GLfloat
spotCutoff = makeLightVar glGetLightfv glLightfv SpotCutoff

--------------------------------------------------------------------------------

attenuation :: Light -> StateVar (GLfloat, GLfloat, GLfloat)
attenuation theLight =
   makeStateVar
      (liftM3 (,,) (get (constantAttenuation  theLight))
                   (get (linearAttenuation    theLight))
                   (get (quadraticAttenuation theLight)))
      (\(constant, linear, quadratic) -> do
         constantAttenuation  theLight $= constant
         linearAttenuation    theLight $= linear
         quadraticAttenuation theLight $= quadratic)

constantAttenuation :: Light -> StateVar GLfloat
constantAttenuation = makeLightVar glGetLightfv glLightfv ConstantAttenuation

linearAttenuation :: Light -> StateVar GLfloat
linearAttenuation = makeLightVar glGetLightfv glLightfv LinearAttenuation

quadraticAttenuation :: Light -> StateVar GLfloat
quadraticAttenuation = makeLightVar glGetLightfv glLightfv QuadraticAttenuation

--------------------------------------------------------------------------------

data LightModelParameter =
     LightModelAmbient
   | LightModelLocalViewer
   | LightModelTwoSide
   | LightModelColorControl

marshalLightModelParameter :: LightModelParameter -> GLenum
marshalLightModelParameter x = case x of
   LightModelAmbient -> 0xb53
   LightModelLocalViewer -> 0xb51
   LightModelTwoSide -> 0xb52
   LightModelColorControl -> 0x81f8

--------------------------------------------------------------------------------

lightModelAmbient :: StateVar (Color4 GLfloat)
lightModelAmbient =
   makeStateVar
      (getFloat4 Color4 GetLightModelAmbient)
      (\c -> with c $
                glLightModelfv (marshalLightModelParameter LightModelAmbient))

foreign import CALLCONV unsafe "glLightModelfv" glLightModelfv ::
   GLenum -> Ptr (Color4 GLfloat) -> IO ()

--------------------------------------------------------------------------------

lightModelLocalViewer :: StateVar Capability
lightModelLocalViewer =
   makeLightModelCapVar GetLightModelLocalViewer LightModelLocalViewer

makeLightModelCapVar :: GetPName -> LightModelParameter -> StateVar Capability
makeLightModelCapVar pname lightModelParameter =
   makeStateVar
      (getBoolean1 unmarshalCapability pname)
      (glLightModeli (marshalLightModelParameter lightModelParameter) .
                     fromIntegral . marshalCapability)

foreign import CALLCONV unsafe "glLightModeli" glLightModeli ::
   GLenum -> GLint -> IO ()

--------------------------------------------------------------------------------

lightModelTwoSide :: StateVar Capability
lightModelTwoSide = makeLightModelCapVar GetLightModelTwoSide LightModelTwoSide

--------------------------------------------------------------------------------

data LightModelColorControl =
     SingleColor
   | SeparateSpecularColor
   deriving ( Eq, Ord, Show )

marshalLightModelColorControl :: LightModelColorControl -> GLenum
marshalLightModelColorControl x = case x of
   SingleColor -> 0x81f9
   SeparateSpecularColor -> 0x81fa

unmarshalLightModelColorControl :: GLenum -> LightModelColorControl
unmarshalLightModelColorControl x
   | x == 0x81f9 = SingleColor
   | x == 0x81fa = SeparateSpecularColor
   | otherwise = error ("unmarshalLightModelColorControl: illegal value " ++ show x)

--------------------------------------------------------------------------------

lightModelColorControl :: StateVar LightModelColorControl
lightModelColorControl =
   makeStateVar
      (getEnum1 unmarshalLightModelColorControl GetLightModelColorControl)
      (glLightModeli (marshalLightModelParameter LightModelColorControl) .
                     fromIntegral . marshalLightModelColorControl)

--------------------------------------------------------------------------------

data ColorMaterialParameter =
     Ambient
   | Diffuse
   | Specular
   | Emission
   | AmbientAndDiffuse
   deriving ( Eq, Ord, Show )

marshalColorMaterialParameter :: ColorMaterialParameter -> GLenum
marshalColorMaterialParameter x = case x of
   Ambient -> 0x1200
   Diffuse -> 0x1201
   Specular -> 0x1202
   Emission -> 0x1600
   AmbientAndDiffuse -> 0x1602

unmarshalColorMaterialParameter :: GLenum -> ColorMaterialParameter
unmarshalColorMaterialParameter x
   | x == 0x1200 = Ambient
   | x == 0x1201 = Diffuse
   | x == 0x1202 = Specular
   | x == 0x1600 = Emission
   | x == 0x1602 = AmbientAndDiffuse
   | otherwise = error ("unmarshalColorMaterialParameter: illegal value " ++ show x)

--------------------------------------------------------------------------------

colorMaterial :: StateVar (Maybe (Face, ColorMaterialParameter))
colorMaterial =
   makeStateVarMaybe
      (return CapColorMaterial)
      (liftM2
         (,)
         (getEnum1 unmarshalFace GetColorMaterialFace)
         (getEnum1 unmarshalColorMaterialParameter GetColorMaterialParameter))
      (\(face, param) -> glColorMaterial (marshalFace face)
                                         (marshalColorMaterialParameter param))

foreign import CALLCONV unsafe "glColorMaterial" glColorMaterial ::
   GLenum -> GLenum -> IO ()

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
      (getEnum1 unmarshalShadingModel GetShadeModel)
      (glShadeModel . marshalShadingModel)

foreign import CALLCONV unsafe "glShadeModel" glShadeModel :: GLenum -> IO ()
