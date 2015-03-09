--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Colors
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 2.14 (Colors and Coloring) of the
-- OpenGL 2.1 specs.
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
   vertexProgramTwoSide,
   LightModelColorControl(..), lightModelColorControl,

   -- * ColorMaterial
   ColorMaterialParameter(..), colorMaterial,

   -- * Flatshading
   ShadingModel(..), shadeModel,

   -- * Color clamping
   ClampTarget(..), ClampMode(..),
   clampColor,
) where

import Control.Monad
import Data.StateVar
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.Tensor
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.Face
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

lighting :: StateVar Capability
lighting = makeCapability CapLighting

--------------------------------------------------------------------------------

newtype Light = Light GLsizei
   deriving ( Eq, Ord, Show )

marshalLight :: Light -> Maybe GLenum
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
   CW -> gl_CW
   CCW -> gl_CCW

unmarshalFrontFaceDirection :: GLenum -> FrontFaceDirection
unmarshalFrontFaceDirection x
   | x == gl_CW = CW
   | x == gl_CCW = CCW
   | otherwise = error ("unmarshalFrontFaceDirection: illegal value " ++ show x)

--------------------------------------------------------------------------------

frontFace :: StateVar FrontFaceDirection
frontFace =
   makeStateVar
      (getEnum1 unmarshalFrontFaceDirection GetFrontFace)
      (glFrontFace . marshalFrontFaceDirection)

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
   MaterialEmission -> gl_EMISSION
   MaterialShininess -> gl_SHININESS
   MaterialAmbientAndDiffuse -> gl_AMBIENT_AND_DIFFUSE
   MaterialColorIndexes -> gl_COLOR_INDEXES
   MaterialAmbient -> gl_AMBIENT
   MaterialDiffuse -> gl_DIFFUSE
   MaterialSpecular -> gl_SPECULAR

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

glGetMaterialfvc :: GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()
glGetMaterialfvc face pname ptr = glGetMaterialfv face pname (castPtr ptr)

glMaterialfvc :: GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()
glMaterialfvc face pname ptr = glMaterialfv face pname (castPtr ptr)

--------------------------------------------------------------------------------

materialShininess :: Face -> StateVar GLfloat
materialShininess =
   makeMaterialVar glGetMaterialfvf glMaterialff MaterialShininess

glGetMaterialfvf :: GLenum -> GLenum -> Ptr GLfloat -> IO ()
glGetMaterialfvf face pname ptr = glGetMaterialfv face pname (castPtr ptr)

glMaterialff :: GLenum -> GLenum -> Ptr GLfloat -> IO ()
glMaterialff face pname ptr = glMaterialfv face pname (castPtr ptr)

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

setMaterialColorIndexes ::
   Face -> (Index1 GLint, Index1 GLint, Index1 GLint) -> IO ()
setMaterialColorIndexes face (Index1 a, Index1 d, Index1 s) =
   withArray [a, d, s] $
      glMaterialiv (marshalFace face)
                   (marshalMaterialParameter MaterialColorIndexes)

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
   Ambient' -> gl_AMBIENT
   Diffuse' -> gl_DIFFUSE
   Specular' -> gl_SPECULAR
   Position -> gl_POSITION
   SpotDirection -> gl_SPOT_DIRECTION
   SpotExponent -> gl_SPOT_EXPONENT
   SpotCutoff -> gl_SPOT_CUTOFF
   ConstantAttenuation -> gl_CONSTANT_ATTENUATION
   LinearAttenuation -> gl_LINEAR_ATTENUATION
   QuadraticAttenuation -> gl_QUADRATIC_ATTENUATION

--------------------------------------------------------------------------------

ambient :: Light -> StateVar (Color4 GLfloat)
ambient = makeLightVar glGetLightfvc glLightfvc Ambient' black

black :: Color4 GLfloat
black = Color4 0 0 0 0

diffuse :: Light -> StateVar (Color4 GLfloat)
diffuse = makeLightVar glGetLightfvc glLightfvc Diffuse' black

specular :: Light -> StateVar (Color4 GLfloat)
specular = makeLightVar glGetLightfvc glLightfvc Specular' black

makeLightVar :: Storable a
             => (GLenum -> GLenum -> Ptr a -> IO ())
             -> (GLenum -> GLenum -> Ptr a -> IO ())
             -> LightParameter -> a -> Light -> StateVar a
makeLightVar getter setter lightParameter defaultValue theLight =
   makeStateVar (maybe (return defaultValue) getLightVar ml)
                (\val -> maybe recordInvalidEnum (setLightVar val) ml)
   where lp          = marshalLightParameter lightParameter
         ml          = marshalLight theLight
         getLightVar = \l -> alloca $ \buf -> do getter l lp buf ; peek buf
         setLightVar = \val l -> with val $ setter l lp

glGetLightfvc :: GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()
glGetLightfvc l pname ptr = glGetLightfv l pname (castPtr ptr)

glLightfvc :: GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()
glLightfvc l pname ptr = glLightfv l pname (castPtr ptr)

--------------------------------------------------------------------------------

position :: Light -> StateVar (Vertex4 GLfloat)
position = makeLightVar glGetLightfvv glLightfvv Position (Vertex4 0 0 0 0)

glLightfvv :: GLenum -> GLenum -> Ptr (Vertex4 GLfloat) -> IO ()
glLightfvv l pname ptr = glLightfv l pname (castPtr ptr)

glGetLightfvv :: GLenum -> GLenum -> Ptr (Vertex4 GLfloat) -> IO ()
glGetLightfvv l pname ptr = glGetLightfv l pname (castPtr ptr)

--------------------------------------------------------------------------------

spotDirection :: Light -> StateVar (Normal3 GLfloat)
spotDirection =
   makeLightVar glGetLightfvn glLightfvn SpotDirection (Normal3 0 0 0)

glLightfvn :: GLenum -> GLenum -> Ptr (Normal3 GLfloat) -> IO ()
glLightfvn l pname ptr = glLightfv l pname (castPtr ptr)

glGetLightfvn :: GLenum -> GLenum -> Ptr (Normal3 GLfloat) -> IO ()
glGetLightfvn l pname ptr = glGetLightfv l pname (castPtr ptr)

--------------------------------------------------------------------------------

spotExponent :: Light -> StateVar GLfloat
spotExponent = makeLightVar glGetLightfv glLightfv SpotExponent 0

maxSpotExponent :: GettableStateVar GLfloat
maxSpotExponent = makeGettableStateVar $ getFloat1 id GetMaxSpotExponent

--------------------------------------------------------------------------------

spotCutoff :: Light -> StateVar GLfloat
spotCutoff = makeLightVar glGetLightfv glLightfv SpotCutoff 0

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
constantAttenuation = makeLightVar glGetLightfv glLightfv ConstantAttenuation 0

linearAttenuation :: Light -> StateVar GLfloat
linearAttenuation = makeLightVar glGetLightfv glLightfv LinearAttenuation 0

quadraticAttenuation :: Light -> StateVar GLfloat
quadraticAttenuation =
   makeLightVar glGetLightfv glLightfv QuadraticAttenuation 0

--------------------------------------------------------------------------------

data LightModelParameter =
     LightModelAmbient
   | LightModelLocalViewer
   | LightModelTwoSide
   | LightModelColorControl

marshalLightModelParameter :: LightModelParameter -> GLenum
marshalLightModelParameter x = case x of
   LightModelAmbient -> gl_LIGHT_MODEL_AMBIENT
   LightModelLocalViewer -> gl_LIGHT_MODEL_LOCAL_VIEWER
   LightModelTwoSide -> gl_LIGHT_MODEL_TWO_SIDE
   LightModelColorControl -> gl_LIGHT_MODEL_COLOR_CONTROL

--------------------------------------------------------------------------------

lightModelAmbient :: StateVar (Color4 GLfloat)
lightModelAmbient =
   makeStateVar
      (getFloat4 Color4 GetLightModelAmbient)
      (\c -> with c $
                glLightModelfv (marshalLightModelParameter LightModelAmbient) . castPtr)

--------------------------------------------------------------------------------

lightModelLocalViewer :: StateVar Capability
lightModelLocalViewer =
   makeLightModelCapVar GetLightModelLocalViewer LightModelLocalViewer

makeLightModelCapVar :: PName1I -> LightModelParameter -> StateVar Capability
makeLightModelCapVar pname lightModelParameter =
   makeStateVar
      (getBoolean1 unmarshalCapability pname)
      (glLightModeli (marshalLightModelParameter lightModelParameter) .
                     fromIntegral . marshalCapability)

--------------------------------------------------------------------------------

lightModelTwoSide :: StateVar Capability
lightModelTwoSide = makeLightModelCapVar GetLightModelTwoSide LightModelTwoSide

vertexProgramTwoSide :: StateVar Capability
vertexProgramTwoSide = makeCapability CapVertexProgramTwoSide

--------------------------------------------------------------------------------

data LightModelColorControl =
     SingleColor
   | SeparateSpecularColor
   deriving ( Eq, Ord, Show )

marshalLightModelColorControl :: LightModelColorControl -> GLenum
marshalLightModelColorControl x = case x of
   SingleColor -> gl_SINGLE_COLOR
   SeparateSpecularColor -> gl_SEPARATE_SPECULAR_COLOR

unmarshalLightModelColorControl :: GLenum -> LightModelColorControl
unmarshalLightModelColorControl x
   | x == gl_SINGLE_COLOR = SingleColor
   | x == gl_SEPARATE_SPECULAR_COLOR = SeparateSpecularColor
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
   Ambient -> gl_AMBIENT
   Diffuse -> gl_DIFFUSE
   Specular -> gl_SPECULAR
   Emission -> gl_EMISSION
   AmbientAndDiffuse -> gl_AMBIENT_AND_DIFFUSE

unmarshalColorMaterialParameter :: GLenum -> ColorMaterialParameter
unmarshalColorMaterialParameter x
   | x == gl_AMBIENT = Ambient
   | x == gl_DIFFUSE = Diffuse
   | x == gl_SPECULAR = Specular
   | x == gl_EMISSION = Emission
   | x == gl_AMBIENT_AND_DIFFUSE = AmbientAndDiffuse
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

--------------------------------------------------------------------------------

data ShadingModel =
     Flat
   | Smooth
   deriving ( Eq, Ord, Show )

marshalShadingModel :: ShadingModel -> GLenum
marshalShadingModel x = case x of
   Flat -> gl_FLAT
   Smooth -> gl_SMOOTH

unmarshalShadingModel :: GLenum -> ShadingModel
unmarshalShadingModel x
   | x == gl_FLAT = Flat
   | x == gl_SMOOTH = Smooth
   | otherwise = error ("unmarshalShadingModel: illegal value " ++ show x)

--------------------------------------------------------------------------------

shadeModel :: StateVar ShadingModel
shadeModel =
   makeStateVar
      (getEnum1 unmarshalShadingModel GetShadeModel)
      (glShadeModel . marshalShadingModel)

--------------------------------------------------------------------------------

data ClampTarget =
     ClampVertexColor
   | ClampFragmentColor
   | ClampReadColor
   deriving ( Eq, Ord, Show )

marshalClampTarget :: ClampTarget -> GLenum
marshalClampTarget x = case x of
   ClampVertexColor -> gl_CLAMP_VERTEX_COLOR
   ClampFragmentColor -> gl_CLAMP_FRAGMENT_COLOR
   ClampReadColor -> gl_CLAMP_READ_COLOR

marshalClampTargetToPName :: ClampTarget -> PName1I
marshalClampTargetToPName x = case x of
   ClampFragmentColor -> GetFragmentColorClamp
   ClampVertexColor -> GetVertexColorClamp
   ClampReadColor -> GetReadColorClamp

--------------------------------------------------------------------------------

data ClampMode =
     ClampOn
   | FixedOnly
   | ClampOff
   deriving ( Eq, Ord, Show )

marshalClampMode :: ClampMode -> GLenum
marshalClampMode x = case x of
   ClampOn -> gl_TRUE
   FixedOnly -> gl_FIXED_ONLY
   ClampOff -> gl_FALSE

unmarshalClampMode :: GLenum -> ClampMode
unmarshalClampMode x
   | x == gl_TRUE = ClampOn
   | x == gl_FIXED_ONLY = FixedOnly
   | x == gl_FALSE = ClampOff
   | otherwise = error $ "unmarshalClampMode: unknown enum value " ++ show x

--------------------------------------------------------------------------------

clampColor :: ClampTarget -> StateVar ClampMode
clampColor ct = makeStateVar (getClampColor ct) (setClampColor ct)
   where getClampColor = getEnum1 unmarshalClampMode . marshalClampTargetToPName
         setClampColor t = glClampColor (marshalClampTarget t) . marshalClampMode
