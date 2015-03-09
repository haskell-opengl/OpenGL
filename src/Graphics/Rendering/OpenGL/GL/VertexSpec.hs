--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.VertexSpec
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 2.7 (Vertex Specification) of the
-- OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances #-}

module Graphics.Rendering.OpenGL.GL.VertexSpec (
   -- * Vertex Coordinates
   Vertex(..),
   VertexComponent,

   -- * Auxiliary Vertex Attributes
   -- $AuxiliaryVertexAttributes

   -- ** Texture Coordinates
   currentTextureCoords, TexCoord(..),
   TexCoordComponent,
   TexCoord1(..), TexCoord2(..), TexCoord3(..), TexCoord4(..),

   -- ** Normal
   currentNormal, Normal(..),
   NormalComponent,
   Normal3(..),

   -- ** Fog Coordinate
   currentFogCoord, FogCoord(..),
   FogCoordComponent,
   FogCoord1(..),

   -- ** Color and Secondary Color
   rgbaMode,
   currentColor, Color(..),
   currentSecondaryColor, SecondaryColor(..),
   ColorComponent,
   Color3(..), Color4(..),

   currentIndex, Index(..),
   IndexComponent,
   Index1(..),

   -- * Generic Vertex Attributes
   IntegerHandling(..), AttribLocation(..),
   currentVertexAttrib, currentVertexAttribI, currentVertexAttribIu,
   VertexAttrib(..), VertexAttribComponent(..),

   -- * Texture Units
   TextureUnit(..), maxTextureUnit
) where

import Data.StateVar
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.Tensor
import Graphics.Rendering.OpenGL.GL.Texturing.TextureUnit
import Graphics.Rendering.OpenGL.GL.VertexAttributes
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

-- | The class of all types which can be used as a vertex coordinate.

class VertexComponent a where
   vertex2 :: a -> a -> IO ()
   vertex3 :: a -> a -> a -> IO ()
   vertex4 :: a -> a -> a -> a -> IO ()

   vertex2v :: Ptr a -> IO ()
   vertex3v :: Ptr a -> IO ()
   vertex4v :: Ptr a -> IO ()

instance VertexComponent GLshort where
   vertex2 = glVertex2s
   vertex3 = glVertex3s
   vertex4 = glVertex4s

   vertex2v = glVertex2sv
   vertex3v = glVertex3sv
   vertex4v = glVertex4sv

instance VertexComponent GLint where
   vertex2 = glVertex2i
   vertex3 = glVertex3i
   vertex4 = glVertex4i

   vertex2v = glVertex2iv
   vertex3v = glVertex3iv
   vertex4v = glVertex4iv

instance VertexComponent GLfloat where
   vertex2 = glVertex2f
   vertex3 = glVertex3f
   vertex4 = glVertex4f

   vertex2v = glVertex2fv
   vertex3v = glVertex3fv
   vertex4v = glVertex4fv

instance VertexComponent GLdouble where
   vertex2 = glVertex2d
   vertex3 = glVertex3d
   vertex4 = glVertex4d

   vertex2v = glVertex2dv
   vertex3v = glVertex3dv
   vertex4v = glVertex4dv

--------------------------------------------------------------------------------

-- | Specify the (/x/, /y/, /z/, /w/) coordinates of a four-dimensional vertex.
-- This must only be done during
-- 'Graphics.Rendering.OpenGL.GL.BeginEnd.renderPrimitive', otherwise the
-- behaviour is unspecified. The current values of the auxiliary vertex
-- attributes are associated with the vertex.
-- 
-- Note that there is no such thing as a \"current vertex\" which could be
-- retrieved.

class Vertex a where
   vertex  ::     a -> IO ()
   vertexv :: Ptr a -> IO ()

instance VertexComponent a => Vertex (Vertex2 a) where
   vertex (Vertex2 x y) = vertex2 x y
   vertexv = vertex2v . (castPtr :: Ptr (Vertex2 b) -> Ptr b)

instance VertexComponent a => Vertex (Vertex3 a) where
   vertex (Vertex3 x y z) = vertex3 x y z
   vertexv = vertex3v . (castPtr :: Ptr (Vertex3 b) -> Ptr b)

instance VertexComponent a => Vertex (Vertex4 a) where
   vertex (Vertex4 x y z w) = vertex4 x y z w
   vertexv = vertex4v . (castPtr :: Ptr (Vertex4 b) -> Ptr b)

--------------------------------------------------------------------------------
-- $AuxiliaryVertexAttributes
-- Apart from its coordinates in four-dimensional space, every vertex has
-- associated /auxiliary attributes/: Its texture coordinates, a normal, a
-- fog coordinate, and a color plus a secondary color. For every attribute, the
-- OpenGL state contains its current value, which can be changed at any time.
--
-- Every attribute has a \"natural\" format via which it can be manipulated
-- directly as part of the OpenGL state, e.g. the current texture coordinates
-- are internally handled as @'TexCoord4' 'GLfloat'@. Different formats are
-- converted to this format, e.g. the /s/, /r/, and /t/ coordinates of a
-- @'TexCoord3' 'GLint'@ are converted to floating point values and a /q/
-- coordinate of 1.0 is implicitly assumed.
--
-- Consequently, the vast majority of classes, functions, and data types in this
-- module are for convenience only and offer no additional functionality.

--------------------------------------------------------------------------------

-- | The current texture coordinates (/s/, /t/, /r/, /q/) for the current
-- texture unit (see 'Graphics.Rendering.OpenGL.GL.CoordTrans.activeTexture').
-- The initial value is (0,0,0,1) for all texture units.

currentTextureCoords :: StateVar (TexCoord4 GLfloat)
currentTextureCoords =
   makeStateVar (getFloat4 TexCoord4 GetCurrentTextureCoords) texCoord

--------------------------------------------------------------------------------

-- | The class of all types which can be used as a texture coordinate.

class TexCoordComponent a where
   texCoord1 :: a -> IO ()
   texCoord2 :: a -> a -> IO ()
   texCoord3 :: a -> a -> a -> IO ()
   texCoord4 :: a -> a -> a -> a -> IO ()

   texCoord1v :: Ptr a -> IO ()
   texCoord2v :: Ptr a -> IO ()
   texCoord3v :: Ptr a -> IO ()
   texCoord4v :: Ptr a -> IO ()

   multiTexCoord1 :: GLenum -> a -> IO ()
   multiTexCoord2 :: GLenum -> a -> a -> IO ()
   multiTexCoord3 :: GLenum -> a -> a -> a -> IO ()
   multiTexCoord4 :: GLenum -> a -> a -> a -> a -> IO ()

   multiTexCoord1v :: GLenum -> Ptr a -> IO ()
   multiTexCoord2v :: GLenum -> Ptr a -> IO ()
   multiTexCoord3v :: GLenum -> Ptr a -> IO ()
   multiTexCoord4v :: GLenum -> Ptr a -> IO ()

instance TexCoordComponent GLshort where
   texCoord1 = glTexCoord1s
   texCoord2 = glTexCoord2s
   texCoord3 = glTexCoord3s
   texCoord4 = glTexCoord4s

   texCoord1v = glTexCoord1sv
   texCoord2v = glTexCoord2sv
   texCoord3v = glTexCoord3sv
   texCoord4v = glTexCoord4sv

   multiTexCoord1 = glMultiTexCoord1s
   multiTexCoord2 = glMultiTexCoord2s
   multiTexCoord3 = glMultiTexCoord3s
   multiTexCoord4 = glMultiTexCoord4s

   multiTexCoord1v = glMultiTexCoord1sv
   multiTexCoord2v = glMultiTexCoord2sv
   multiTexCoord3v = glMultiTexCoord3sv
   multiTexCoord4v = glMultiTexCoord4sv

instance TexCoordComponent GLint where
   texCoord1 = glTexCoord1i
   texCoord2 = glTexCoord2i
   texCoord3 = glTexCoord3i
   texCoord4 = glTexCoord4i

   texCoord1v = glTexCoord1iv
   texCoord2v = glTexCoord2iv
   texCoord3v = glTexCoord3iv
   texCoord4v = glTexCoord4iv

   multiTexCoord1 = glMultiTexCoord1i
   multiTexCoord2 = glMultiTexCoord2i
   multiTexCoord3 = glMultiTexCoord3i
   multiTexCoord4 = glMultiTexCoord4i

   multiTexCoord1v = glMultiTexCoord1iv
   multiTexCoord2v = glMultiTexCoord2iv
   multiTexCoord3v = glMultiTexCoord3iv
   multiTexCoord4v = glMultiTexCoord4iv

instance TexCoordComponent GLfloat where
   texCoord1 = glTexCoord1f
   texCoord2 = glTexCoord2f
   texCoord3 = glTexCoord3f
   texCoord4 = glTexCoord4f

   texCoord1v = glTexCoord1fv
   texCoord2v = glTexCoord2fv
   texCoord3v = glTexCoord3fv
   texCoord4v = glTexCoord4fv

   multiTexCoord1 = glMultiTexCoord1f
   multiTexCoord2 = glMultiTexCoord2f
   multiTexCoord3 = glMultiTexCoord3f
   multiTexCoord4 = glMultiTexCoord4f

   multiTexCoord1v = glMultiTexCoord1fv
   multiTexCoord2v = glMultiTexCoord2fv
   multiTexCoord3v = glMultiTexCoord3fv
   multiTexCoord4v = glMultiTexCoord4fv

instance TexCoordComponent GLdouble where
   texCoord1 = glTexCoord1d
   texCoord2 = glTexCoord2d
   texCoord3 = glTexCoord3d
   texCoord4 = glTexCoord4d

   texCoord1v = glTexCoord1dv
   texCoord2v = glTexCoord2dv
   texCoord3v = glTexCoord3dv
   texCoord4v = glTexCoord4dv

   multiTexCoord1 = glMultiTexCoord1d
   multiTexCoord2 = glMultiTexCoord2d
   multiTexCoord3 = glMultiTexCoord3d
   multiTexCoord4 = glMultiTexCoord4d

   multiTexCoord1v = glMultiTexCoord1dv
   multiTexCoord2v = glMultiTexCoord2dv
   multiTexCoord3v = glMultiTexCoord3dv
   multiTexCoord4v = glMultiTexCoord4dv

--------------------------------------------------------------------------------

-- | Change the current texture coordinates of the current or given texture
-- unit.

class TexCoord a where
   texCoord       ::                    a -> IO ()
   texCoordv      ::                Ptr a -> IO ()
   multiTexCoord  :: TextureUnit ->     a -> IO ()
   multiTexCoordv :: TextureUnit -> Ptr a -> IO ()

instance TexCoordComponent a => TexCoord (TexCoord1 a) where
   texCoord (TexCoord1 s) = texCoord1 s
   texCoordv = texCoord1v . (castPtr :: Ptr (TexCoord1 b) -> Ptr b)
   multiTexCoord u (TexCoord1 s) =
      multiTexCoord1 (marshalTextureUnit u) s
   multiTexCoordv u =
      multiTexCoord1v (marshalTextureUnit u) . (castPtr :: Ptr (TexCoord1 b) -> Ptr b)

instance TexCoordComponent a => TexCoord (TexCoord2 a) where
   texCoord (TexCoord2 s t) = texCoord2 s t
   texCoordv = texCoord2v . (castPtr :: Ptr (TexCoord2 b) -> Ptr b)
   multiTexCoord u (TexCoord2 s t) =
      multiTexCoord2 (marshalTextureUnit u) s t
   multiTexCoordv u =
      multiTexCoord2v (marshalTextureUnit u) . (castPtr :: Ptr (TexCoord2 b) -> Ptr b)

instance TexCoordComponent a => TexCoord (TexCoord3 a) where
   texCoord (TexCoord3 s t r) = texCoord3 s t r
   texCoordv = texCoord3v . (castPtr :: Ptr (TexCoord3 b) -> Ptr b)
   multiTexCoord u (TexCoord3 s t r) =
      multiTexCoord3 (marshalTextureUnit u) s t r
   multiTexCoordv u =
      multiTexCoord3v (marshalTextureUnit u) . (castPtr :: Ptr (TexCoord3 b) -> Ptr b)

instance TexCoordComponent a => TexCoord (TexCoord4 a) where
   texCoord (TexCoord4 s t r q) = texCoord4 s t r q
   texCoordv = texCoord4v . (castPtr :: Ptr (TexCoord4 b) -> Ptr b)
   multiTexCoord u (TexCoord4 s t r q) =
      multiTexCoord4 (marshalTextureUnit u) s t r q
   multiTexCoordv u =
      multiTexCoord4v (marshalTextureUnit u) . (castPtr :: Ptr (TexCoord4 b) -> Ptr b)

--------------------------------------------------------------------------------

-- | The current normal (/x/, /y/, /z/). The initial value is the unit vector
-- (0, 0, 1).

currentNormal :: StateVar (Normal3 GLfloat)
currentNormal = makeStateVar (getFloat3 Normal3 GetCurrentNormal) normal

--------------------------------------------------------------------------------

-- | The class of all types which can be used as a component of a normal.

class NormalComponent a where
   normal3 :: a -> a -> a -> IO ()
   normal3v :: Ptr a -> IO ()

instance NormalComponent GLbyte where
   normal3 = glNormal3b
   normal3v = glNormal3bv

instance NormalComponent GLshort where
   normal3 = glNormal3s
   normal3v = glNormal3sv

instance NormalComponent GLint where
   normal3 = glNormal3i
   normal3v = glNormal3iv

instance NormalComponent GLfloat where
   normal3 = glNormal3f
   normal3v = glNormal3fv

instance NormalComponent GLdouble where
   normal3 = glNormal3d
   normal3v = glNormal3dv

--------------------------------------------------------------------------------

-- | Change the current normal. Integral arguments are converted to
-- floating-point with a linear mapping that maps the most positive
-- representable integer value to 1.0, and the most negative representable
-- integer value to -1.0.
--
-- Normals specified with 'normal' or 'normalv' need not have unit length.
-- If 'Graphics.Rendering.OpenGL.GL.CoordTrans.normalize' is enabled, then
-- normals of any length specified with 'normal' or 'normalv' are normalized
-- after transformation. If
-- 'Graphics.Rendering.OpenGL.GL.CoordTrans.rescaleNormal' is enabled, normals
-- are scaled by a scaling factor derived from the modelview matrix.
-- 'Graphics.Rendering.OpenGL.GL.CoordTrans.rescaleNormal' requires that the
-- originally specified normals were of unit length, and that the modelview
-- matrix contains only uniform scales for proper results. Normalization is 
-- initially disabled.

class Normal a where
   normal  ::     a -> IO ()
   normalv :: Ptr a -> IO ()

instance NormalComponent a => Normal (Normal3 a) where
   normal (Normal3 x y z) = normal3 x y z
   normalv = normal3v . (castPtr :: Ptr (Normal3 b) -> Ptr b)

--------------------------------------------------------------------------------

-- | The current fog coordinate. The initial value is 0.

currentFogCoord :: StateVar (FogCoord1 GLfloat)
currentFogCoord =
   makeStateVar (getFloat1 FogCoord1 GetCurrentFogCoord) fogCoord

--------------------------------------------------------------------------------

-- | The class of all types which can be used as the fog coordinate.

class FogCoordComponent a where
   fogCoord1 :: a -> IO ()
   fogCoord1v :: Ptr a -> IO ()

instance FogCoordComponent GLfloat where
   fogCoord1 = glFogCoordf
   fogCoord1v = glFogCoordfv

instance FogCoordComponent GLdouble where
   fogCoord1 = glFogCoordd
   fogCoord1v = glFogCoorddv

--------------------------------------------------------------------------------

-- | Change the current fog coordinate.

class FogCoord a where
   fogCoord  ::     a -> IO ()
   fogCoordv :: Ptr a -> IO ()

instance FogCoordComponent a => FogCoord (FogCoord1 a) where
   fogCoord (FogCoord1 c) = fogCoord1 c
   fogCoordv = fogCoord1v . (castPtr :: Ptr (FogCoord1 b) -> Ptr b)

--------------------------------------------------------------------------------

-- | If 'rgbaMode' contains 'True', the color buffers store RGBA value. If
-- color indexes are stored, it contains 'False'.

rgbaMode :: GettableStateVar Bool
rgbaMode = makeGettableStateVar (getBoolean1 unmarshalGLboolean GetRGBAMode)

--------------------------------------------------------------------------------

-- The current color (/R/, /G/, /B/, /A/). The initial value is (1, 1, 1, 1).
-- Note that this state variable is significant only when the GL is in RGBA
-- mode.

currentColor :: StateVar (Color4 GLfloat)
currentColor =
   makeStateVar (getFloat4 Color4 GetCurrentColor) color

-- The current secondary color (/R/, /G/, /B/). The initial value is (0, 0, 0).
-- Note that this state variable is significant only when the GL is in RGBA
-- mode.

currentSecondaryColor :: StateVar (Color3 GLfloat)
currentSecondaryColor =
   makeStateVar
      (do Color4 r g b _ <- getFloat4 Color4 GetCurrentSecondaryColor
          return $ Color3 r g b)
      secondaryColor

--------------------------------------------------------------------------------

-- | The class of all types which can be used as a color component.

class ColorComponent a where
   color3 :: a -> a -> a -> IO ()
   color4 :: a -> a -> a -> a -> IO ()

   color3v :: Ptr a -> IO ()
   color4v :: Ptr a -> IO ()

   secondaryColor3  :: a -> a -> a -> IO ()
   secondaryColor3v :: Ptr a -> IO ()

instance ColorComponent GLbyte where
   color3 = glColor3b
   color4 = glColor4b

   color3v = glColor3bv
   color4v = glColor4bv

   secondaryColor3 = glSecondaryColor3b
   secondaryColor3v = glSecondaryColor3bv

instance ColorComponent GLshort where
   color3 = glColor3s
   color4 = glColor4s

   color3v = glColor3sv
   color4v = glColor4sv

   secondaryColor3 = glSecondaryColor3s
   secondaryColor3v = glSecondaryColor3sv

instance ColorComponent GLint where
   color3 = glColor3i
   color4 = glColor4i

   color3v = glColor3iv
   color4v = glColor4iv

   secondaryColor3 = glSecondaryColor3i
   secondaryColor3v = glSecondaryColor3iv

instance ColorComponent GLfloat where
   color3 = glColor3f
   color4 = glColor4f

   color3v = glColor3fv
   color4v = glColor4fv

   secondaryColor3 = glSecondaryColor3f
   secondaryColor3v = glSecondaryColor3fv

instance ColorComponent GLdouble where
   color3 = glColor3d
   color4 = glColor4d

   color3v = glColor3dv
   color4v = glColor4dv

   secondaryColor3 = glSecondaryColor3d
   secondaryColor3v = glSecondaryColor3dv

instance ColorComponent GLubyte where
   color3 = glColor3ub
   color4 = glColor4ub

   color3v = glColor3ubv
   color4v = glColor4ubv

   secondaryColor3 = glSecondaryColor3ub
   secondaryColor3v = glSecondaryColor3ubv

instance ColorComponent GLushort where
   color3 = glColor3us
   color4 = glColor4us

   color3v = glColor3usv
   color4v = glColor4usv

   secondaryColor3 = glSecondaryColor3us
   secondaryColor3v = glSecondaryColor3usv

instance ColorComponent GLuint where
   color3 = glColor3ui
   color4 = glColor4ui

   color3v = glColor3uiv
   color4v = glColor4uiv

   secondaryColor3 = glSecondaryColor3ui
   secondaryColor3v = glSecondaryColor3uiv

--------------------------------------------------------------------------------

-- | Change the current color.

class Color a where
   color  ::     a -> IO ()
   colorv :: Ptr a -> IO ()

instance ColorComponent a => Color (Color3 a) where
   color (Color3 r g b) = color3 r g b
   colorv = color3v . (castPtr :: Ptr (Color3 b) -> Ptr b)

instance ColorComponent a => Color (Color4 a) where
   color (Color4 r g b a) = color4 r g b a
   colorv = color4v . (castPtr :: Ptr (Color4 b) -> Ptr b)

--------------------------------------------------------------------------------

-- | Change the current secondary color.

class SecondaryColor a where
   secondaryColor  ::     a -> IO ()
   secondaryColorv :: Ptr a -> IO ()

instance ColorComponent a => SecondaryColor (Color3 a) where
   secondaryColor (Color3 r g b) = secondaryColor3 r g b
   secondaryColorv = secondaryColor3v . (castPtr :: Ptr (Color3 b) -> Ptr b)

--------------------------------------------------------------------------------

-- The current color index. The initial value is 1. Note that this state
-- variable is significant only when the GL is in color index mode.

currentIndex :: StateVar (Index1 GLint)
currentIndex = makeStateVar (getInteger1 Index1 GetCurrentIndex) index

--------------------------------------------------------------------------------

-- | The class of all types which can be used as a color index.

class IndexComponent a where
   index1 :: a -> IO ()
   index1v :: Ptr a -> IO ()

instance IndexComponent GLshort where
   index1 = glIndexs
   index1v = glIndexsv

instance IndexComponent GLint where
   index1 = glIndexi
   index1v = glIndexiv

instance IndexComponent GLfloat where
   index1 = glIndexf
   index1v = glIndexfv

instance IndexComponent GLdouble where
   index1 = glIndexd
   index1v = glIndexdv

instance IndexComponent GLubyte where
   index1 = glIndexub
   index1v = glIndexubv

--------------------------------------------------------------------------------

-- | Change the current color index.

class Index a where
   index  ::     a -> IO ()  -- Collision with Prelude.index
   indexv :: Ptr a -> IO ()

instance IndexComponent a => Index (Index1 a) where
   index (Index1 i) = index1 i
   indexv = index1v . (castPtr :: Ptr (Index1 b) -> Ptr b)

--------------------------------------------------------------------------------

data IntegerHandling =
     ToFloat
   | ToNormalizedFloat
   | KeepIntegral
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

currentVertexAttrib :: AttribLocation -> StateVar (Vertex4 GLfloat)
currentVertexAttrib location =
   makeStateVar
      (getVertexAttribFloat4 Vertex4 location GetCurrentVertexAttrib)
      (vertexAttrib ToFloat location)

currentVertexAttribI :: AttribLocation -> StateVar (Vertex4 GLint)
currentVertexAttribI location =
   makeStateVar
      (getVertexAttribIInteger4 Vertex4 location GetCurrentVertexAttrib)
      (vertexAttrib ToNormalizedFloat location)

currentVertexAttribIu :: AttribLocation -> StateVar (Vertex4 GLuint)
currentVertexAttribIu location =
   makeStateVar
      (getVertexAttribIuInteger4 Vertex4 location GetCurrentVertexAttrib)
      (vertexAttrib KeepIntegral location)

--------------------------------------------------------------------------------
-- The generic vertex attribute API is not as orthogonal as we would like.
-- Minimal methods: vertexAttrib4v and vertexAttrib4Nv and vertexAttrib4Iv

-- | The class of all types which can be used as a generic vertex attribute.
-- NOTE: Do not use the methods of this class directly, they were only exported
-- by accident and will be hidden in future versions of this package.

class (Storable a, Num a) => VertexAttribComponent a where
   vertexAttrib1 :: AttribLocation -> a -> IO ()
   vertexAttrib2 :: AttribLocation -> a -> a -> IO ()
   vertexAttrib3 :: AttribLocation -> a -> a -> a -> IO ()
   vertexAttrib4 :: AttribLocation -> a -> a -> a -> a -> IO ()

   vertexAttrib1N :: AttribLocation -> a -> IO ()
   vertexAttrib2N :: AttribLocation -> a -> a -> IO ()
   vertexAttrib3N :: AttribLocation -> a -> a -> a -> IO ()
   vertexAttrib4N :: AttribLocation -> a -> a -> a -> a -> IO ()

   vertexAttrib1I :: AttribLocation -> a -> IO ()
   vertexAttrib2I :: AttribLocation -> a -> a -> IO ()
   vertexAttrib3I :: AttribLocation -> a -> a -> a -> IO ()
   vertexAttrib4I :: AttribLocation -> a -> a -> a -> a -> IO ()

   vertexAttrib1v :: AttribLocation -> Ptr a -> IO ()
   vertexAttrib2v :: AttribLocation -> Ptr a -> IO ()
   vertexAttrib3v :: AttribLocation -> Ptr a -> IO ()
   vertexAttrib4v :: AttribLocation -> Ptr a -> IO ()

   vertexAttrib1Nv :: AttribLocation -> Ptr a -> IO ()
   vertexAttrib2Nv :: AttribLocation -> Ptr a -> IO ()
   vertexAttrib3Nv :: AttribLocation -> Ptr a -> IO ()
   vertexAttrib4Nv :: AttribLocation -> Ptr a -> IO ()

   vertexAttrib1Iv :: AttribLocation -> Ptr a -> IO ()
   vertexAttrib2Iv :: AttribLocation -> Ptr a -> IO ()
   vertexAttrib3Iv :: AttribLocation -> Ptr a -> IO ()
   vertexAttrib4Iv :: AttribLocation -> Ptr a -> IO ()

   vertexAttrib1 location x = vertexAttrib4 location x 0 0 1
   vertexAttrib2 location x y = vertexAttrib4 location x y 0 1
   vertexAttrib3 location x y z = vertexAttrib4 location x y z 1
   vertexAttrib4 location x y z w = allocaArray 4 $ \buf -> do
                                       poke4 buf x y z w
                                       vertexAttrib4v location buf

   vertexAttrib1N location x = vertexAttrib4N location x 0 0 1
   vertexAttrib2N location x y = vertexAttrib4N location x y 0 1
   vertexAttrib3N location x y z = vertexAttrib4N location x y z 1
   vertexAttrib4N location x y z w = allocaArray 4 $ \buf -> do
                                       poke4 buf x y z w
                                       vertexAttrib4Nv location buf

   vertexAttrib1I location x = vertexAttrib4I location x 0 0 1
   vertexAttrib2I location x y = vertexAttrib4I location x y 0 1
   vertexAttrib3I location x y z = vertexAttrib4I location x y z 1
   vertexAttrib4I location x y z w = allocaArray 4 $ \buf -> do
                                       poke4 buf x y z w
                                       vertexAttrib4Iv location buf

   vertexAttrib1v location = peek1M $ vertexAttrib1 location
   vertexAttrib2v location = peek2M $ vertexAttrib2 location
   vertexAttrib3v location = peek3M $ vertexAttrib3 location

   vertexAttrib1Nv location = peek1M $ vertexAttrib1N location
   vertexAttrib2Nv location = peek2M $ vertexAttrib2N location
   vertexAttrib3Nv location = peek3M $ vertexAttrib3N location

   vertexAttrib1Iv location = peek1M $ vertexAttrib1I location
   vertexAttrib2Iv location = peek2M $ vertexAttrib2I location
   vertexAttrib3Iv location = peek3M $ vertexAttrib3I location

instance VertexAttribComponent GLbyte where
   vertexAttrib4v (AttribLocation al) = glVertexAttrib4bv al
   vertexAttrib4Nv (AttribLocation al) = glVertexAttrib4Nbv al
   vertexAttrib4Iv (AttribLocation al) = glVertexAttribI4bv al

instance VertexAttribComponent GLubyte where
   vertexAttrib4N (AttribLocation al) = glVertexAttrib4Nub al
   vertexAttrib4v (AttribLocation al) = glVertexAttrib4ubv al
   vertexAttrib4Nv (AttribLocation al) = glVertexAttrib4Nubv al
   vertexAttrib4Iv (AttribLocation al) = glVertexAttribI4ubv al

instance VertexAttribComponent GLshort where
   vertexAttrib1 (AttribLocation al) = glVertexAttrib1s al
   vertexAttrib2 (AttribLocation al) = glVertexAttrib2s al
   vertexAttrib3 (AttribLocation al) = glVertexAttrib3s al
   vertexAttrib4 (AttribLocation al) = glVertexAttrib4s al

   vertexAttrib1v (AttribLocation al) = glVertexAttrib1sv al
   vertexAttrib2v (AttribLocation al) = glVertexAttrib2sv al
   vertexAttrib3v (AttribLocation al) = glVertexAttrib3sv al
   vertexAttrib4v (AttribLocation al) = glVertexAttrib4sv al

   vertexAttrib4Nv (AttribLocation al) = glVertexAttrib4Nsv al

   vertexAttrib4Iv (AttribLocation al) = glVertexAttribI4sv al

instance VertexAttribComponent GLushort where
   vertexAttrib4v (AttribLocation al) = glVertexAttrib4usv al
   vertexAttrib4Nv (AttribLocation al) = glVertexAttrib4Nusv al
   vertexAttrib4Iv (AttribLocation al) = glVertexAttribI4usv al

instance VertexAttribComponent GLint where
   vertexAttrib1I (AttribLocation al) = glVertexAttribI1i al
   vertexAttrib2I (AttribLocation al) = glVertexAttribI2i al
   vertexAttrib3I (AttribLocation al) = glVertexAttribI3i al
   vertexAttrib4I (AttribLocation al) = glVertexAttribI4i al

   vertexAttrib4v (AttribLocation al) = glVertexAttrib4iv al

   vertexAttrib4Nv (AttribLocation al) = glVertexAttrib4Niv al

   vertexAttrib1Iv (AttribLocation al) = glVertexAttribI1iv al
   vertexAttrib2Iv (AttribLocation al) = glVertexAttribI2iv al
   vertexAttrib3Iv (AttribLocation al) = glVertexAttribI3iv al
   vertexAttrib4Iv (AttribLocation al) = glVertexAttribI4iv al

instance VertexAttribComponent GLuint where
   vertexAttrib1I (AttribLocation al) = glVertexAttribI1ui al
   vertexAttrib2I (AttribLocation al) = glVertexAttribI2ui al
   vertexAttrib3I (AttribLocation al) = glVertexAttribI3ui al
   vertexAttrib4I (AttribLocation al) = glVertexAttribI4ui al

   vertexAttrib4v (AttribLocation al) = glVertexAttrib4uiv al

   vertexAttrib4Nv (AttribLocation al) = glVertexAttrib4Nuiv al

   vertexAttrib1Iv (AttribLocation al) = glVertexAttribI1uiv al
   vertexAttrib2Iv (AttribLocation al) = glVertexAttribI2uiv al
   vertexAttrib3Iv (AttribLocation al) = glVertexAttribI3uiv al
   vertexAttrib4Iv (AttribLocation al) = glVertexAttribI4uiv al

instance VertexAttribComponent GLfloat where
   vertexAttrib1 (AttribLocation al) = glVertexAttrib1f al
   vertexAttrib2 (AttribLocation al) = glVertexAttrib2f al
   vertexAttrib3 (AttribLocation al) = glVertexAttrib3f al
   vertexAttrib4 (AttribLocation al) = glVertexAttrib4f al

   vertexAttrib1v (AttribLocation al) = glVertexAttrib1fv al
   vertexAttrib2v (AttribLocation al) = glVertexAttrib2fv al
   vertexAttrib3v (AttribLocation al) = glVertexAttrib3fv al
   vertexAttrib4v (AttribLocation al) = glVertexAttrib4fv al

   vertexAttrib4Nv = vertexAttrib4v

   vertexAttrib4Iv = vertexAttrib4IvRealFrac

vertexAttrib4IvRealFrac :: (Storable a, RealFrac a) => AttribLocation -> Ptr a -> IO ()
vertexAttrib4IvRealFrac location = peek4M $ \x y z w ->
   vertexAttrib4I location (toGLint x) (toGLint y) (toGLint z) (toGLint w)

-- formula 2.6 from the OpenGL 3.1 spec
toGLint :: RealFrac a => a -> GLint
toGLint = truncate . (fromIntegral (maxBound :: GLint) *). clamp
   where clamp = max (-1.0) . min 1.0

instance VertexAttribComponent GLdouble where
   vertexAttrib1 (AttribLocation al) = glVertexAttrib1d al
   vertexAttrib2 (AttribLocation al) = glVertexAttrib2d al
   vertexAttrib3 (AttribLocation al) = glVertexAttrib3d al
   vertexAttrib4 (AttribLocation al) = glVertexAttrib4d al

   vertexAttrib1v (AttribLocation al) = glVertexAttrib1dv al
   vertexAttrib2v (AttribLocation al) = glVertexAttrib2dv al
   vertexAttrib3v (AttribLocation al) = glVertexAttrib3dv al
   vertexAttrib4v (AttribLocation al) = glVertexAttrib4dv al

   vertexAttrib4Nv = vertexAttrib4v

   vertexAttrib4Iv = vertexAttrib4IvRealFrac

--------------------------------------------------------------------------------

class VertexAttrib a where
   vertexAttrib  :: IntegerHandling -> AttribLocation ->     a -> IO ()
   vertexAttribv :: IntegerHandling -> AttribLocation -> Ptr a -> IO ()

instance VertexAttribComponent a => VertexAttrib (Vertex1 a) where
   vertexAttrib ToFloat location (Vertex1 i) = vertexAttrib1 location i
   vertexAttrib ToNormalizedFloat location (Vertex1 i) = vertexAttrib1N location i
   vertexAttrib KeepIntegral location (Vertex1 i) = vertexAttrib1I location i

   vertexAttribv ToFloat location = vertexAttrib1v location . (castPtr :: Ptr (Vertex1 b) -> Ptr b)
   vertexAttribv ToNormalizedFloat location = vertexAttrib1Nv location . (castPtr :: Ptr (Vertex1 b) -> Ptr b)
   vertexAttribv KeepIntegral location = vertexAttrib1Iv location . (castPtr :: Ptr (Vertex1 b) -> Ptr b)

instance VertexAttribComponent a => VertexAttrib (Vertex2 a) where
   vertexAttrib ToFloat location (Vertex2 x y) = vertexAttrib2 location x y
   vertexAttrib ToNormalizedFloat location (Vertex2 x y) = vertexAttrib2N location x y
   vertexAttrib KeepIntegral location (Vertex2 x y) = vertexAttrib2I location x y

   vertexAttribv ToFloat location = vertexAttrib2v location . (castPtr :: Ptr (Vertex2 b) -> Ptr b)
   vertexAttribv ToNormalizedFloat location = vertexAttrib2Nv location . (castPtr :: Ptr (Vertex2 b) -> Ptr b)
   vertexAttribv KeepIntegral location = vertexAttrib2Iv location . (castPtr :: Ptr (Vertex2 b) -> Ptr b)

instance VertexAttribComponent a => VertexAttrib (Vertex3 a) where
   vertexAttrib ToFloat location (Vertex3 x y z) = vertexAttrib3 location x y z
   vertexAttrib ToNormalizedFloat location (Vertex3 x y z) = vertexAttrib3N location x y z
   vertexAttrib KeepIntegral location (Vertex3 x y z) = vertexAttrib3I location x y z

   vertexAttribv ToFloat location = vertexAttrib3v location . (castPtr :: Ptr (Vertex3 b) -> Ptr b)
   vertexAttribv ToNormalizedFloat location = vertexAttrib3Nv location . (castPtr :: Ptr (Vertex3 b) -> Ptr b)
   vertexAttribv KeepIntegral location = vertexAttrib3Iv location . (castPtr :: Ptr (Vertex3 b) -> Ptr b)

instance VertexAttribComponent a => VertexAttrib (Vertex4 a) where
   vertexAttrib ToFloat location (Vertex4 x y z w) = vertexAttrib4 location x y z w
   vertexAttrib ToNormalizedFloat location (Vertex4 x y z w) = vertexAttrib4N location x y z w
   vertexAttrib KeepIntegral location (Vertex4 x y z w) = vertexAttrib4I location x y z w

   vertexAttribv ToFloat location = vertexAttrib4v location . (castPtr :: Ptr (Vertex4 b) -> Ptr b)
   vertexAttribv ToNormalizedFloat location = vertexAttrib4Nv location . (castPtr :: Ptr (Vertex4 b) -> Ptr b)
   vertexAttribv KeepIntegral location = vertexAttrib4Iv location . (castPtr :: Ptr (Vertex4 b) -> Ptr b)

instance VertexAttribComponent a => VertexAttrib (Vector1 a) where
   vertexAttrib ToFloat location (Vector1 i) = vertexAttrib1 location i
   vertexAttrib ToNormalizedFloat location (Vector1 i) = vertexAttrib1N location i
   vertexAttrib KeepIntegral location (Vector1 i) = vertexAttrib1I location i

   vertexAttribv ToFloat location = vertexAttrib1v location . (castPtr :: Ptr (Vector1 b) -> Ptr b)
   vertexAttribv ToNormalizedFloat location = vertexAttrib1Nv location . (castPtr :: Ptr (Vector1 b) -> Ptr b)
   vertexAttribv KeepIntegral location = vertexAttrib1Iv location . (castPtr :: Ptr (Vector1 b) -> Ptr b)

instance VertexAttribComponent a => VertexAttrib (Vector2 a) where
   vertexAttrib ToFloat location (Vector2 x y) = vertexAttrib2 location x y
   vertexAttrib ToNormalizedFloat location (Vector2 x y) = vertexAttrib2N location x y
   vertexAttrib KeepIntegral location (Vector2 x y) = vertexAttrib2I location x y

   vertexAttribv ToFloat location = vertexAttrib2v location . (castPtr :: Ptr (Vector2 b) -> Ptr b)
   vertexAttribv ToNormalizedFloat location = vertexAttrib2Nv location . (castPtr :: Ptr (Vector2 b) -> Ptr b)
   vertexAttribv KeepIntegral location = vertexAttrib2Iv location . (castPtr :: Ptr (Vector2 b) -> Ptr b)

instance VertexAttribComponent a => VertexAttrib (Vector3 a) where
   vertexAttrib ToFloat location (Vector3 x y z) = vertexAttrib3 location x y z
   vertexAttrib ToNormalizedFloat location (Vector3 x y z) = vertexAttrib3N location x y z
   vertexAttrib KeepIntegral location (Vector3 x y z) = vertexAttrib3I location x y z

   vertexAttribv ToFloat location = vertexAttrib3v location . (castPtr :: Ptr (Vector3 b) -> Ptr b)
   vertexAttribv ToNormalizedFloat location = vertexAttrib3Nv location . (castPtr :: Ptr (Vector3 b) -> Ptr b)
   vertexAttribv KeepIntegral location = vertexAttrib3Iv location . (castPtr :: Ptr (Vector3 b) -> Ptr b)

instance VertexAttribComponent a => VertexAttrib (Vector4 a) where
   vertexAttrib ToFloat location (Vector4 x y z w) = vertexAttrib4 location x y z w
   vertexAttrib ToNormalizedFloat location (Vector4 x y z w) = vertexAttrib4N location x y z w
   vertexAttrib KeepIntegral location (Vector4 x y z w) = vertexAttrib4I location x y z w

   vertexAttribv ToFloat location = vertexAttrib4v location . (castPtr :: Ptr (Vector4 b) -> Ptr b)
   vertexAttribv ToNormalizedFloat location = vertexAttrib4Nv location . (castPtr :: Ptr (Vector4 b) -> Ptr b)
   vertexAttribv KeepIntegral location = vertexAttrib4Iv location . (castPtr :: Ptr (Vector4 b) -> Ptr b)

instance VertexAttribComponent a => VertexAttrib (TexCoord1 a) where
   vertexAttrib ToFloat location (TexCoord1 s) = vertexAttrib1 location s
   vertexAttrib ToNormalizedFloat location (TexCoord1 s) = vertexAttrib1N location s
   vertexAttrib KeepIntegral location (TexCoord1 s) = vertexAttrib1I location s

   vertexAttribv ToFloat location = vertexAttrib1v location . (castPtr :: Ptr (TexCoord1 b) -> Ptr b)
   vertexAttribv ToNormalizedFloat location = vertexAttrib1Nv location . (castPtr :: Ptr (TexCoord1 b) -> Ptr b)
   vertexAttribv KeepIntegral location = vertexAttrib1Iv location . (castPtr :: Ptr (TexCoord1 b) -> Ptr b)

instance VertexAttribComponent a => VertexAttrib (TexCoord2 a) where
   vertexAttrib ToFloat location (TexCoord2 s t) = vertexAttrib2 location s t
   vertexAttrib ToNormalizedFloat location (TexCoord2 s t) = vertexAttrib2N location s t
   vertexAttrib KeepIntegral location (TexCoord2 s t) = vertexAttrib2I location s t

   vertexAttribv ToFloat location = vertexAttrib2v location . (castPtr :: Ptr (TexCoord2 b) -> Ptr b)
   vertexAttribv ToNormalizedFloat location = vertexAttrib2Nv location . (castPtr :: Ptr (TexCoord2 b) -> Ptr b)
   vertexAttribv KeepIntegral location = vertexAttrib2Iv location . (castPtr :: Ptr (TexCoord2 b) -> Ptr b)

instance VertexAttribComponent a => VertexAttrib (TexCoord3 a) where
   vertexAttrib ToFloat location (TexCoord3 s t u) = vertexAttrib3 location s t u
   vertexAttrib ToNormalizedFloat location (TexCoord3 s t u) = vertexAttrib3N location s t u
   vertexAttrib KeepIntegral location (TexCoord3 s t u) = vertexAttrib3I location s t u

   vertexAttribv ToFloat location = vertexAttrib3v location . (castPtr :: Ptr (TexCoord3 b) -> Ptr b)
   vertexAttribv ToNormalizedFloat location = vertexAttrib3Nv location . (castPtr :: Ptr (TexCoord3 b) -> Ptr b)
   vertexAttribv KeepIntegral location = vertexAttrib3Iv location . (castPtr :: Ptr (TexCoord3 b) -> Ptr b)

instance VertexAttribComponent a => VertexAttrib (TexCoord4 a) where
   vertexAttrib ToFloat location (TexCoord4 s t u v) = vertexAttrib4 location s t u v
   vertexAttrib ToNormalizedFloat location (TexCoord4 s t u v) = vertexAttrib4N location s t u v
   vertexAttrib KeepIntegral location (TexCoord4 s t u v) = vertexAttrib4I location s t u v

   vertexAttribv ToFloat location = vertexAttrib4v location . (castPtr :: Ptr (TexCoord4 b) -> Ptr b)
   vertexAttribv ToNormalizedFloat location = vertexAttrib4Nv location . (castPtr :: Ptr (TexCoord4 b) -> Ptr b)
   vertexAttribv KeepIntegral location = vertexAttrib4Iv location . (castPtr :: Ptr (TexCoord4 b) -> Ptr b)

instance VertexAttribComponent a => VertexAttrib (Normal3 a) where
   vertexAttrib ToFloat location (Normal3 x y z) = vertexAttrib3 location x y z
   vertexAttrib ToNormalizedFloat location (Normal3 x y z) = vertexAttrib3N location x y z
   vertexAttrib KeepIntegral location (Normal3 x y z) = vertexAttrib3I location x y z

   vertexAttribv ToFloat location = vertexAttrib3v location . (castPtr :: Ptr (Normal3 b) -> Ptr b)
   vertexAttribv ToNormalizedFloat location = vertexAttrib3Nv location . (castPtr :: Ptr (Normal3 b) -> Ptr b)
   vertexAttribv KeepIntegral location = vertexAttrib3Iv location . (castPtr :: Ptr (Normal3 b) -> Ptr b)

instance VertexAttribComponent a => VertexAttrib (FogCoord1 a) where
   vertexAttrib ToFloat location (FogCoord1 c) = vertexAttrib1 location c
   vertexAttrib ToNormalizedFloat location (FogCoord1 c) = vertexAttrib1N location c
   vertexAttrib KeepIntegral location (FogCoord1 c) = vertexAttrib1I location c

   vertexAttribv ToFloat location = vertexAttrib1v location . (castPtr :: Ptr (FogCoord1 b) -> Ptr b)
   vertexAttribv ToNormalizedFloat location = vertexAttrib1Nv location . (castPtr :: Ptr (FogCoord1 b) -> Ptr b)
   vertexAttribv KeepIntegral location = vertexAttrib1Iv location . (castPtr :: Ptr (FogCoord1 b) -> Ptr b)

instance VertexAttribComponent a => VertexAttrib (Color3 a) where
   vertexAttrib ToFloat location (Color3 r g b) = vertexAttrib3 location r g b
   vertexAttrib ToNormalizedFloat location (Color3 r g b) = vertexAttrib3N location r g b
   vertexAttrib KeepIntegral location (Color3 r g b) = vertexAttrib3I location r g b

   vertexAttribv ToFloat location = vertexAttrib3v location . (castPtr :: Ptr (Color3 b) -> Ptr b)
   vertexAttribv ToNormalizedFloat location = vertexAttrib3Nv location . (castPtr :: Ptr (Color3 b) -> Ptr b)
   vertexAttribv KeepIntegral location = vertexAttrib3Iv location . (castPtr :: Ptr (Color3 b) -> Ptr b)

instance VertexAttribComponent a => VertexAttrib (Color4 a) where
   vertexAttrib ToFloat location (Color4 r g b a) = vertexAttrib4 location r g b a
   vertexAttrib ToNormalizedFloat location (Color4 r g b a) = vertexAttrib4N location r g b a
   vertexAttrib KeepIntegral location (Color4 r g b a) = vertexAttrib4I location r g b a

   vertexAttribv ToFloat location = vertexAttrib4v location . (castPtr :: Ptr (Color4 b) -> Ptr b)
   vertexAttribv ToNormalizedFloat location = vertexAttrib4Nv location . (castPtr :: Ptr (Color4 b) -> Ptr b)
   vertexAttribv KeepIntegral location = vertexAttrib4Iv location . (castPtr :: Ptr (Color4 b) -> Ptr b)

instance VertexAttribComponent a => VertexAttrib (Index1 a) where
   vertexAttrib ToFloat location (Index1 i) = vertexAttrib1 location i
   vertexAttrib ToNormalizedFloat location (Index1 i) = vertexAttrib1N location i
   vertexAttrib KeepIntegral location (Index1 i) = vertexAttrib1I location i

   vertexAttribv ToFloat location = vertexAttrib1v location . (castPtr :: Ptr (Index1 b) -> Ptr b)
   vertexAttribv ToNormalizedFloat location = vertexAttrib1Nv location . (castPtr :: Ptr (Index1 b) -> Ptr b)
   vertexAttribv KeepIntegral location = vertexAttrib1Iv location . (castPtr :: Ptr (Index1 b) -> Ptr b)

--------------------------------------------------------------------------------

-- | An implementation must support at least 2 texture units, but it may
-- support up to 32 ones. This state variable can be used to query the actual
-- implementation limit.

maxTextureUnit :: GettableStateVar TextureUnit
maxTextureUnit =
   makeGettableStateVar (getEnum1 (TextureUnit . fromIntegral) GetMaxTextureUnits)
