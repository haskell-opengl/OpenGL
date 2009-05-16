--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.VertexSpec
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 2.7 (Vertex Specification) of the
-- OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.VertexSpec (
   -- * Vertex Coordinates
   Vertex(..),
   VertexComponent,
   Vertex1(..), Vertex2(..), Vertex3(..), Vertex4(..),

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

import Data.Int
import Data.Word
import Foreign.Marshal.Array ( allocaArray )
import Foreign.Ptr ( Ptr, castPtr )
import Foreign.Storable ( Storable(..) )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLbyte, GLshort, GLint, GLubyte, GLushort, GLuint, GLfloat,
   GLdouble )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.GLboolean ( unmarshalGLboolean )
import Graphics.Rendering.OpenGL.GL.PeekPoke (
   poke4, peek1M, peek2M, peek3M, peek4M )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   AttribLocation(..),
   GetPName(GetCurrentTextureCoords, GetCurrentNormal, GetCurrentFogCoord,
            GetCurrentColor, GetCurrentSecondaryColor, GetCurrentIndex,
            GetMaxTextureUnits,GetRGBAMode),
   getBoolean1, getInteger1, getEnum1, getFloat1, getFloat3, getFloat4,
   GetVertexAttribPName(GetCurrentVertexAttrib),
   getVertexAttribFloat4, getVertexAttribIInteger4, getVertexAttribIuInteger4  )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.Tensor (
   Vertex1(..), Vertex2(..), Vertex3(..), Vertex4(..),
   Vector1(..), Vector2(..), Vector3(..), Vector4(..) )
import Graphics.Rendering.OpenGL.GL.Texturing.TextureUnit (
   TextureUnit(..), marshalTextureUnit, unmarshalTextureUnit )
import Graphics.Rendering.OpenGL.GL.VertexAttributes (
   TexCoord1(..), TexCoord2(..), TexCoord3(..), TexCoord4(..),
   Normal3(..), FogCoord1(..), Color3(..), Color4(..), Index1(..) )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"
#include "HsOpenGLTypes.h"

--------------------------------------------------------------------------------

-- | The class of all types which can be used as a vertex coordinate.

class VertexComponent a where
   vertex2 :: a -> a -> IO ()
   vertex3 :: a -> a -> a -> IO ()
   vertex4 :: a -> a -> a -> a -> IO ()

   vertex2v :: Ptr a -> IO ()
   vertex3v :: Ptr a -> IO ()
   vertex4v :: Ptr a -> IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glVertex2s" glVertex2s ::
   GLshort -> GLshort -> IO ()

foreign import CALLCONV unsafe "glVertex3s" glVertex3s ::
   GLshort -> GLshort -> GLshort -> IO ()

foreign import CALLCONV unsafe "glVertex4s" glVertex4s ::
   GLshort -> GLshort -> GLshort -> GLshort -> IO ()

foreign import CALLCONV unsafe "glVertex2sv" glVertex2sv ::
   Ptr GLshort -> IO ()

foreign import CALLCONV unsafe "glVertex3sv" glVertex3sv ::
   Ptr GLshort -> IO ()

foreign import CALLCONV unsafe "glVertex4sv" glVertex4sv ::
   Ptr GLshort -> IO ()

instance VertexComponent GLshort_ where
   vertex2 = glVertex2s
   vertex3 = glVertex3s
   vertex4 = glVertex4s

   vertex2v = glVertex2sv
   vertex3v = glVertex3sv
   vertex4v = glVertex4sv

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glVertex2i" glVertex2i ::
   GLint -> GLint -> IO ()

foreign import CALLCONV unsafe "glVertex3i" glVertex3i ::
   GLint -> GLint -> GLint -> IO ()

foreign import CALLCONV unsafe "glVertex4i" glVertex4i ::
   GLint -> GLint -> GLint -> GLint -> IO ()

foreign import CALLCONV unsafe "glVertex2iv" glVertex2iv ::
   Ptr GLint -> IO ()

foreign import CALLCONV unsafe "glVertex3iv" glVertex3iv ::
   Ptr GLint -> IO ()

foreign import CALLCONV unsafe "glVertex4iv" glVertex4iv ::
   Ptr GLint -> IO ()

instance VertexComponent GLint_ where
   vertex2 = glVertex2i
   vertex3 = glVertex3i
   vertex4 = glVertex4i

   vertex2v = glVertex2iv
   vertex3v = glVertex3iv
   vertex4v = glVertex4iv

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glVertex2f" glVertex2f ::
   GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glVertex3f" glVertex3f ::
   GLfloat -> GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glVertex4f" glVertex4f ::
   GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glVertex2fv" glVertex2fv ::
   Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glVertex3fv" glVertex3fv ::
   Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glVertex4fv" glVertex4fv ::
   Ptr GLfloat -> IO ()

instance VertexComponent GLfloat_ where
   vertex2 = glVertex2f
   vertex3 = glVertex3f
   vertex4 = glVertex4f

   vertex2v = glVertex2fv
   vertex3v = glVertex3fv
   vertex4v = glVertex4fv

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glVertex2d" glVertex2d ::
   GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glVertex3d" glVertex3d ::
   GLdouble -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glVertex4d" glVertex4d ::
   GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glVertex2dv" glVertex2dv ::
   Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glVertex3dv" glVertex3dv ::
   Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glVertex4dv" glVertex4dv ::
   Ptr GLdouble -> IO ()

instance VertexComponent GLdouble_ where
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

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glTexCoord1s" glTexCoord1s ::
   GLshort -> IO ()

foreign import CALLCONV unsafe "glTexCoord2s" glTexCoord2s ::
   GLshort -> GLshort -> IO ()

foreign import CALLCONV unsafe "glTexCoord3s" glTexCoord3s ::
   GLshort -> GLshort -> GLshort -> IO ()

foreign import CALLCONV unsafe "glTexCoord4s" glTexCoord4s ::
   GLshort -> GLshort -> GLshort -> GLshort -> IO ()

foreign import CALLCONV unsafe "glTexCoord1sv" glTexCoord1sv ::
   Ptr GLshort -> IO ()

foreign import CALLCONV unsafe "glTexCoord2sv" glTexCoord2sv ::
   Ptr GLshort -> IO ()

foreign import CALLCONV unsafe "glTexCoord3sv" glTexCoord3sv ::
   Ptr GLshort -> IO ()

foreign import CALLCONV unsafe "glTexCoord4sv" glTexCoord4sv ::
   Ptr GLshort -> IO ()

EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord1sARB,GLenum -> GLshort -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord2sARB,GLenum -> GLshort -> GLshort -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord3sARB,GLenum -> GLshort -> GLshort -> GLshort -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord4sARB,GLenum -> GLshort -> GLshort -> GLshort -> GLshort -> IO ())

EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord1svARB,GLenum -> Ptr GLshort -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord2svARB,GLenum -> Ptr GLshort -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord3svARB,GLenum -> Ptr GLshort -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord4svARB,GLenum -> Ptr GLshort -> IO ())

instance TexCoordComponent GLshort_ where
   texCoord1 = glTexCoord1s
   texCoord2 = glTexCoord2s
   texCoord3 = glTexCoord3s
   texCoord4 = glTexCoord4s

   texCoord1v = glTexCoord1sv
   texCoord2v = glTexCoord2sv
   texCoord3v = glTexCoord3sv
   texCoord4v = glTexCoord4sv

   multiTexCoord1 = glMultiTexCoord1sARB
   multiTexCoord2 = glMultiTexCoord2sARB
   multiTexCoord3 = glMultiTexCoord3sARB
   multiTexCoord4 = glMultiTexCoord4sARB

   multiTexCoord1v = glMultiTexCoord1svARB
   multiTexCoord2v = glMultiTexCoord2svARB
   multiTexCoord3v = glMultiTexCoord3svARB
   multiTexCoord4v = glMultiTexCoord4svARB

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glTexCoord1i" glTexCoord1i ::
   GLint -> IO ()

foreign import CALLCONV unsafe "glTexCoord2i" glTexCoord2i ::
   GLint -> GLint -> IO ()

foreign import CALLCONV unsafe "glTexCoord3i" glTexCoord3i ::
   GLint -> GLint -> GLint -> IO ()

foreign import CALLCONV unsafe "glTexCoord4i" glTexCoord4i ::
   GLint -> GLint -> GLint -> GLint -> IO ()

foreign import CALLCONV unsafe "glTexCoord1iv" glTexCoord1iv ::
   Ptr GLint -> IO ()

foreign import CALLCONV unsafe "glTexCoord2iv" glTexCoord2iv ::
   Ptr GLint -> IO ()

foreign import CALLCONV unsafe "glTexCoord3iv" glTexCoord3iv ::
   Ptr GLint -> IO ()

foreign import CALLCONV unsafe "glTexCoord4iv" glTexCoord4iv ::
   Ptr GLint -> IO ()

EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord1iARB,GLenum -> GLint -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord2iARB,GLenum -> GLint -> GLint -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord3iARB,GLenum -> GLint -> GLint -> GLint -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord4iARB,GLenum -> GLint -> GLint -> GLint -> GLint -> IO ())

EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord1ivARB,GLenum -> Ptr GLint -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord2ivARB,GLenum -> Ptr GLint -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord3ivARB,GLenum -> Ptr GLint -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord4ivARB,GLenum -> Ptr GLint -> IO ())

instance TexCoordComponent GLint_ where
   texCoord1 = glTexCoord1i
   texCoord2 = glTexCoord2i
   texCoord3 = glTexCoord3i
   texCoord4 = glTexCoord4i

   texCoord1v = glTexCoord1iv
   texCoord2v = glTexCoord2iv
   texCoord3v = glTexCoord3iv
   texCoord4v = glTexCoord4iv

   multiTexCoord1 = glMultiTexCoord1iARB
   multiTexCoord2 = glMultiTexCoord2iARB
   multiTexCoord3 = glMultiTexCoord3iARB
   multiTexCoord4 = glMultiTexCoord4iARB

   multiTexCoord1v = glMultiTexCoord1ivARB
   multiTexCoord2v = glMultiTexCoord2ivARB
   multiTexCoord3v = glMultiTexCoord3ivARB
   multiTexCoord4v = glMultiTexCoord4ivARB

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glTexCoord1f" glTexCoord1f ::
   GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexCoord2f" glTexCoord2f ::
   GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexCoord3f" glTexCoord3f ::
   GLfloat -> GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexCoord4f" glTexCoord4f ::
   GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexCoord1fv" glTexCoord1fv ::
   Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexCoord2fv" glTexCoord2fv ::
   Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexCoord3fv" glTexCoord3fv ::
   Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexCoord4fv" glTexCoord4fv ::
   Ptr GLfloat -> IO ()

EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord1fARB,GLenum -> GLfloat -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord2fARB,GLenum -> GLfloat -> GLfloat -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord3fARB,GLenum -> GLfloat -> GLfloat -> GLfloat -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord4fARB,GLenum -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())

EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord1fvARB,GLenum -> Ptr GLfloat -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord2fvARB,GLenum -> Ptr GLfloat -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord3fvARB,GLenum -> Ptr GLfloat -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord4fvARB,GLenum -> Ptr GLfloat -> IO ())

instance TexCoordComponent GLfloat_ where
   texCoord1 = glTexCoord1f
   texCoord2 = glTexCoord2f
   texCoord3 = glTexCoord3f
   texCoord4 = glTexCoord4f

   texCoord1v = glTexCoord1fv
   texCoord2v = glTexCoord2fv
   texCoord3v = glTexCoord3fv
   texCoord4v = glTexCoord4fv

   multiTexCoord1 = glMultiTexCoord1fARB
   multiTexCoord2 = glMultiTexCoord2fARB
   multiTexCoord3 = glMultiTexCoord3fARB
   multiTexCoord4 = glMultiTexCoord4fARB

   multiTexCoord1v = glMultiTexCoord1fvARB
   multiTexCoord2v = glMultiTexCoord2fvARB
   multiTexCoord3v = glMultiTexCoord3fvARB
   multiTexCoord4v = glMultiTexCoord4fvARB

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glTexCoord1d" glTexCoord1d ::
   GLdouble -> IO ()

foreign import CALLCONV unsafe "glTexCoord2d" glTexCoord2d ::
   GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glTexCoord3d" glTexCoord3d ::
   GLdouble -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glTexCoord4d" glTexCoord4d ::
   GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glTexCoord1dv" glTexCoord1dv ::
   Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glTexCoord2dv" glTexCoord2dv ::
   Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glTexCoord3dv" glTexCoord3dv ::
   Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glTexCoord4dv" glTexCoord4dv ::
   Ptr GLdouble -> IO ()

EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord1dARB,GLenum -> GLdouble -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord2dARB,GLenum -> GLdouble -> GLdouble -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord3dARB,GLenum -> GLdouble -> GLdouble -> GLdouble -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord4dARB,GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())

EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord1dvARB,GLenum -> Ptr GLdouble -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord2dvARB,GLenum -> Ptr GLdouble -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord3dvARB,GLenum -> Ptr GLdouble -> IO ())
EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glMultiTexCoord4dvARB,GLenum -> Ptr GLdouble -> IO ())

instance TexCoordComponent GLdouble_ where
   texCoord1 = glTexCoord1d
   texCoord2 = glTexCoord2d
   texCoord3 = glTexCoord3d
   texCoord4 = glTexCoord4d

   texCoord1v = glTexCoord1dv
   texCoord2v = glTexCoord2dv
   texCoord3v = glTexCoord3dv
   texCoord4v = glTexCoord4dv

   multiTexCoord1 = glMultiTexCoord1dARB
   multiTexCoord2 = glMultiTexCoord2dARB
   multiTexCoord3 = glMultiTexCoord3dARB
   multiTexCoord4 = glMultiTexCoord4dARB

   multiTexCoord1v = glMultiTexCoord1dvARB
   multiTexCoord2v = glMultiTexCoord2dvARB
   multiTexCoord3v = glMultiTexCoord3dvARB
   multiTexCoord4v = glMultiTexCoord4dvARB

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

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glNormal3b" glNormal3b ::
   GLbyte -> GLbyte -> GLbyte -> IO ()

foreign import CALLCONV unsafe "glNormal3bv" glNormal3bv ::
   Ptr GLbyte -> IO ()

instance NormalComponent GLbyte_ where
   normal3 = glNormal3b
   normal3v = glNormal3bv

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glNormal3s" glNormal3s ::
   GLshort -> GLshort -> GLshort -> IO ()

foreign import CALLCONV unsafe "glNormal3sv" glNormal3sv ::
   Ptr GLshort -> IO ()

instance NormalComponent GLshort_ where
   normal3 = glNormal3s
   normal3v = glNormal3sv

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glNormal3i" glNormal3i ::
   GLint -> GLint -> GLint -> IO ()

foreign import CALLCONV unsafe "glNormal3iv" glNormal3iv ::
   Ptr GLint -> IO ()

instance NormalComponent GLint_ where
   normal3 = glNormal3i
   normal3v = glNormal3iv

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glNormal3f" glNormal3f ::
   GLfloat -> GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glNormal3fv" glNormal3fv ::
   Ptr GLfloat -> IO ()

instance NormalComponent GLfloat_ where
   normal3 = glNormal3f
   normal3v = glNormal3fv

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glNormal3d" glNormal3d ::
   GLdouble -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glNormal3dv" glNormal3dv ::
   Ptr GLdouble -> IO ()

instance NormalComponent GLdouble_ where
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

--------------------------------------------------------------------------------

EXTENSION_ENTRY("GL_EXT_fog_coord or OpenGL 1.4",glFogCoordfEXT,GLfloat -> IO ())
EXTENSION_ENTRY("GL_EXT_fog_coord or OpenGL 1.4",glFogCoordfvEXT,Ptr GLfloat -> IO ())

instance FogCoordComponent GLfloat_ where
   fogCoord1 = glFogCoordfEXT
   fogCoord1v = glFogCoordfvEXT

--------------------------------------------------------------------------------

EXTENSION_ENTRY("GL_EXT_fog_coord or OpenGL 1.4",glFogCoorddEXT,GLdouble -> IO ())
EXTENSION_ENTRY("GL_EXT_fog_coord or OpenGL 1.4",glFogCoorddvEXT,Ptr GLdouble -> IO ())

instance FogCoordComponent GLdouble_ where
   fogCoord1 = glFogCoorddEXT
   fogCoord1v = glFogCoorddvEXT

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

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glColor3b" glColor3b ::
   GLbyte -> GLbyte -> GLbyte -> IO ()

foreign import CALLCONV unsafe "glColor4b" glColor4b ::
   GLbyte -> GLbyte -> GLbyte -> GLbyte -> IO ()

foreign import CALLCONV unsafe "glColor3bv" glColor3bv ::
   Ptr GLbyte -> IO ()

foreign import CALLCONV unsafe "glColor4bv" glColor4bv ::
   Ptr GLbyte -> IO ()

EXTENSION_ENTRY("GL_EXT_secondary_color or OpenGL 1.4",glSecondaryColor3bEXT,GLbyte -> GLbyte -> GLbyte -> IO ())
EXTENSION_ENTRY("GL_EXT_secondary_color or OpenGL 1.4",glSecondaryColor3bvEXT,Ptr GLbyte -> IO ())

instance ColorComponent GLbyte_ where
   color3 = glColor3b
   color4 = glColor4b

   color3v = glColor3bv
   color4v = glColor4bv

   secondaryColor3 = glSecondaryColor3bEXT
   secondaryColor3v = glSecondaryColor3bvEXT

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glColor3s" glColor3s ::
   GLshort -> GLshort -> GLshort -> IO ()

foreign import CALLCONV unsafe "glColor4s" glColor4s ::
   GLshort -> GLshort -> GLshort -> GLshort -> IO ()

foreign import CALLCONV unsafe "glColor3sv" glColor3sv ::
   Ptr GLshort -> IO ()

foreign import CALLCONV unsafe "glColor4sv" glColor4sv ::
   Ptr GLshort -> IO ()

EXTENSION_ENTRY("GL_EXT_secondary_color or OpenGL 1.4",glSecondaryColor3sEXT,GLshort -> GLshort -> GLshort -> IO ())
EXTENSION_ENTRY("GL_EXT_secondary_color or OpenGL 1.4",glSecondaryColor3svEXT,Ptr GLshort -> IO ())

instance ColorComponent GLshort_ where
   color3 = glColor3s
   color4 = glColor4s

   color3v = glColor3sv
   color4v = glColor4sv

   secondaryColor3 = glSecondaryColor3sEXT
   secondaryColor3v = glSecondaryColor3svEXT

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glColor3i" glColor3i ::
   GLint -> GLint -> GLint -> IO ()

foreign import CALLCONV unsafe "glColor4i" glColor4i ::
   GLint -> GLint -> GLint -> GLint -> IO ()

foreign import CALLCONV unsafe "glColor3iv" glColor3iv ::
   Ptr GLint -> IO ()

foreign import CALLCONV unsafe "glColor4iv" glColor4iv ::
   Ptr GLint -> IO ()

EXTENSION_ENTRY("GL_EXT_secondary_color or OpenGL 1.4",glSecondaryColor3iEXT,GLint -> GLint -> GLint -> IO ())
EXTENSION_ENTRY("GL_EXT_secondary_color or OpenGL 1.4",glSecondaryColor3ivEXT,Ptr GLint -> IO ())

instance ColorComponent GLint_ where
   color3 = glColor3i
   color4 = glColor4i

   color3v = glColor3iv
   color4v = glColor4iv

   secondaryColor3 = glSecondaryColor3iEXT
   secondaryColor3v = glSecondaryColor3ivEXT

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glColor3f" glColor3f ::
   GLfloat -> GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glColor4f" glColor4f ::
   GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glColor3fv" glColor3fv ::
   Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glColor4fv" glColor4fv ::
   Ptr GLfloat -> IO ()

EXTENSION_ENTRY("GL_EXT_secondary_color or OpenGL 1.4",glSecondaryColor3fEXT,GLfloat -> GLfloat -> GLfloat -> IO ())
EXTENSION_ENTRY("GL_EXT_secondary_color or OpenGL 1.4",glSecondaryColor3fvEXT,Ptr GLfloat -> IO ())

instance ColorComponent GLfloat_ where
   color3 = glColor3f
   color4 = glColor4f

   color3v = glColor3fv
   color4v = glColor4fv

   secondaryColor3 = glSecondaryColor3fEXT
   secondaryColor3v = glSecondaryColor3fvEXT

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glColor3d" glColor3d ::
   GLdouble -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glColor4d" glColor4d ::
   GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glColor3dv" glColor3dv ::
   Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glColor4dv" glColor4dv ::
   Ptr GLdouble -> IO ()

EXTENSION_ENTRY("GL_EXT_secondary_color or OpenGL 1.4",glSecondaryColor3dEXT,GLdouble -> GLdouble -> GLdouble -> IO ())
EXTENSION_ENTRY("GL_EXT_secondary_color or OpenGL 1.4",glSecondaryColor3dvEXT,Ptr GLdouble -> IO ())

instance ColorComponent GLdouble_ where
   color3 = glColor3d
   color4 = glColor4d

   color3v = glColor3dv
   color4v = glColor4dv

   secondaryColor3 = glSecondaryColor3dEXT
   secondaryColor3v = glSecondaryColor3dvEXT

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glColor3ub" glColor3ub ::
   GLubyte -> GLubyte -> GLubyte -> IO ()

foreign import CALLCONV unsafe "glColor4ub" glColor4ub ::
   GLubyte -> GLubyte -> GLubyte -> GLubyte -> IO ()


foreign import CALLCONV unsafe "glColor3ubv" glColor3ubv ::
   Ptr GLubyte -> IO ()

foreign import CALLCONV unsafe "glColor4ubv" glColor4ubv ::
   Ptr GLubyte -> IO ()

EXTENSION_ENTRY("GL_EXT_secondary_color or OpenGL 1.4",glSecondaryColor3ubEXT,GLubyte -> GLubyte -> GLubyte -> IO ())
EXTENSION_ENTRY("GL_EXT_secondary_color or OpenGL 1.4",glSecondaryColor3ubvEXT,Ptr GLubyte -> IO ())

instance ColorComponent GLubyte_ where
   color3 = glColor3ub
   color4 = glColor4ub

   color3v = glColor3ubv
   color4v = glColor4ubv

   secondaryColor3 = glSecondaryColor3ubEXT
   secondaryColor3v = glSecondaryColor3ubvEXT

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glColor3us" glColor3us ::
   GLushort -> GLushort -> GLushort -> IO ()

foreign import CALLCONV unsafe "glColor4us" glColor4us ::
   GLushort -> GLushort -> GLushort -> GLushort -> IO ()

foreign import CALLCONV unsafe "glColor3usv" glColor3usv ::
   Ptr GLushort -> IO ()

foreign import CALLCONV unsafe "glColor4usv" glColor4usv ::
   Ptr GLushort -> IO ()

EXTENSION_ENTRY("GL_EXT_secondary_color or OpenGL 1.4",glSecondaryColor3usEXT,GLushort -> GLushort -> GLushort -> IO ())
EXTENSION_ENTRY("GL_EXT_secondary_color or OpenGL 1.4",glSecondaryColor3usvEXT,Ptr GLushort -> IO ())

instance ColorComponent GLushort_ where
   color3 = glColor3us
   color4 = glColor4us

   color3v = glColor3usv
   color4v = glColor4usv

   secondaryColor3 = glSecondaryColor3usEXT
   secondaryColor3v = glSecondaryColor3usvEXT

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glColor3ui" glColor3ui ::
   GLuint -> GLuint -> GLuint -> IO ()

foreign import CALLCONV unsafe "glColor4ui" glColor4ui ::
   GLuint -> GLuint -> GLuint -> GLuint -> IO ()

foreign import CALLCONV unsafe "glColor3uiv" glColor3uiv ::
   Ptr GLuint -> IO ()

foreign import CALLCONV unsafe "glColor4uiv" glColor4uiv ::
   Ptr GLuint -> IO ()

EXTENSION_ENTRY("GL_EXT_secondary_color or OpenGL 1.4",glSecondaryColor3uiEXT,GLuint -> GLuint -> GLuint -> IO ())
EXTENSION_ENTRY("GL_EXT_secondary_color or OpenGL 1.4",glSecondaryColor3uivEXT,Ptr GLuint -> IO ())

instance ColorComponent GLuint_ where
   color3 = glColor3ui
   color4 = glColor4ui

   color3v = glColor3uiv
   color4v = glColor4uiv

   secondaryColor3 = glSecondaryColor3uiEXT
   secondaryColor3v = glSecondaryColor3uivEXT

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

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glIndexs" glIndexs ::
   GLshort -> IO ()

foreign import CALLCONV unsafe "glIndexsv" glIndexsv ::
   Ptr GLshort -> IO ()

instance IndexComponent GLshort_ where
   index1 = glIndexs
   index1v = glIndexsv

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glIndexi" glIndexi ::
   GLint -> IO ()

foreign import CALLCONV unsafe "glIndexiv" glIndexiv ::
   Ptr GLint -> IO ()

instance IndexComponent GLint_ where
   index1 = glIndexi
   index1v = glIndexiv

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glIndexf" glIndexf ::
   GLfloat -> IO ()

foreign import CALLCONV unsafe "glIndexfv" glIndexfv ::
   Ptr GLfloat -> IO ()

instance IndexComponent GLfloat_ where
   index1 = glIndexf
   index1v = glIndexfv

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glIndexd" glIndexd ::
   GLdouble -> IO ()

foreign import CALLCONV unsafe "glIndexdv" glIndexdv ::
   Ptr GLdouble -> IO ()

instance IndexComponent GLdouble_ where
   index1 = glIndexd
   index1v = glIndexdv

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glIndexub" glIndexub ::
   GLubyte -> IO ()

foreign import CALLCONV unsafe "glIndexubv" glIndexubv ::
   Ptr GLubyte -> IO ()

instance IndexComponent GLubyte_ where
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

--------------------------------------------------------------------------------

EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib4bvARB,AttribLocation -> Ptr GLbyte -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib4NbvARB,AttribLocation -> Ptr GLbyte -> IO ())
EXTENSION_ENTRY("GL_EXT_gpu_shader4 or OpenGL 3.0",glVertexAttribI4bv,AttribLocation -> Ptr GLbyte -> IO ())

instance VertexAttribComponent GLbyte_ where
   vertexAttrib4v = glVertexAttrib4bvARB
   vertexAttrib4Nv = glVertexAttrib4NbvARB
   vertexAttrib4Iv = glVertexAttribI4bv

--------------------------------------------------------------------------------

EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib4NubARB,AttribLocation -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib4ubvARB,AttribLocation -> Ptr GLubyte -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib4NubvARB,AttribLocation -> Ptr GLubyte -> IO ())
EXTENSION_ENTRY("GL_EXT_gpu_shader4 or OpenGL 3.0",glVertexAttribI4ubv,AttribLocation -> Ptr GLubyte -> IO ())

instance VertexAttribComponent GLubyte_ where
   vertexAttrib4N = glVertexAttrib4NubARB
   vertexAttrib4v = glVertexAttrib4ubvARB
   vertexAttrib4Nv = glVertexAttrib4NubvARB
   vertexAttrib4Iv = glVertexAttribI4ubv

--------------------------------------------------------------------------------

EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib1sARB,AttribLocation -> GLshort -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib2sARB,AttribLocation -> GLshort -> GLshort -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib3sARB,AttribLocation -> GLshort -> GLshort -> GLshort -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib4sARB,AttribLocation -> GLshort -> GLshort -> GLshort -> GLshort -> IO ())

EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib1svARB,AttribLocation -> Ptr GLshort -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib2svARB,AttribLocation -> Ptr GLshort -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib3svARB,AttribLocation -> Ptr GLshort -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib4svARB,AttribLocation -> Ptr GLshort -> IO ())

EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib4NsvARB,AttribLocation -> Ptr GLshort -> IO ())

EXTENSION_ENTRY("GL_EXT_gpu_shader4 or OpenGL 3.0",glVertexAttribI4sv,AttribLocation -> Ptr GLshort -> IO ())

instance VertexAttribComponent GLshort_ where
   vertexAttrib1 = glVertexAttrib1sARB
   vertexAttrib2 = glVertexAttrib2sARB
   vertexAttrib3 = glVertexAttrib3sARB
   vertexAttrib4 = glVertexAttrib4sARB

   vertexAttrib1v = glVertexAttrib1svARB
   vertexAttrib2v = glVertexAttrib2svARB
   vertexAttrib3v = glVertexAttrib3svARB
   vertexAttrib4v = glVertexAttrib4svARB

   vertexAttrib4Nv = glVertexAttrib4NsvARB

   vertexAttrib4Iv = glVertexAttribI4sv

--------------------------------------------------------------------------------

EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib4usvARB,AttribLocation -> Ptr GLushort -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib4NusvARB,AttribLocation -> Ptr GLushort -> IO ())
EXTENSION_ENTRY("GL_EXT_gpu_shader4 or OpenGL 3.0",glVertexAttribI4usv,AttribLocation -> Ptr GLushort -> IO ())

instance VertexAttribComponent GLushort_ where
   vertexAttrib4v = glVertexAttrib4usvARB
   vertexAttrib4Nv = glVertexAttrib4NusvARB
   vertexAttrib4Iv = glVertexAttribI4usv

--------------------------------------------------------------------------------

EXTENSION_ENTRY("GL_EXT_gpu_shader4 or OpenGL 3.0",glVertexAttribI1i,AttribLocation -> GLint -> IO ())
EXTENSION_ENTRY("GL_EXT_gpu_shader4 or OpenGL 3.0",glVertexAttribI2i,AttribLocation -> GLint -> GLint -> IO ())
EXTENSION_ENTRY("GL_EXT_gpu_shader4 or OpenGL 3.0",glVertexAttribI3i,AttribLocation -> GLint -> GLint -> GLint -> IO ())
EXTENSION_ENTRY("GL_EXT_gpu_shader4 or OpenGL 3.0",glVertexAttribI4i,AttribLocation -> GLint -> GLint -> GLint -> GLint -> IO ())

EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib4ivARB,AttribLocation -> Ptr GLint -> IO ())

EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib4NivARB,AttribLocation -> Ptr GLint -> IO ())

EXTENSION_ENTRY("GL_EXT_gpu_shader4 or OpenGL 3.0",glVertexAttribI1iv,AttribLocation -> Ptr GLint -> IO ())
EXTENSION_ENTRY("GL_EXT_gpu_shader4 or OpenGL 3.0",glVertexAttribI2iv,AttribLocation -> Ptr GLint -> IO ())
EXTENSION_ENTRY("GL_EXT_gpu_shader4 or OpenGL 3.0",glVertexAttribI3iv,AttribLocation -> Ptr GLint -> IO ())
EXTENSION_ENTRY("GL_EXT_gpu_shader4 or OpenGL 3.0",glVertexAttribI4iv,AttribLocation -> Ptr GLint -> IO ())

instance VertexAttribComponent GLint_ where
   vertexAttrib1I = glVertexAttribI1i
   vertexAttrib2I = glVertexAttribI2i
   vertexAttrib3I = glVertexAttribI3i
   vertexAttrib4I = glVertexAttribI4i

   vertexAttrib4v = glVertexAttrib4ivARB

   vertexAttrib4Nv = glVertexAttrib4NivARB

   vertexAttrib1Iv = glVertexAttribI1iv
   vertexAttrib2Iv = glVertexAttribI2iv
   vertexAttrib3Iv = glVertexAttribI3iv
   vertexAttrib4Iv = glVertexAttribI4iv

--------------------------------------------------------------------------------

EXTENSION_ENTRY("GL_EXT_gpu_shader4 or OpenGL 3.0",glVertexAttribI1ui,AttribLocation -> GLuint -> IO ())
EXTENSION_ENTRY("GL_EXT_gpu_shader4 or OpenGL 3.0",glVertexAttribI2ui,AttribLocation -> GLuint -> GLuint -> IO ())
EXTENSION_ENTRY("GL_EXT_gpu_shader4 or OpenGL 3.0",glVertexAttribI3ui,AttribLocation -> GLuint -> GLuint -> GLuint -> IO ())
EXTENSION_ENTRY("GL_EXT_gpu_shader4 or OpenGL 3.0",glVertexAttribI4ui,AttribLocation -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())

EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib4uivARB,AttribLocation -> Ptr GLuint -> IO ())

EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib4NuivARB,AttribLocation -> Ptr GLuint -> IO ())

EXTENSION_ENTRY("GL_EXT_gpu_shader4 or OpenGL 3.0",glVertexAttribI1uiv,AttribLocation -> Ptr GLuint -> IO ())
EXTENSION_ENTRY("GL_EXT_gpu_shader4 or OpenGL 3.0",glVertexAttribI2uiv,AttribLocation -> Ptr GLuint -> IO ())
EXTENSION_ENTRY("GL_EXT_gpu_shader4 or OpenGL 3.0",glVertexAttribI3uiv,AttribLocation -> Ptr GLuint -> IO ())
EXTENSION_ENTRY("GL_EXT_gpu_shader4 or OpenGL 3.0",glVertexAttribI4uiv,AttribLocation -> Ptr GLuint -> IO ())

instance VertexAttribComponent GLuint_ where
   vertexAttrib1I = glVertexAttribI1ui
   vertexAttrib2I = glVertexAttribI2ui
   vertexAttrib3I = glVertexAttribI3ui
   vertexAttrib4I = glVertexAttribI4ui

   vertexAttrib4v = glVertexAttrib4uivARB

   vertexAttrib4Nv = glVertexAttrib4NuivARB

   vertexAttrib1Iv = glVertexAttribI1uiv
   vertexAttrib2Iv = glVertexAttribI2uiv
   vertexAttrib3Iv = glVertexAttribI3uiv
   vertexAttrib4Iv = glVertexAttribI4uiv

--------------------------------------------------------------------------------

EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib1fARB,AttribLocation -> GLfloat -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib2fARB,AttribLocation -> GLfloat -> GLfloat -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib3fARB,AttribLocation -> GLfloat -> GLfloat -> GLfloat -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib4fARB,AttribLocation -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())

EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib1fvARB,AttribLocation -> Ptr GLfloat -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib2fvARB,AttribLocation -> Ptr GLfloat -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib3fvARB,AttribLocation -> Ptr GLfloat -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib4fvARB,AttribLocation -> Ptr GLfloat -> IO ())

instance VertexAttribComponent GLfloat_ where
   vertexAttrib1 = glVertexAttrib1fARB
   vertexAttrib2 = glVertexAttrib2fARB
   vertexAttrib3 = glVertexAttrib3fARB
   vertexAttrib4 = glVertexAttrib4fARB

   vertexAttrib1v = glVertexAttrib1fvARB
   vertexAttrib2v = glVertexAttrib2fvARB
   vertexAttrib3v = glVertexAttrib3fvARB
   vertexAttrib4v = glVertexAttrib4fvARB

   vertexAttrib4Nv = vertexAttrib4v

   vertexAttrib4Iv = vertexAttrib4IvRealFrac

vertexAttrib4IvRealFrac :: (Storable a, RealFrac a) => AttribLocation -> Ptr a -> IO ()
vertexAttrib4IvRealFrac location = peek4M $ \x y z w ->
   vertexAttrib4I location (toGLint x) (toGLint y) (toGLint z) (toGLint w)

-- formula 2.6 from the OpenGL 3.1 spec
toGLint :: RealFrac a => a -> GLint
toGLint = truncate . (fromIntegral (maxBound :: GLint) *). clamp
   where clamp = max (-1.0) . min 1.0

--------------------------------------------------------------------------------

EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib1dARB,AttribLocation -> GLdouble -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib2dARB,AttribLocation -> GLdouble -> GLdouble -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib3dARB,AttribLocation -> GLdouble -> GLdouble -> GLdouble -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib4dARB,AttribLocation -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())

EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib1dvARB,AttribLocation -> Ptr GLdouble -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib2dvARB,AttribLocation -> Ptr GLdouble -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib3dvARB,AttribLocation -> Ptr GLdouble -> IO ())
EXTENSION_ENTRY("GL_ARB_vertex_shader or OpenGL 2.0",glVertexAttrib4dvARB,AttribLocation -> Ptr GLdouble -> IO ())

instance VertexAttribComponent GLdouble_ where
   vertexAttrib1 = glVertexAttrib1dARB
   vertexAttrib2 = glVertexAttrib2dARB
   vertexAttrib3 = glVertexAttrib3dARB
   vertexAttrib4 = glVertexAttrib4dARB

   vertexAttrib1v = glVertexAttrib1dvARB
   vertexAttrib2v = glVertexAttrib2dvARB
   vertexAttrib3v = glVertexAttrib3dvARB
   vertexAttrib4v = glVertexAttrib4dvARB

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
   makeGettableStateVar (getEnum1 unmarshalTextureUnit GetMaxTextureUnits)
