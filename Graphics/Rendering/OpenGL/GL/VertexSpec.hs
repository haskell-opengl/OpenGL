--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.VertexSpec
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.VertexSpec (
   VertexComponent(..), Vertex(..),
   Vertex2(..), Vertex3(..), Vertex4(..),

   TexCoordComponent(..), TexCoord(..),
   TexCoord1(..), TexCoord2(..), TexCoord3(..), TexCoord4(..),

   Normal(..), NormalComponent(..),
   Normal3(..),

   ColorComponent(..), Color(..),
   Color3(..), Color4(..),

   IndexComponent(..), Index(..),
   ColorIndex(..),
) where

import Foreign.Ptr ( Ptr, castPtr )
import Foreign.Storable ( Storable(..) )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLbyte, GLshort, GLint, GLubyte, GLushort, GLuint, GLfloat, GLdouble )

---------------------------------------------------------------------------

class VertexComponent a where
   vertex2 :: a -> a -> IO ()
   vertex3 :: a -> a -> a -> IO ()
   vertex4 :: a -> a -> a -> a -> IO ()
   vertex2v :: Ptr a -> IO ()
   vertex3v :: Ptr a -> IO ()
   vertex4v :: Ptr a -> IO ()

---------------------------------------------------------------------------

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

instance VertexComponent GLshort where
   vertex2 = glVertex2s
   vertex3 = glVertex3s
   vertex4 = glVertex4s
   vertex2v = glVertex2sv
   vertex3v = glVertex3sv
   vertex4v = glVertex4sv

---------------------------------------------------------------------------

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

instance VertexComponent GLint where
   vertex2 = glVertex2i
   vertex3 = glVertex3i
   vertex4 = glVertex4i
   vertex2v = glVertex2iv
   vertex3v = glVertex3iv
   vertex4v = glVertex4iv

---------------------------------------------------------------------------

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

instance VertexComponent GLfloat where
   vertex2 = glVertex2f
   vertex3 = glVertex3f
   vertex4 = glVertex4f
   vertex2v = glVertex2fv
   vertex3v = glVertex3fv
   vertex4v = glVertex4fv

---------------------------------------------------------------------------

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

instance VertexComponent GLdouble where
   vertex2 = glVertex2d
   vertex3 = glVertex3d
   vertex4 = glVertex4d
   vertex2v = glVertex2dv
   vertex3v = glVertex3dv
   vertex4v = glVertex4dv

---------------------------------------------------------------------------

class Vertex a where
   vertex  ::     a -> IO ()
   vertexv :: Ptr a -> IO ()

data Vertex2 a = Vertex2 a a
   deriving ( Eq, Ord, Show )

instance VertexComponent a => Vertex (Vertex2 a) where
   vertex (Vertex2 x y) = vertex2 x y
   vertexv = vertex2v . (castPtr :: Ptr (Vertex2 b) -> Ptr b)

instance Storable a => Storable (Vertex2 a) where
   sizeOf    ~(Vertex2 x _) = 2 * sizeOf x
   alignment ~(Vertex2 x _) = alignment x
   peek                     = peek2 Vertex2
   poke ptr   (Vertex2 x y) = poke2 ptr x y

data Vertex3 a = Vertex3 a a a
   deriving ( Eq, Ord, Show )

instance VertexComponent a => Vertex (Vertex3 a) where
   vertex (Vertex3 x y z) = vertex3 x y z
   vertexv = vertex3v . (castPtr :: Ptr (Vertex3 b) -> Ptr b)

instance Storable a => Storable (Vertex3 a) where
   sizeOf    ~(Vertex3 x _ _) = 3 * sizeOf x
   alignment ~(Vertex3 x _ _) = alignment x
   peek                       = peek3 Vertex3
   poke ptr   (Vertex3 x y z) = poke3 ptr x y z

data Vertex4 a = Vertex4 a a a a
   deriving ( Eq, Ord, Show )

instance VertexComponent a => Vertex (Vertex4 a) where
   vertex (Vertex4 x y z w) = vertex4 x y z w
   vertexv = vertex4v . (castPtr :: Ptr (Vertex4 b) -> Ptr b)

instance Storable a => Storable (Vertex4 a) where
   sizeOf    ~(Vertex4 x _ _ _) = 4 * sizeOf x
   alignment ~(Vertex4 x _ _ _) = alignment x
   peek                         = peek4 Vertex4
   poke ptr   (Vertex4 x y z w) = poke4 ptr x y z w

---------------------------------------------------------------------------

class TexCoordComponent a where
   texCoord1 :: a -> IO ()
   texCoord2 :: a -> a -> IO ()
   texCoord3 :: a -> a -> a -> IO ()
   texCoord4 :: a -> a -> a -> a -> IO ()
   texCoord1v :: Ptr a -> IO ()
   texCoord2v :: Ptr a -> IO ()
   texCoord3v :: Ptr a -> IO ()
   texCoord4v :: Ptr a -> IO ()

---------------------------------------------------------------------------

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

instance TexCoordComponent GLshort where
   texCoord1 = glTexCoord1s
   texCoord2 = glTexCoord2s
   texCoord3 = glTexCoord3s
   texCoord4 = glTexCoord4s
   texCoord1v = glTexCoord1sv
   texCoord2v = glTexCoord2sv
   texCoord3v = glTexCoord3sv
   texCoord4v = glTexCoord4sv

---------------------------------------------------------------------------

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

instance TexCoordComponent GLint where
   texCoord1 = glTexCoord1i
   texCoord2 = glTexCoord2i
   texCoord3 = glTexCoord3i
   texCoord4 = glTexCoord4i
   texCoord1v = glTexCoord1iv
   texCoord2v = glTexCoord2iv
   texCoord3v = glTexCoord3iv
   texCoord4v = glTexCoord4iv

---------------------------------------------------------------------------

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

instance TexCoordComponent GLfloat where
   texCoord1 = glTexCoord1f
   texCoord2 = glTexCoord2f
   texCoord3 = glTexCoord3f
   texCoord4 = glTexCoord4f

   texCoord1v = glTexCoord1fv
   texCoord2v = glTexCoord2fv
   texCoord3v = glTexCoord3fv
   texCoord4v = glTexCoord4fv

---------------------------------------------------------------------------

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

instance TexCoordComponent GLdouble where
   texCoord1 = glTexCoord1d
   texCoord2 = glTexCoord2d
   texCoord3 = glTexCoord3d
   texCoord4 = glTexCoord4d
   texCoord1v = glTexCoord1dv
   texCoord2v = glTexCoord2dv
   texCoord3v = glTexCoord3dv
   texCoord4v = glTexCoord4dv

---------------------------------------------------------------------------

class TexCoord a where
   texCoord  ::     a -> IO ()
   texCoordv :: Ptr a -> IO ()

data TexCoord1 a = TexCoord1 a
   deriving ( Eq, Ord, Show )

instance TexCoordComponent a => TexCoord (TexCoord1 a) where
   texCoord (TexCoord1 s) = texCoord1 s
   texCoordv = texCoord1v . (castPtr :: Ptr (TexCoord1 b) -> Ptr b)

instance Storable a => Storable (TexCoord1 a) where
   sizeOf    ~(TexCoord1 x) = sizeOf x
   alignment ~(TexCoord1 x) = alignment x
   peek                     = peek1 TexCoord1
   poke ptr   (TexCoord1 x) = poke1 ptr x

data TexCoord2 a = TexCoord2 a a
   deriving ( Eq, Ord, Show )

instance TexCoordComponent a => TexCoord (TexCoord2 a) where
   texCoord (TexCoord2 s t) = texCoord2 s t
   texCoordv = texCoord2v . (castPtr :: Ptr (TexCoord2 b) -> Ptr b)

instance Storable a => Storable (TexCoord2 a) where
   sizeOf    ~(TexCoord2 x _) = 2 * sizeOf x
   alignment ~(TexCoord2 x _) = alignment x
   peek                       = peek2 TexCoord2
   poke ptr   (TexCoord2 x y) = poke2 ptr x y

data TexCoord3 a = TexCoord3 a a a
   deriving ( Eq, Ord, Show )

instance TexCoordComponent a => TexCoord (TexCoord3 a) where
   texCoord (TexCoord3 s t r) = texCoord3 s t r
   texCoordv = texCoord3v . (castPtr :: Ptr (TexCoord3 b) -> Ptr b)

instance Storable a => Storable (TexCoord3 a) where
   sizeOf    ~(TexCoord3 x _ _) = 3 * sizeOf x
   alignment ~(TexCoord3 x _ _) = alignment x
   peek                         = peek3 TexCoord3
   poke ptr   (TexCoord3 x y z) = poke3 ptr x y z

data TexCoord4 a = TexCoord4 a a a a
   deriving ( Eq, Ord, Show )

instance TexCoordComponent a => TexCoord (TexCoord4 a) where
   texCoord (TexCoord4 s t r q) = texCoord4 s t r q
   texCoordv = texCoord4v . (castPtr :: Ptr (TexCoord4 b) -> Ptr b)

instance Storable a => Storable (TexCoord4 a) where
   sizeOf    ~(TexCoord4 x _ _ _) = 4 * sizeOf x
   alignment ~(TexCoord4 x _ _ _) = alignment x
   peek                           = peek4 TexCoord4
   poke ptr   (TexCoord4 x y z w) = poke4 ptr x y z w

---------------------------------------------------------------------------

class NormalComponent a where
   normal3 :: a -> a -> a -> IO ()
   normal3v :: Ptr a -> IO ()

---------------------------------------------------------------------------

foreign import CALLCONV unsafe "glNormal3b" glNormal3b ::
   GLbyte -> GLbyte -> GLbyte -> IO ()

foreign import CALLCONV unsafe "glNormal3bv" glNormal3bv ::
   Ptr GLbyte -> IO ()

instance NormalComponent GLbyte where
   normal3 = glNormal3b
   normal3v = glNormal3bv

---------------------------------------------------------------------------

foreign import CALLCONV unsafe "glNormal3s" glNormal3s ::
   GLshort -> GLshort -> GLshort -> IO ()

foreign import CALLCONV unsafe "glNormal3sv" glNormal3sv ::
   Ptr GLshort -> IO ()

instance NormalComponent GLshort where
   normal3 = glNormal3s
   normal3v = glNormal3sv

---------------------------------------------------------------------------

foreign import CALLCONV unsafe "glNormal3i" glNormal3i ::
   GLint -> GLint -> GLint -> IO ()

foreign import CALLCONV unsafe "glNormal3iv" glNormal3iv ::
   Ptr GLint -> IO ()

instance NormalComponent GLint where
   normal3 = glNormal3i
   normal3v = glNormal3iv

---------------------------------------------------------------------------

foreign import CALLCONV unsafe "glNormal3f" glNormal3f ::
   GLfloat -> GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glNormal3fv" glNormal3fv ::
   Ptr GLfloat -> IO ()

instance NormalComponent GLfloat where
   normal3 = glNormal3f
   normal3v = glNormal3fv

---------------------------------------------------------------------------

foreign import CALLCONV unsafe "glNormal3d" glNormal3d ::
   GLdouble -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glNormal3dv" glNormal3dv ::
   Ptr GLdouble -> IO ()

instance NormalComponent GLdouble where
   normal3 = glNormal3d
   normal3v = glNormal3dv

---------------------------------------------------------------------------

class Normal a where
   normal  ::     a -> IO ()
   normalv :: Ptr a -> IO ()

data Normal3 a = Normal3 a a a
   deriving ( Eq, Ord, Show )

instance NormalComponent a => Normal (Normal3 a) where
   normal (Normal3 x y z) = normal3 x y z
   normalv = normal3v . (castPtr :: Ptr (Normal3 b) -> Ptr b)

instance Storable a => Storable (Normal3 a) where
   sizeOf    ~(Normal3 x _ _) = 3 * sizeOf x
   alignment ~(Normal3 x _ _) = alignment x
   peek                       = peek3 Normal3
   poke ptr   (Normal3 x y z) = poke3 ptr x y z

---------------------------------------------------------------------------

class ColorComponent a where
   color3 :: a -> a -> a -> IO ()
   color4 :: a -> a -> a -> a -> IO ()
   color3v :: Ptr a -> IO ()
   color4v :: Ptr a -> IO ()

---------------------------------------------------------------------------

foreign import CALLCONV unsafe "glColor3b" glColor3b ::
   GLbyte -> GLbyte -> GLbyte -> IO ()

foreign import CALLCONV unsafe "glColor4b" glColor4b ::
   GLbyte -> GLbyte -> GLbyte -> GLbyte -> IO ()

foreign import CALLCONV unsafe "glColor3bv" glColor3bv ::
   Ptr GLbyte -> IO ()

foreign import CALLCONV unsafe "glColor4bv" glColor4bv ::
   Ptr GLbyte -> IO ()

instance ColorComponent GLbyte where
   color3 = glColor3b
   color4 = glColor4b
   color3v = glColor3bv
   color4v = glColor4bv

---------------------------------------------------------------------------

foreign import CALLCONV unsafe "glColor3s" glColor3s ::
   GLshort -> GLshort -> GLshort -> IO ()

foreign import CALLCONV unsafe "glColor4s" glColor4s ::
   GLshort -> GLshort -> GLshort -> GLshort -> IO ()

foreign import CALLCONV unsafe "glColor3sv" glColor3sv ::
   Ptr GLshort -> IO ()

foreign import CALLCONV unsafe "glColor4sv" glColor4sv ::
   Ptr GLshort -> IO ()

instance ColorComponent GLshort where
   color3 = glColor3s
   color4 = glColor4s
   color3v = glColor3sv
   color4v = glColor4sv

---------------------------------------------------------------------------

foreign import CALLCONV unsafe "glColor3i" glColor3i ::
   GLint -> GLint -> GLint -> IO ()

foreign import CALLCONV unsafe "glColor4i" glColor4i ::
   GLint -> GLint -> GLint -> GLint -> IO ()

foreign import CALLCONV unsafe "glColor3iv" glColor3iv ::
   Ptr GLint -> IO ()

foreign import CALLCONV unsafe "glColor4iv" glColor4iv ::
   Ptr GLint -> IO ()

instance ColorComponent GLint where
   color3 = glColor3i
   color4 = glColor4i
   color3v = glColor3iv
   color4v = glColor4iv

---------------------------------------------------------------------------

foreign import CALLCONV unsafe "glColor3f" glColor3f ::
   GLfloat -> GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glColor4f" glColor4f ::
   GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glColor3fv" glColor3fv ::
   Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glColor4fv" glColor4fv ::
   Ptr GLfloat -> IO ()

instance ColorComponent GLfloat where
   color3 = glColor3f
   color4 = glColor4f
   color3v = glColor3fv
   color4v = glColor4fv

---------------------------------------------------------------------------

foreign import CALLCONV unsafe "glColor3d" glColor3d ::
   GLdouble -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glColor4d" glColor4d ::
   GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glColor3dv" glColor3dv ::
   Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glColor4dv" glColor4dv ::
   Ptr GLdouble -> IO ()

instance ColorComponent GLdouble where
   color3 = glColor3d
   color4 = glColor4d
   color3v = glColor3dv
   color4v = glColor4dv

---------------------------------------------------------------------------

foreign import CALLCONV unsafe "glColor3ub" glColor3ub ::
   GLubyte -> GLubyte -> GLubyte -> IO ()

foreign import CALLCONV unsafe "glColor4ub" glColor4ub ::
   GLubyte -> GLubyte -> GLubyte -> GLubyte -> IO ()


foreign import CALLCONV unsafe "glColor3ubv" glColor3ubv ::
   Ptr GLubyte -> IO ()

foreign import CALLCONV unsafe "glColor4ubv" glColor4ubv ::
   Ptr GLubyte -> IO ()

instance ColorComponent GLubyte where
   color3 = glColor3ub
   color4 = glColor4ub
   color3v = glColor3ubv
   color4v = glColor4ubv

---------------------------------------------------------------------------

foreign import CALLCONV unsafe "glColor3us" glColor3us ::
   GLushort -> GLushort -> GLushort -> IO ()

foreign import CALLCONV unsafe "glColor4us" glColor4us ::
   GLushort -> GLushort -> GLushort -> GLushort -> IO ()

foreign import CALLCONV unsafe "glColor3usv" glColor3usv ::
   Ptr GLushort -> IO ()

foreign import CALLCONV unsafe "glColor4usv" glColor4usv ::
   Ptr GLushort -> IO ()

instance ColorComponent GLushort where
   color3 = glColor3us
   color4 = glColor4us
   color3v = glColor3usv
   color4v = glColor4usv

---------------------------------------------------------------------------

foreign import CALLCONV unsafe "glColor3ui" glColor3ui ::
   GLuint -> GLuint -> GLuint -> IO ()

foreign import CALLCONV unsafe "glColor4ui" glColor4ui ::
   GLuint -> GLuint -> GLuint -> GLuint -> IO ()

foreign import CALLCONV unsafe "glColor3uiv" glColor3uiv ::
   Ptr GLuint -> IO ()

foreign import CALLCONV unsafe "glColor4uiv" glColor4uiv ::
   Ptr GLuint -> IO ()

instance ColorComponent GLuint where
   color3 = glColor3ui
   color4 = glColor4ui
   color3v = glColor3uiv
   color4v = glColor4uiv

---------------------------------------------------------------------------

class Color a where
   color  ::     a -> IO ()
   colorv :: Ptr a -> IO ()

data Color3 a = Color3 a a a
   deriving ( Eq, Ord, Show )

instance ColorComponent a => Color (Color3 a) where
   color (Color3 r g b) = color3 r g b
   colorv = color3v . (castPtr :: Ptr (Color3 b) -> Ptr b)

instance Storable a => Storable (Color3 a) where
   sizeOf    ~(Color3 r _ _) = 3 * sizeOf r
   alignment ~(Color3 r _ _) = alignment r
   peek                      = peek3 Color3
   poke ptr   (Color3 r g b) = poke3 ptr r g b

data Color4 a = Color4 a a a a
   deriving ( Eq, Ord, Show )

instance ColorComponent a => Color (Color4 a) where
   color (Color4 r g b a) = color4 r g b a
   colorv = color4v . (castPtr :: Ptr (Color4 b) -> Ptr b)

instance Storable a => Storable (Color4 a) where
   sizeOf    ~(Color4 r _ _ _) = 4 * sizeOf r
   alignment ~(Color4 r _ _ _) = alignment r
   peek                        = peek4 Color4
   poke ptr   (Color4 r g b a) = poke4 ptr r g b a

---------------------------------------------------------------------------

class IndexComponent a where
   index1 :: a -> IO ()
   index1v :: Ptr a -> IO ()

---------------------------------------------------------------------------

foreign import CALLCONV unsafe "glIndexs" glIndexs ::
   GLshort -> IO ()

foreign import CALLCONV unsafe "glIndexsv" glIndexsv ::
   Ptr GLshort -> IO ()

instance IndexComponent GLshort where
   index1 = glIndexs
   index1v = glIndexsv

---------------------------------------------------------------------------

foreign import CALLCONV unsafe "glIndexi" glIndexi ::
   GLint -> IO ()

foreign import CALLCONV unsafe "glIndexiv" glIndexiv ::
   Ptr GLint -> IO ()

instance IndexComponent GLint where
   index1 = glIndexi
   index1v = glIndexiv

---------------------------------------------------------------------------

foreign import CALLCONV unsafe "glIndexf" glIndexf ::
   GLfloat -> IO ()

foreign import CALLCONV unsafe "glIndexfv" glIndexfv ::
   Ptr GLfloat -> IO ()

instance IndexComponent GLfloat where
   index1 = glIndexf
   index1v = glIndexfv

---------------------------------------------------------------------------

foreign import CALLCONV unsafe "glIndexd" glIndexd ::
   GLdouble -> IO ()

foreign import CALLCONV unsafe "glIndexdv" glIndexdv ::
   Ptr GLdouble -> IO ()

instance IndexComponent GLdouble where
   index1 = glIndexd
   index1v = glIndexdv

---------------------------------------------------------------------------

foreign import CALLCONV unsafe "glIndexub" glIndexub ::
   GLubyte -> IO ()

foreign import CALLCONV unsafe "glIndexubv" glIndexubv ::
   Ptr GLubyte -> IO ()

instance IndexComponent GLubyte where
   index1 = glIndexub
   index1v = glIndexubv

---------------------------------------------------------------------------

-- Collision with Prelude.index
class Index a where
   index  ::     a -> IO ()
   indexv :: Ptr a -> IO ()

newtype ColorIndex a = ColorIndex a
   deriving ( Eq, Ord, Show )

instance IndexComponent a => Index (ColorIndex a) where
   index (ColorIndex i) = index1 i
   indexv = index1v . (castPtr :: Ptr (ColorIndex b) -> Ptr b)

---------------------------------------------------------------------------
-- Utilities (a little bit verbose/redundant, but seems to generate better
-- code than mapM/zipWithM_)

{-# INLINE peek1 #-}
peek1 :: Storable a => (a -> b) -> Ptr c -> IO b
peek1 f ptr = do
   x <- peekElemOff (castPtr ptr) 0
   return $ f x

{-# INLINE peek2 #-}
peek2 :: Storable a => (a -> a -> b) -> Ptr c -> IO b
peek2 f ptr = do
   x <- peekElemOff (castPtr ptr) 0
   y <- peekElemOff (castPtr ptr) 1
   return $ f x y

{-# INLINE peek3 #-}
peek3 :: Storable a => (a -> a -> a -> b) -> Ptr c -> IO b
peek3 f ptr = do
   x <- peekElemOff (castPtr ptr) 0
   y <- peekElemOff (castPtr ptr) 1
   z <- peekElemOff (castPtr ptr) 2
   return $ f x y z

{-# INLINE peek4 #-}
peek4 :: Storable a => (a -> a -> a -> a -> b) -> Ptr c -> IO b
peek4 f ptr = do
   x <- peekElemOff (castPtr ptr) 0
   y <- peekElemOff (castPtr ptr) 1
   z <- peekElemOff (castPtr ptr) 2
   w <- peekElemOff (castPtr ptr) 3
   return $ f x y z w

{-# INLINE poke1 #-}
poke1 :: Storable b => Ptr a -> b -> IO ()
poke1 ptr x =
   pokeElemOff (castPtr ptr) 0 x

{-# INLINE poke2 #-}
poke2 :: Storable b => Ptr a -> b -> b -> IO ()
poke2 ptr x y = do
   pokeElemOff (castPtr ptr) 0 x
   pokeElemOff (castPtr ptr) 1 y

{-# INLINE poke3 #-}
poke3 :: Storable b => Ptr a -> b -> b -> b -> IO ()
poke3 ptr x y z = do
   pokeElemOff (castPtr ptr) 0 x
   pokeElemOff (castPtr ptr) 1 y
   pokeElemOff (castPtr ptr) 2 z

{-# INLINE poke4 #-}
poke4 :: Storable b => Ptr a -> b -> b -> b -> b -> IO ()
poke4 ptr x y z w = do
   pokeElemOff (castPtr ptr) 0 x
   pokeElemOff (castPtr ptr) 1 y
   pokeElemOff (castPtr ptr) 2 z
   pokeElemOff (castPtr ptr) 3 w
