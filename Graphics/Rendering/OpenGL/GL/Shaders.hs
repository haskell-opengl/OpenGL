--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Shaders
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to sections 2.15 (Vertex Shaders) and section 3.11
-- (Fragment Shaders) of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Shaders (
   -- * Shader Objects
   Shader, VertexShader, FragmentShader, shaderDeleteStatus, shaderSource,
   compileShader, compileStatus, shaderInfoLog,

   -- * Program Objects
   Program, programDeleteStatus, attachedShaders, linkProgram, linkStatus,
   programInfoLog, validateProgram, validateStatus, currentProgram,

   -- * Vertex attributes
   attribLocation, VariableType(..), activeAttribs,

   -- * Uniform variables
   UniformLocation, uniformLocation, activeUniforms, Uniform(..),
   UniformComponent,

   -- * Implementation limits related to GLSL
   maxVertexTextureImageUnits, maxTextureImageUnits,
   maxCombinedTextureImageUnits, maxTextureCoords, maxVertexUniformComponents,
   maxFragmentUniformComponents, maxVertexAttribs, maxVaryingFloats
) where

import Control.Monad
import Control.Monad.Fix
import Data.List
import Data.ObjectName
import Data.StateVar
import Data.Tensor
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

type GLStringLen = (Ptr GLchar, GLsizei)

peekGLstringLen :: GLStringLen -> IO String
peekGLstringLen (p,l) = peekCAStringLen (castPtr p, fromIntegral l)

withGLStringLen :: String -> (GLStringLen -> IO a) -> IO a
withGLStringLen s act =
   withCAStringLen s $ \(p,len) ->
      act (castPtr p, fromIntegral len)

withGLString :: String -> (Ptr GLchar -> IO a) -> IO a
withGLString s act = withCAString s $ act . castPtr

--------------------------------------------------------------------------------

newtype VertexShader = VertexShader { vertexShaderID :: GLuint }
   deriving ( Eq, Ord, Show )

newtype FragmentShader = FragmentShader { fragmentShaderID :: GLuint }
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

class (Eq s, Ord s, Show s, ObjectName s) => Shader s where
   shaderID :: s -> GLuint
   makeShader :: GLuint -> s
   shaderType :: s -> GLenum

instance Shader VertexShader where
   makeShader = VertexShader
   shaderID = vertexShaderID
   shaderType = const 0x8B31

instance Shader FragmentShader where
   makeShader = FragmentShader
   shaderID = fragmentShaderID
   shaderType = const 0x8B30

--------------------------------------------------------------------------------

instance ObjectName VertexShader where
   genObjectNames = genShaderNames
   deleteObjectNames = deleteShaderNames
   isObjectName = isShaderName

instance ObjectName FragmentShader where
   genObjectNames = genShaderNames
   deleteObjectNames = deleteShaderNames
   isObjectName = isShaderName

genShaderNames :: Shader s => Int -> IO [s]
genShaderNames n = replicateM n createShader

createShader :: Shader s => IO s
createShader = mfix (fmap makeShader . glCreateShader . shaderType)

deleteShaderNames :: Shader s => [s] -> IO ()
deleteShaderNames = mapM_ (glDeleteShader . shaderID)

isShaderName :: Shader s => s -> IO Bool
isShaderName = fmap unmarshalGLboolean . glIsShader . shaderID

--------------------------------------------------------------------------------

compileShader :: Shader s => s -> IO ()
compileShader = glCompileShader . shaderID

--------------------------------------------------------------------------------

shaderSource :: Shader s => s -> StateVar [String]
shaderSource shader =
   makeStateVar (getShaderSource shader) (setShaderSource shader)

setShaderSource :: Shader s => s -> [String] -> IO ()
setShaderSource shader srcs = do
   let len = genericLength srcs
   withMany withGLStringLen srcs $ \charBufsAndLengths -> do
      let (charBufs, lengths) = unzip charBufsAndLengths
      withArray charBufs $ \charBufsBuf ->
         withArray lengths $ \lengthsBuf ->
            glShaderSource (shaderID shader) len charBufsBuf lengthsBuf

getShaderSource :: Shader s => s -> IO [String]
getShaderSource shader = do
   src <- get (stringQuery (shaderSourceLength shader)
                           (glGetShaderSource (shaderID shader)))
   return [src]

stringQuery :: GettableStateVar GLsizei -> (GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) -> GettableStateVar String
stringQuery lengthVar getStr =
   makeGettableStateVar $ do
      len <- get lengthVar -- Note: This includes the NUL character!
      if len == 0
        then return ""
        else allocaArray (fromIntegral len) $ \buf -> do
                getStr len nullPtr buf
                peekGLstringLen (buf, len-1)

--------------------------------------------------------------------------------

shaderInfoLog :: Shader s => s -> GettableStateVar String
shaderInfoLog shader =
   stringQuery (shaderInfoLogLength shader) (glGetShaderInfoLog (shaderID shader))

--------------------------------------------------------------------------------

shaderDeleteStatus :: Shader s => s -> GettableStateVar Bool
shaderDeleteStatus = shaderVar unmarshalGLboolean ShaderDeleteStatus

compileStatus :: Shader s => s -> GettableStateVar Bool
compileStatus = shaderVar unmarshalGLboolean CompileStatus

shaderInfoLogLength :: Shader s => s -> GettableStateVar GLsizei
shaderInfoLogLength = shaderVar fromIntegral ShaderInfoLogLength

shaderSourceLength :: Shader s => s -> GettableStateVar GLsizei
shaderSourceLength = shaderVar fromIntegral ShaderSourceLength

shaderTypeEnum :: Shader s => s -> GettableStateVar GLenum
shaderTypeEnum = shaderVar fromIntegral ShaderType

--------------------------------------------------------------------------------

data GetShaderPName =
     ShaderDeleteStatus
   | CompileStatus
   | ShaderInfoLogLength
   | ShaderSourceLength
   | ShaderType

marshalGetShaderPName :: GetShaderPName -> GLenum
marshalGetShaderPName x = case x of
   ShaderDeleteStatus -> 0x8B80
   CompileStatus -> 0x8B81
   ShaderInfoLogLength -> 0x8B84
   ShaderSourceLength -> 0x8B88
   ShaderType -> 0x8B4F

shaderVar :: Shader s => (GLint -> a) -> GetShaderPName -> s -> GettableStateVar a
shaderVar f p shader =
   makeGettableStateVar $
      alloca $ \buf -> do
         glGetShaderiv (shaderID shader) (marshalGetShaderPName p) buf
         peek1 f buf

--------------------------------------------------------------------------------

newtype Program = Program { programID :: GLuint }
   deriving ( Eq, Ord, Show )

instance ObjectName Program where
   genObjectNames n = replicateM n $ fmap Program glCreateProgram
   deleteObjectNames = mapM_ (glDeleteProgram . programID)
   isObjectName = fmap unmarshalGLboolean . glIsProgram . programID

--------------------------------------------------------------------------------

attachedShaders :: Program -> StateVar ([VertexShader],[FragmentShader])
attachedShaders program =
   makeStateVar (getAttachedShaders program) (setAttachedShaders program)

getAttachedShaders :: Program -> IO ([VertexShader],[FragmentShader])
getAttachedShaders program = getAttachedShaderIDs program >>= splitShaderIDs

getAttachedShaderIDs :: Program -> IO [GLuint]
getAttachedShaderIDs program = do
   numShaders <- get (numAttachedShaders program)
   allocaArray (fromIntegral numShaders) $ \buf -> do
      glGetAttachedShaders (programID program) numShaders nullPtr buf
      peekArray (fromIntegral numShaders) buf

splitShaderIDs :: [GLuint] -> IO ([VertexShader],[FragmentShader])
splitShaderIDs ids = do
   (vs, fs) <- partitionM isVertexShaderID ids
   return (map VertexShader vs, map FragmentShader fs)

isVertexShaderID :: GLuint -> IO Bool
isVertexShaderID x = do
   t <- get (shaderTypeEnum (VertexShader x))
   return $ t == shaderType (undefined :: VertexShader)

partitionM :: (a -> IO Bool) -> [a] -> IO ([a],[a])
partitionM p = foldM select ([],[])
   where select (ts, fs) x = do
            b <- p x
            return $ if b then (x:ts, fs) else (ts, x:fs)

setAttachedShaders :: Program -> ([VertexShader],[FragmentShader]) -> IO ()
setAttachedShaders p@(Program program) (vs, fs) = do
   currentIDs <- getAttachedShaderIDs p
   let newIDs = map shaderID vs ++ map shaderID fs
   mapM_ (glAttachShader program) (newIDs \\ currentIDs)
   mapM_ (glDetachShader program) (currentIDs \\ newIDs)

--------------------------------------------------------------------------------

linkProgram :: Program -> IO ()
linkProgram (Program program) = glLinkProgram program

currentProgram :: StateVar (Maybe Program)
currentProgram =
   makeStateVar
      (do p <- getCurrentProgram
          return $ if p == noProgram then Nothing else Just p)
      ((\(Program p) -> glUseProgram p) . maybe noProgram id)

getCurrentProgram :: IO Program
getCurrentProgram = fmap Program $ getInteger1 fromIntegral GetCurrentProgram

noProgram :: Program
noProgram = Program 0

validateProgram :: Program -> IO ()
validateProgram (Program program) = glValidateProgram program

programInfoLog :: Program -> GettableStateVar String
programInfoLog p =
   stringQuery (programInfoLogLength p) (glGetProgramInfoLog (programID p))

--------------------------------------------------------------------------------

programDeleteStatus :: Program -> GettableStateVar Bool
programDeleteStatus = programVar unmarshalGLboolean ProgramDeleteStatus

linkStatus :: Program -> GettableStateVar Bool
linkStatus = programVar unmarshalGLboolean LinkStatus

validateStatus :: Program -> GettableStateVar Bool
validateStatus = programVar unmarshalGLboolean ValidateStatus

programInfoLogLength :: Program -> GettableStateVar GLsizei
programInfoLogLength = programVar fromIntegral ProgramInfoLogLength

numAttachedShaders :: Program -> GettableStateVar GLsizei
numAttachedShaders = programVar fromIntegral AttachedShaders

activeAttributes :: Program -> GettableStateVar GLuint
activeAttributes = programVar fromIntegral ActiveAttributes

activeAttributeMaxLength :: Program -> GettableStateVar GLsizei
activeAttributeMaxLength = programVar fromIntegral ActiveAttributeMaxLength

numActiveUniforms :: Program -> GettableStateVar GLuint
numActiveUniforms = programVar fromIntegral ActiveUniforms

activeUniformMaxLength :: Program -> GettableStateVar GLsizei
activeUniformMaxLength = programVar fromIntegral ActiveUniformMaxLength

--------------------------------------------------------------------------------

data GetProgramPName =
     ProgramDeleteStatus
   | LinkStatus
   | ValidateStatus
   | ProgramInfoLogLength
   | AttachedShaders
   | ActiveAttributes
   | ActiveAttributeMaxLength
   | ActiveUniforms
   | ActiveUniformMaxLength

marshalGetProgramPName :: GetProgramPName -> GLenum
marshalGetProgramPName x = case x of
   ProgramDeleteStatus -> 0x8B80
   LinkStatus -> 0x8B82
   ValidateStatus -> 0x8B83
   ProgramInfoLogLength -> 0x8B84
   AttachedShaders -> 0x8B85
   ActiveAttributes -> 0x8B89
   ActiveAttributeMaxLength -> 0x8B8A
   ActiveUniforms -> 0x8B86
   ActiveUniformMaxLength -> 0x8B87

programVar :: (GLint -> a) -> GetProgramPName -> Program -> GettableStateVar a
programVar f p program =
   makeGettableStateVar $
      alloca $ \buf -> do
         glGetProgramiv (programID program) (marshalGetProgramPName p) buf
         peek1 f buf

--------------------------------------------------------------------------------

attribLocation :: Program -> String -> StateVar AttribLocation
attribLocation program name =
   makeStateVar (getAttribLocation program name)
                (\location -> bindAttribLocation program location name)

getAttribLocation :: Program -> String -> IO AttribLocation
getAttribLocation (Program program) name =
   fmap (AttribLocation . fromIntegral) $
      withGLString name $
         glGetAttribLocation program

bindAttribLocation :: Program -> AttribLocation -> String -> IO ()
bindAttribLocation (Program program) (AttribLocation location) name =
   withGLString name $
      glBindAttribLocation program location

--------------------------------------------------------------------------------

-- Table 2.9 of the OpenGL 3.1 spec: OpenGL Shading Language type tokens
data VariableType =
     Float'
   | FloatVec2
   | FloatVec3
   | FloatVec4
   | Int'
   | IntVec2
   | IntVec3
   | IntVec4
   | UnsignedInt'
   | UnsignedIntVec2
   | UnsignedIntVec3
   | UnsignedIntVec4
   | Bool
   | BoolVec2
   | BoolVec3
   | BoolVec4
   | FloatMat2
   | FloatMat3
   | FloatMat4
   | FloatMat2x3
   | FloatMat2x4
   | FloatMat3x2
   | FloatMat3x4
   | FloatMat4x2
   | FloatMat4x3
   | Sampler1D
   | Sampler2D
   | Sampler3D
   | SamplerCube
   | Sampler1DShadow
   | Sampler2DShadow
   | Sampler1DArray
   | Sampler2DArray
   | Sampler1DArrayShadow
   | Sampler2DArrayShadow
   | SamplerCubeShadow
   | Sampler2DRect
   | Sampler2DRectShadow
   | IntSampler1D
   | IntSampler2D
   | IntSampler3D
   | IntSamplerCube
   | IntSampler1DArray
   | IntSampler2DArray
   | UnsignedIntSampler1D
   | UnsignedIntSampler2D
   | UnsignedIntSampler3D
   | UnsignedIntSamplerCube
   | UnsignedIntSampler1DArray
   | UnsignedIntSampler2DArray
   deriving ( Eq, Ord, Show )

unmarshalVariableType :: GLenum -> VariableType
unmarshalVariableType x
   | x == 0x1406 = Float'
   | x == 0x8B50 = FloatVec2
   | x == 0x8B51 = FloatVec3
   | x == 0x8B52 = FloatVec4
   | x == 0x1404 = Int'
   | x == 0x8B53 = IntVec2
   | x == 0x8B54 = IntVec3
   | x == 0x8B55 = IntVec4
   | x == 0x1405 = UnsignedInt'
   | x == 0x8DC6 = UnsignedIntVec2
   | x == 0x8DC7 = UnsignedIntVec3
   | x == 0x8DC8 = UnsignedIntVec4
   | x == 0x8B56 = Bool
   | x == 0x8B57 = BoolVec2
   | x == 0x8B58 = BoolVec3
   | x == 0x8B59 = BoolVec4
   | x == 0x8B5A = FloatMat2
   | x == 0x8B5B = FloatMat3
   | x == 0x8B5C = FloatMat4
   | x == 0x8B65 = FloatMat2x3
   | x == 0x8B66 = FloatMat2x4
   | x == 0x8B67 = FloatMat3x2
   | x == 0x8B68 = FloatMat3x4
   | x == 0x8B69 = FloatMat4x2
   | x == 0x8B6A = FloatMat4x3
   | x == 0x8B5D = Sampler1D
   | x == 0x8B5E = Sampler2D
   | x == 0x8B5F = Sampler3D
   | x == 0x8B60 = SamplerCube
   | x == 0x8B61 = Sampler1DShadow
   | x == 0x8B62 = Sampler2DShadow
   | x == 0x8DC0 = Sampler1DArray
   | x == 0x8DC1 = Sampler2DArray
   | x == 0x8DC3 = Sampler1DArrayShadow
   | x == 0x8DC4 = Sampler2DArrayShadow
   | x == 0x8DC5 = SamplerCubeShadow
   | x == 0x8B63 = Sampler2DRect
   | x == 0x8B64 = Sampler2DRectShadow
   | x == 0x8DC9 = IntSampler1D
   | x == 0x8DCA = IntSampler2D
   | x == 0x8DCB = IntSampler3D
   | x == 0x8DCC = IntSamplerCube
   | x == 0x8DCE = IntSampler1DArray
   | x == 0x8DCF = IntSampler2DArray
   | x == 0x8DD1 = UnsignedIntSampler1D
   | x == 0x8DD2 = UnsignedIntSampler2D
   | x == 0x8DD3 = UnsignedIntSampler3D
   | x == 0x8DD4 = UnsignedIntSamplerCube
   | x == 0x8DD6 = UnsignedIntSampler1DArray
   | x == 0x8DD7 = UnsignedIntSampler2DArray
   | otherwise = error ("unmarshalVariableType: illegal value " ++ show x)

--------------------------------------------------------------------------------

activeVars :: (Program -> GettableStateVar GLuint)
           -> (Program -> GettableStateVar GLsizei)
           -> (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLchar -> IO ())
           -> Program -> GettableStateVar [(GLint,VariableType,String)]
activeVars numVars maxLength getter p@(Program program) =
   makeGettableStateVar $ do
      numActiveVars <- get (numVars p)
      maxLen <- get (maxLength p)
      allocaArray (fromIntegral maxLen) $ \nameBuf ->
         alloca $ \nameLengthBuf ->
            alloca $ \sizeBuf ->
               alloca $ \typeBuf ->
                  flip mapM [0 .. numActiveVars - 1] $ \i -> do
                    getter program i maxLen nameLengthBuf sizeBuf typeBuf nameBuf
                    l <- peek nameLengthBuf
                    s <- peek sizeBuf
                    t <- peek typeBuf
                    n <- peekGLstringLen (nameBuf, l)
                    return (s, unmarshalVariableType t, n)

activeAttribs :: Program -> GettableStateVar [(GLint,VariableType,String)]
activeAttribs = activeVars activeAttributes activeAttributeMaxLength glGetActiveAttrib

--------------------------------------------------------------------------------

newtype UniformLocation = UniformLocation GLint
   deriving ( Eq, Ord, Show )

uniformLocation :: Program -> String -> GettableStateVar UniformLocation
uniformLocation (Program program) name =
   makeGettableStateVar $
      fmap UniformLocation $
         withGLString name $
            glGetUniformLocation program

--------------------------------------------------------------------------------

activeUniforms :: Program -> GettableStateVar [(GLint,VariableType,String)]
activeUniforms = activeVars numActiveUniforms activeUniformMaxLength glGetActiveUniform

--------------------------------------------------------------------------------

class Storable a => UniformComponent a where
   uniform1 :: UniformLocation -> a -> IO ()
   uniform2 :: UniformLocation -> a -> a -> IO ()
   uniform3 :: UniformLocation -> a -> a -> a -> IO ()
   uniform4 :: UniformLocation -> a -> a -> a -> a -> IO ()

   getUniform :: Storable (b a) => Program -> UniformLocation -> Ptr (b a) -> IO ()

   uniform1v :: UniformLocation -> GLsizei -> Ptr a -> IO ()
   uniform2v :: UniformLocation -> GLsizei -> Ptr a -> IO ()
   uniform3v :: UniformLocation -> GLsizei -> Ptr a -> IO ()
   uniform4v :: UniformLocation -> GLsizei -> Ptr a -> IO ()

-- GLint instance
instance UniformComponent CInt where
   uniform1 (UniformLocation ul) = glUniform1i ul
   uniform2 (UniformLocation ul) = glUniform2i ul
   uniform3 (UniformLocation ul) = glUniform3i ul
   uniform4 (UniformLocation ul) = glUniform4i ul

   getUniform (Program p) (UniformLocation ul) = glGetUniformiv p ul . castPtr

   uniform1v (UniformLocation ul) = glUniform1iv ul
   uniform2v (UniformLocation ul) = glUniform2iv ul
   uniform3v (UniformLocation ul) = glUniform3iv ul
   uniform4v (UniformLocation ul) = glUniform4iv ul

-- GLuint instance
instance UniformComponent CUInt where
   uniform1 (UniformLocation ul) = glUniform1ui ul
   uniform2 (UniformLocation ul) = glUniform2ui ul
   uniform3 (UniformLocation ul) = glUniform3ui ul
   uniform4 (UniformLocation ul) = glUniform4ui ul

   getUniform (Program p) (UniformLocation ul) = glGetUniformuiv p ul . castPtr

   uniform1v (UniformLocation ul) = glUniform1uiv ul
   uniform2v (UniformLocation ul) = glUniform2uiv ul
   uniform3v (UniformLocation ul) = glUniform3uiv ul
   uniform4v (UniformLocation ul) = glUniform4uiv ul

-- GLfloat instance
instance UniformComponent CFloat where
   uniform1 (UniformLocation ul) = glUniform1f ul
   uniform2 (UniformLocation ul) = glUniform2f ul
   uniform3 (UniformLocation ul) = glUniform3f ul
   uniform4 (UniformLocation ul) = glUniform4f ul

   getUniform (Program p) (UniformLocation ul) = glGetUniformfv p ul . castPtr

   uniform1v (UniformLocation ul) = glUniform1fv ul
   uniform2v (UniformLocation ul) = glUniform2fv ul
   uniform3v (UniformLocation ul) = glUniform3fv ul
   uniform4v (UniformLocation ul) = glUniform4fv ul

--------------------------------------------------------------------------------

class Uniform a where
   uniform :: UniformLocation -> StateVar a
   uniformv :: UniformLocation -> GLsizei -> Ptr a -> IO ()

maxComponentSize :: Int
maxComponentSize = sizeOf (undefined :: GLint) `max` sizeOf (undefined :: GLfloat)

maxNumComponents :: Int
maxNumComponents = 16

maxUniformBufferSize :: Int
maxUniformBufferSize = maxComponentSize * maxNumComponents

makeUniformVar :: (UniformComponent a, Storable (b a))
               => (UniformLocation -> b a -> IO ())
               -> UniformLocation -> StateVar (b a)
makeUniformVar setter location = makeStateVar getter (setter location)
   where getter = do
            program <- getCurrentProgram
            allocaBytes maxUniformBufferSize  $ \buf -> do
            getUniform program location buf
            peek buf

instance UniformComponent a => Uniform (Vertex2 a) where
   uniform = makeUniformVar $ \location (Vertex2 x y) -> uniform2 location x y
   uniformv location count = uniform2v location count . (castPtr :: Ptr (Vertex2 b) -> Ptr b)

instance UniformComponent a => Uniform (Vertex3 a) where
   uniform = makeUniformVar $ \location (Vertex3 x y z) -> uniform3 location x y z
   uniformv location count = uniform3v location count . (castPtr :: Ptr (Vertex3 b) -> Ptr b)

instance UniformComponent a => Uniform (Vertex4 a) where
   uniform = makeUniformVar $ \location (Vertex4 x y z w) -> uniform4 location x y z w
   uniformv location count = uniform4v location count . (castPtr :: Ptr (Vertex4 b) -> Ptr b)

instance UniformComponent a => Uniform (TexCoord1 a) where
   uniform = makeUniformVar $ \location (TexCoord1 s) -> uniform1 location s
   uniformv location count = uniform1v location count . (castPtr :: Ptr (TexCoord1 b) -> Ptr b)

instance UniformComponent a => Uniform (TexCoord2 a) where
   uniform = makeUniformVar $ \location (TexCoord2 s t) -> uniform2 location s t
   uniformv location count = uniform2v location count . (castPtr :: Ptr (TexCoord2 b) -> Ptr b)

instance UniformComponent a => Uniform (TexCoord3 a) where
   uniform = makeUniformVar $ \location (TexCoord3 s t r) -> uniform3 location s t  r
   uniformv location count = uniform3v location count . (castPtr :: Ptr (TexCoord3 b) -> Ptr b)

instance UniformComponent a => Uniform (TexCoord4 a) where
   uniform = makeUniformVar $ \location (TexCoord4 s t r q) -> uniform4 location s t  r q
   uniformv location count = uniform4v location count . (castPtr :: Ptr (TexCoord4 b) -> Ptr b)

instance UniformComponent a => Uniform (Normal3 a) where
   uniform = makeUniformVar $ \location (Normal3 x y z) -> uniform3 location x y z
   uniformv location count = uniform3v location count . (castPtr :: Ptr (Normal3 b) -> Ptr b)

instance UniformComponent a => Uniform (FogCoord1 a) where
   uniform = makeUniformVar $ \location (FogCoord1 c) -> uniform1 location c
   uniformv location count = uniform1v location count . (castPtr :: Ptr (FogCoord1 b) -> Ptr b)

instance UniformComponent a => Uniform (Color3 a) where
   uniform = makeUniformVar $ \location (Color3 r g b) -> uniform3 location r g b
   uniformv location count = uniform3v location count . (castPtr :: Ptr (Color3 b) -> Ptr b)

instance UniformComponent a => Uniform (Color4 a) where
   uniform = makeUniformVar $ \location (Color4 r g b a) -> uniform4 location r g b a
   uniformv location count = uniform4v location count . (castPtr :: Ptr (Color4 b) -> Ptr b)

instance UniformComponent a => Uniform (Index1 a) where
   uniform = makeUniformVar $ \location (Index1 i) -> uniform1 location i
   uniformv location count = uniform1v location count . (castPtr :: Ptr (Index1 b) -> Ptr b)

--------------------------------------------------------------------------------

-- | Contains the number of hardware units that can be used to access texture
-- maps from the vertex processor. The minimum legal value is 0.

maxVertexTextureImageUnits :: GettableStateVar GLsizei
maxVertexTextureImageUnits = getLimit GetMaxVertexTextureImageUnits

-- | Contains the total number of hardware units that can be used to access
-- texture maps from the fragment processor. The minimum legal value is 2.

maxTextureImageUnits :: GettableStateVar GLsizei
maxTextureImageUnits = getLimit GetMaxTextureImageUnits

-- | Contains the total number of hardware units that can be used to access
-- texture maps from the vertex processor and the fragment processor combined.
-- Note: If the vertex shader and the fragment processing stage access the same
-- texture image unit, then that counts as using two texture image units. The
-- minimum legal value is 2.

maxCombinedTextureImageUnits :: GettableStateVar GLsizei
maxCombinedTextureImageUnits = getLimit GetMaxCombinedTextureImageUnits

-- | Contains the number of texture coordinate sets that are available. The
-- minimum legal value is 2.

maxTextureCoords :: GettableStateVar GLsizei
maxTextureCoords = getLimit GetMaxTextureCoords

-- | Contains the number of individual components (i.e., floating-point, integer
-- or boolean values) that are available for vertex shader uniform variables.
-- The minimum legal value is 512.
maxVertexUniformComponents :: GettableStateVar GLsizei
maxVertexUniformComponents = getLimit GetMaxVertexUniformComponents

-- | Contains the number of individual components (i.e., floating-point, integer
-- or boolean values) that are available for fragment shader uniform variables.
-- The minimum legal value is 64.

maxFragmentUniformComponents :: GettableStateVar GLsizei
maxFragmentUniformComponents = getLimit GetMaxFragmentUniformComponents

-- | Contains the number of active vertex attributes that are available. The
-- minimum legal value is 16.

maxVertexAttribs :: GettableStateVar GLsizei
maxVertexAttribs = getLimit GetMaxVertexAttribs

-- | Contains the number of individual floating-point values available for
-- varying variables. The minimum legal value is 32.

maxVaryingFloats :: GettableStateVar GLsizei
maxVaryingFloats = getLimit GetMaxVaryingFloats

getLimit :: GetPName -> GettableStateVar GLsizei
getLimit = makeGettableStateVar . getSizei1 id
