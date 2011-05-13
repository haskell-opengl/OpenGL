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
   transformFeedbackBufferMode,

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
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.TransformFeedback
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
   shaderType = const gl_VERTEX_SHADER

instance Shader FragmentShader where
   makeShader = FragmentShader
   shaderID = fragmentShaderID
   shaderType = const gl_FRAGMENT_SHADER

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
         withArray (map fromIntegral lengths) $ \lengthsBuf ->
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
   ShaderDeleteStatus -> gl_DELETE_STATUS
   CompileStatus -> gl_COMPILE_STATUS
   ShaderInfoLogLength -> gl_INFO_LOG_LENGTH
   ShaderSourceLength -> gl_SHADER_SOURCE_LENGTH
   ShaderType -> gl_SHADER_TYPE

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

transformFeedbackBufferMode :: Program -> GettableStateVar TransformFeedbackBufferMode
transformFeedbackBufferMode = programVar (unmarshalTransformFeedbackBufferMode . fromIntegral)
   TransformFeedbackBufferMode

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
   | TransformFeedbackBufferMode

marshalGetProgramPName :: GetProgramPName -> GLenum
marshalGetProgramPName x = case x of
   ProgramDeleteStatus -> gl_DELETE_STATUS
   LinkStatus -> gl_LINK_STATUS
   ValidateStatus -> gl_VALIDATE_STATUS
   ProgramInfoLogLength -> gl_INFO_LOG_LENGTH
   AttachedShaders -> gl_ATTACHED_SHADERS
   ActiveAttributes -> gl_ACTIVE_ATTRIBUTES
   ActiveAttributeMaxLength -> gl_ACTIVE_ATTRIBUTE_MAX_LENGTH
   ActiveUniforms -> gl_ACTIVE_UNIFORMS
   ActiveUniformMaxLength -> gl_ACTIVE_UNIFORM_MAX_LENGTH
   TransformFeedbackBufferMode -> gl_TRANSFORM_FEEDBACK_BUFFER_MODE

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
   | x == gl_FLOAT = Float'
   | x == gl_FLOAT_VEC2 = FloatVec2
   | x == gl_FLOAT_VEC3 = FloatVec3
   | x == gl_FLOAT_VEC4 = FloatVec4
   | x == gl_INT = Int'
   | x == gl_INT_VEC2 = IntVec2
   | x == gl_INT_VEC3 = IntVec3
   | x == gl_INT_VEC4 = IntVec4
   | x == gl_UNSIGNED_INT = UnsignedInt'
   | x == gl_UNSIGNED_INT_VEC2 = UnsignedIntVec2
   | x == gl_UNSIGNED_INT_VEC3 = UnsignedIntVec3
   | x == gl_UNSIGNED_INT_VEC4 = UnsignedIntVec4
   | x == gl_BOOL = Bool
   | x == gl_BOOL_VEC2 = BoolVec2
   | x == gl_BOOL_VEC3 = BoolVec3
   | x == gl_BOOL_VEC4 = BoolVec4
   | x == gl_FLOAT_MAT2 = FloatMat2
   | x == gl_FLOAT_MAT3 = FloatMat3
   | x == gl_FLOAT_MAT4 = FloatMat4
   | x == gl_FLOAT_MAT2x3 = FloatMat2x3
   | x == gl_FLOAT_MAT2x4 = FloatMat2x4
   | x == gl_FLOAT_MAT3x2 = FloatMat3x2
   | x == gl_FLOAT_MAT3x4 = FloatMat3x4
   | x == gl_FLOAT_MAT4x2 = FloatMat4x2
   | x == gl_FLOAT_MAT4x3 = FloatMat4x3
   | x == gl_SAMPLER_1D = Sampler1D
   | x == gl_SAMPLER_2D = Sampler2D
   | x == gl_SAMPLER_3D = Sampler3D
   | x == gl_SAMPLER_CUBE = SamplerCube
   | x == gl_SAMPLER_1D_SHADOW = Sampler1DShadow
   | x == gl_SAMPLER_2D_SHADOW = Sampler2DShadow
   | x == gl_SAMPLER_1D_ARRAY = Sampler1DArray
   | x == gl_SAMPLER_2D_ARRAY = Sampler2DArray
   | x == gl_SAMPLER_1D_ARRAY_SHADOW = Sampler1DArrayShadow
   | x == gl_SAMPLER_2D_ARRAY_SHADOW = Sampler2DArrayShadow
   | x == gl_SAMPLER_CUBE_SHADOW = SamplerCubeShadow
   | x == gl_SAMPLER_2D_RECT = Sampler2DRect
   | x == gl_SAMPLER_2D_RECT_SHADOW = Sampler2DRectShadow
   | x == gl_INT_SAMPLER_1D = IntSampler1D
   | x == gl_INT_SAMPLER_2D = IntSampler2D
   | x == gl_INT_SAMPLER_3D = IntSampler3D
   | x == gl_INT_SAMPLER_CUBE = IntSamplerCube
   | x == gl_INT_SAMPLER_1D_ARRAY = IntSampler1DArray
   | x == gl_INT_SAMPLER_2D_ARRAY = IntSampler2DArray
   | x == gl_UNSIGNED_INT_SAMPLER_1D = UnsignedIntSampler1D
   | x == gl_UNSIGNED_INT_SAMPLER_2D = UnsignedIntSampler2D
   | x == gl_UNSIGNED_INT_SAMPLER_3D = UnsignedIntSampler3D
   | x == gl_UNSIGNED_INT_SAMPLER_CUBE = UnsignedIntSamplerCube
   | x == gl_UNSIGNED_INT_SAMPLER_1D_ARRAY = UnsignedIntSampler1DArray
   | x == gl_UNSIGNED_INT_SAMPLER_2D_ARRAY = UnsignedIntSampler2DArray
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

instance UniformComponent GLint where
   uniform1 (UniformLocation ul) = glUniform1i ul
   uniform2 (UniformLocation ul) = glUniform2i ul
   uniform3 (UniformLocation ul) = glUniform3i ul
   uniform4 (UniformLocation ul) = glUniform4i ul

   getUniform (Program p) (UniformLocation ul) = glGetUniformiv p ul . castPtr

   uniform1v (UniformLocation ul) = glUniform1iv ul
   uniform2v (UniformLocation ul) = glUniform2iv ul
   uniform3v (UniformLocation ul) = glUniform3iv ul
   uniform4v (UniformLocation ul) = glUniform4iv ul

instance UniformComponent GLuint where
   uniform1 (UniformLocation ul) = glUniform1ui ul
   uniform2 (UniformLocation ul) = glUniform2ui ul
   uniform3 (UniformLocation ul) = glUniform3ui ul
   uniform4 (UniformLocation ul) = glUniform4ui ul

   getUniform (Program p) (UniformLocation ul) = glGetUniformuiv p ul . castPtr

   uniform1v (UniformLocation ul) = glUniform1uiv ul
   uniform2v (UniformLocation ul) = glUniform2uiv ul
   uniform3v (UniformLocation ul) = glUniform3uiv ul
   uniform4v (UniformLocation ul) = glUniform4uiv ul

instance UniformComponent GLfloat where
   uniform1 (UniformLocation ul) = glUniform1f ul
   uniform2 (UniformLocation ul) = glUniform2f ul
   uniform3 (UniformLocation ul) = glUniform3f ul
   uniform4 (UniformLocation ul) = glUniform4f ul

   getUniform (Program p) (UniformLocation ul) = glGetUniformfv p ul . castPtr

   uniform1v (UniformLocation ul) = glUniform1fv ul
   uniform2v (UniformLocation ul) = glUniform2fv ul
   uniform3v (UniformLocation ul) = glUniform3fv ul
   uniform4v (UniformLocation ul) = glUniform4fv ul

instance UniformComponent GLclampf where
   uniform1 (UniformLocation ul) x = glUniform1f ul (realToFrac x)
   uniform2 (UniformLocation ul) x y = glUniform2f ul (realToFrac x) (realToFrac y)
   uniform3 (UniformLocation ul) x y z = glUniform3f ul (realToFrac x) (realToFrac y) (realToFrac z)
   uniform4 (UniformLocation ul) x y z w = glUniform4f ul (realToFrac x) (realToFrac y) (realToFrac z) (realToFrac w)

   getUniform (Program p) (UniformLocation ul) = glGetUniformfv p ul . castPtr

   uniform1v (UniformLocation ul) n = glUniform1fv ul n . castPtr
   uniform2v (UniformLocation ul) n = glUniform2fv ul n . castPtr
   uniform3v (UniformLocation ul) n = glUniform3fv ul n . castPtr
   uniform4v (UniformLocation ul) n = glUniform4fv ul n . castPtr

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
