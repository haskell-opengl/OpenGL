--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Shaders
-- Copyright   :  (c) Sven Panne 2002-2006
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
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

   -- * Implementation limits related to GLSL
   maxVertexTextureImageUnits, maxTextureImageUnits,
   maxCombinedTextureImageUnits, maxTextureCoords, maxDrawBuffers,
   maxVertexUniformComponents, maxFragmentUniformComponents, maxVertexAttribs,
   maxVaryingFloats
) where

import Control.Monad ( replicateM, mapM_, foldM )
import Control.Monad.Fix ( MonadFix(..) )
import Data.List ( genericLength, (\\) )
import Foreign.C.String ( peekCAStringLen, withCAStringLen )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( allocaArray, withArray, peekArray )
import Foreign.Marshal.Utils ( withMany )
import Foreign.Ptr ( Ptr, castPtr, nullPtr )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLboolean, GLchar, GLint, GLuint, GLenum, GLsizei )
import Graphics.Rendering.OpenGL.GL.BufferObjects ( ObjectName(..) )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.GLboolean ( unmarshalGLboolean )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek1 )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetMaxCombinedTextureImageUnits,GetMaxDrawBuffers,
            GetMaxFragmentUniformComponents,GetMaxTextureCoords,
            GetMaxTextureImageUnits,GetMaxVaryingFloats,GetMaxVertexAttribs,
            GetMaxVertexTextureImageUnits,GetMaxVertexUniformComponents,
            GetCurrentProgram),
   getInteger1, getSizei1 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasGetter(get), GettableStateVar, makeGettableStateVar, StateVar,
   makeStateVar )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"

--------------------------------------------------------------------------------

type GLStringLen = (Ptr GLchar, GLsizei)

peekGLstringLen :: GLStringLen -> IO String
peekGLstringLen (p,l) = peekCAStringLen (castPtr p, fromIntegral l)

withGLStringLen :: String -> (GLStringLen -> IO a) -> IO a
withGLStringLen s act =
   withCAStringLen s $ \(p,len) ->
      act (castPtr p, fromIntegral len)

--------------------------------------------------------------------------------

newtype VertexShader = VertexShader { vertexShaderID :: GLuint }
   deriving ( Eq, Ord, Show )

newtype FragmentShader = FragmentShader { fragmentShaderID :: GLuint }
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

class Shader s where
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

EXTENSION_ENTRY("OpenGL 2.0",glCreateShader,GLenum -> IO GLuint)
EXTENSION_ENTRY("OpenGL 2.0",glDeleteShader,GLuint -> IO ())
EXTENSION_ENTRY("OpenGL 2.0",glIsShader,GLuint -> IO GLboolean)

--------------------------------------------------------------------------------

compileShader :: Shader s => s -> IO ()
compileShader = glCompileShader . shaderID

EXTENSION_ENTRY("OpenGL 2.0",glCompileShader,GLuint -> IO ())

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

EXTENSION_ENTRY("OpenGL 2.0",glShaderSource,GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLint -> IO ())

getShaderSource :: Shader s => s -> IO [String]
getShaderSource shader = do
   src <- get (stringQuery (shaderSourceLength shader)
                           (glGetShaderSource (shaderID shader)))
   return [src]

EXTENSION_ENTRY("OpenGL 2.0",glGetShaderSource,GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())

stringQuery :: GettableStateVar GLsizei -> (GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) -> GettableStateVar String
stringQuery lengthVar getStr =
   makeGettableStateVar $ do
      len <- get lengthVar
      allocaArray (fromIntegral len) $ \buf -> do
         getStr len nullPtr buf
         peekGLstringLen (buf, len)

--------------------------------------------------------------------------------

shaderInfoLog :: Shader s => s -> GettableStateVar String
shaderInfoLog shader =
   stringQuery (shaderInfoLogLength shader) (glGetShaderInfoLog (shaderID shader))

EXTENSION_ENTRY("OpenGL 2.0",glGetShaderInfoLog,GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())

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

EXTENSION_ENTRY("OpenGL 2.0",glGetShaderiv,GLuint -> GLenum -> Ptr GLint -> IO ())

--------------------------------------------------------------------------------

newtype Program = Program { programID :: GLuint }
   deriving ( Eq, Ord, Show )

instance ObjectName Program where
   genObjectNames n = replicateM n $ fmap Program glCreateProgram
   deleteObjectNames = mapM_ (glDeleteProgram . programID)
   isObjectName = fmap unmarshalGLboolean . glIsProgram . programID

EXTENSION_ENTRY("OpenGL 2.0",glCreateProgram,IO GLuint)
EXTENSION_ENTRY("OpenGL 2.0",glDeleteProgram,GLuint -> IO ())
EXTENSION_ENTRY("OpenGL 2.0",glIsProgram,GLuint -> IO GLboolean)

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

EXTENSION_ENTRY("OpenGL 2.0",glGetAttachedShaders,GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLuint -> IO ())

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
setAttachedShaders program (vs, fs) = do
   currentIDs <- getAttachedShaderIDs program
   let newIDs = map shaderID vs ++ map shaderID fs
   mapM_ (glAttachShader program) (newIDs \\ currentIDs)
   mapM_ (glDetachShader program) (currentIDs \\ newIDs)

EXTENSION_ENTRY("OpenGL 2.0",glAttachShader,Program -> GLuint -> IO ())
EXTENSION_ENTRY("OpenGL 2.0",glDetachShader,Program -> GLuint -> IO ())

--------------------------------------------------------------------------------

linkProgram :: Program -> IO ()
linkProgram = glLinkProgram

EXTENSION_ENTRY("OpenGL 2.0",glLinkProgram,Program -> IO ())

currentProgram :: StateVar (Maybe Program)
currentProgram =
   makeStateVar
      (do p <- getCurrentProgram
          return $ if p == noProgram then Nothing else Just p)
      (glUseProgram . maybe noProgram id)

getCurrentProgram :: IO Program
getCurrentProgram = fmap Program $ getInteger1 fromIntegral GetCurrentProgram

noProgram :: Program
noProgram = Program 0

EXTENSION_ENTRY("OpenGL 2.0",glUseProgram,Program -> IO ())

validateProgram :: Program -> IO ()
validateProgram = glValidateProgram

EXTENSION_ENTRY("OpenGL 2.0",glValidateProgram,Program -> IO ())

programInfoLog :: Program -> GettableStateVar String
programInfoLog p =
   stringQuery (programInfoLogLength p) (glGetProgramInfoLog (programID p))

EXTENSION_ENTRY("OpenGL 2.0",glGetProgramInfoLog,GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())

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

EXTENSION_ENTRY("OpenGL 2.0",glGetProgramiv,GLuint -> GLenum -> Ptr GLint -> IO ())

--------------------------------------------------------------------------------

-- | Contains the number of hardware units that can be used to access texture
-- maps from the vertex processor. The minimum legal value is 0.

maxVertexTextureImageUnits :: GettableStateVar GLint
maxVertexTextureImageUnits = getLimit GetMaxVertexTextureImageUnits

-- | Contains the total number of hardware units that can be used to access
-- texture maps from the fragment processor. The minimum legal value is 2.

maxTextureImageUnits :: GettableStateVar GLint
maxTextureImageUnits = getLimit GetMaxTextureImageUnits

-- | Contains the total number of hardware units that can be used to access
-- texture maps from the vertex processor and the fragment processor combined.
-- Note: If the vertex shader and the fragment processing stage access the same
-- texture image unit, then that counts as using two texture image units. The
-- minimum legal value is 2.

maxCombinedTextureImageUnits :: GettableStateVar GLint
maxCombinedTextureImageUnits = getLimit GetMaxCombinedTextureImageUnits

-- | Contains the number of texture coordinate sets that are available. The
-- minimum legal value is 2.

maxTextureCoords :: GettableStateVar GLint
maxTextureCoords = getLimit GetMaxTextureCoords

-- | Contains the maximum number of buffers that can activated via 'drawBuffers'
-- or which can be simultaneously written into from within a fragment shader
-- using the special output variable array @gl_FragData@. This constant
-- effectively defines the size of the @gl_FragData@ array. The minimum legal
-- value is 1.

maxDrawBuffers :: GettableStateVar GLint
maxDrawBuffers = getLimit GetMaxDrawBuffers

-- | Contains the number of individual components (i.e., floating-point, integer
-- or boolean values) that are available for vertex shader uniform variables.
-- The minimum legal value is 512.
maxVertexUniformComponents :: GettableStateVar GLint
maxVertexUniformComponents = getLimit GetMaxVertexUniformComponents

-- | Contains the number of individual components (i.e., floating-point, integer
-- or boolean values) that are available for fragment shader uniform variables.
-- The minimum legal value is 64.

maxFragmentUniformComponents :: GettableStateVar GLint
maxFragmentUniformComponents = getLimit GetMaxFragmentUniformComponents

-- | Contains the number of active vertex attributes that are available. The
-- minimum legal value is 16.

maxVertexAttribs :: GettableStateVar GLint
maxVertexAttribs = getLimit GetMaxVertexAttribs

-- | Contains the number of individual floating-point values available for
-- varying variables. The minimum legal value is 32.

maxVaryingFloats :: GettableStateVar GLint
maxVaryingFloats = getLimit GetMaxVaryingFloats

getLimit :: GetPName -> GettableStateVar GLsizei
getLimit = makeGettableStateVar . getSizei1 id
