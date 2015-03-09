-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Shaders.ShaderBinaries
-- Copyright   :  (c) Sven Panne 2006-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 7.2 (Shader Binaries) of the OpenGL 4.4
-- spec.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Shaders.ShaderBinaries (
   ShaderBinaryFormat(..), shaderBinaryFormats,
   ShaderBinary(..), shaderBinary,
) where

import Data.StateVar
import Foreign.Marshal.Array
import Graphics.Rendering.OpenGL.GL.ByteString
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.Shaders.Shader
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

newtype ShaderBinaryFormat = ShaderBinaryFormat GLenum
   deriving ( Eq, Ord, Show )

shaderBinaryFormats :: GettableStateVar [ShaderBinaryFormat]
shaderBinaryFormats =
   makeGettableStateVar $ do
      n <- getInteger1 fromIntegral GetNumShaderBinaryFormats
      getEnumN ShaderBinaryFormat GetShaderBinaryFormats n

data ShaderBinary = ShaderBinary ShaderBinaryFormat ByteString
   deriving ( Eq, Ord, Show )

shaderBinary :: [Shader] -> SettableStateVar ShaderBinary
shaderBinary shaders =
   makeSettableStateVar $ \(ShaderBinary (ShaderBinaryFormat format) bs) ->
      withArrayLen (map shaderID shaders) $ \numShaders shadersBuf ->
         withByteString bs $
            glShaderBinary (fromIntegral numShaders) shadersBuf format
