--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PerFragment
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 4.1 (Per-Fragment Operations) of the
-- OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PerFragment (
   -- * Scissor Test
   scissorTest, scissorBox,

   -- * Multisample Fragment Operations
   sampleAlphaToCoverage,  sampleAlphaToOne, sampleCoverage,

   -- * Depth Buffer Test
   depthTest
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLint, GLsizei, GLclampf )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapScissorTest,CapSampleAlphaToCoverage,CapSampleAlphaToOne,
             CapSampleCoverage,CapDepthTest),
   makeCapability )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Position(..), Size(..) )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.GLboolean (
   GLboolean, marshalGLboolean, unmarshalGLboolean )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetScissorBox,GetSampleCoverageValue,GetSampleCoverageInvert),
   getInteger4, getFloat1, getBoolean1 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasGetter(get), HasSetter(($=)), StateVar, makeStateVar )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"

--------------------------------------------------------------------------------

scissorTest :: StateVar Bool
scissorTest = makeCapability CapScissorTest

scissorBox :: StateVar (Position, Size)
scissorBox = makeStateVar (getInteger4 makeSB GetScissorBox)
                          (\(Position x y, Size w h) -> glScissor x y w h)
   where makeSB x y w h = (Position x y, Size (fromIntegral w) (fromIntegral h))

foreign import CALLCONV unsafe "glScissor" glScissor ::
   GLint -> GLint -> GLsizei -> GLsizei -> IO ()

--------------------------------------------------------------------------------

sampleAlphaToCoverage :: StateVar Bool
sampleAlphaToCoverage = makeCapability CapSampleAlphaToCoverage

sampleAlphaToOne :: StateVar Bool
sampleAlphaToOne = makeCapability CapSampleAlphaToOne

sampleCoverage :: StateVar (Maybe (GLclampf, Bool))
sampleCoverage = makeStateVar getSampleCoverage setSampleCoverage

getSampleCoverage :: IO (Maybe (GLclampf, Bool))
getSampleCoverage = do
   enabled <- get sampleCoverageEnabled
   if enabled
      then do value <- getFloat1 id GetSampleCoverageValue
              invert <- getBoolean1 unmarshalGLboolean GetSampleCoverageInvert
              return $ Just (value, invert)
      else return Nothing

setSampleCoverage :: Maybe (GLclampf, Bool) -> IO ()
setSampleCoverage Nothing = sampleCoverageEnabled $= False
setSampleCoverage (Just (value, invert)) = do
   sampleCoverageEnabled $= True
   glSampleCoverageARB value (marshalGLboolean invert)

EXTENSION_ENTRY("GL_ARB_multisample or OpenGL 1.3",glSampleCoverageARB,GLclampf -> GLboolean -> IO ())

sampleCoverageEnabled :: StateVar Bool
sampleCoverageEnabled = makeCapability CapSampleCoverage

--------------------------------------------------------------------------------

depthTest :: StateVar Bool
depthTest = makeCapability CapDepthTest
