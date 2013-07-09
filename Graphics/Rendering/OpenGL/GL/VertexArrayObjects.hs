-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Rendering.OpenGL.GL.VertexArrayObjects
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <sven.panne@aedion.de>
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.VertexArrayObjects (
   VertexArrayObject,
   bindVertexArrayObject
) where

import Foreign.Marshal.Array

import Graphics.Rendering.OpenGL.GL.ObjectName
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.Raw.Core31

-----------------------------------------------------------------------------

newtype VertexArrayObject = VertexArrayObject { vertexArrayID :: GLuint }
   deriving( Eq, Ord, Show )

instance ObjectName VertexArrayObject where
   genObjectNames n = allocaArray n $ \buf -> do
      glGenVertexArrays (fromIntegral n) buf
      fmap (map VertexArrayObject) $ peekArray n buf
   deleteObjectNames bufferObjects =
      withArrayLen (map vertexArrayID bufferObjects) $
         glDeleteVertexArrays . fromIntegral
   isObjectName =  fmap unmarshalGLboolean . glIsVertexArray . vertexArrayID

bindVertexArrayObject :: StateVar (Maybe VertexArrayObject)
bindVertexArrayObject = makeStateVar getVAO bindVAO

getVAO :: IO (Maybe VertexArrayObject)
getVAO = do
   vao <- getInteger1 (VertexArrayObject . fromIntegral) GetVertexArrayBinding
   return $ if vao == noVAO then Nothing else Just vao

bindVAO :: Maybe VertexArrayObject -> IO ()
bindVAO = glBindVertexArray . vertexArrayID . maybe noVAO id

noVAO :: VertexArrayObject
noVAO = VertexArrayObject 0
