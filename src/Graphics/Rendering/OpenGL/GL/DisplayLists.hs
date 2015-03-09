--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.DisplayLists
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 5.4 (Display Lists) of the OpenGL 2.1
-- specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.DisplayLists (
   -- * Defining Display Lists
   DisplayList(DisplayList), ListMode(..), defineList, defineNewList, listIndex,
   listMode, maxListNesting,

   -- * Calling Display Lists
   callList, callLists, listBase
) where

import Control.Monad.IO.Class
import Data.ObjectName
import Data.StateVar
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.DebugOutput
import Graphics.Rendering.OpenGL.GL.DataType
import Graphics.Rendering.OpenGL.GL.Exception
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

newtype DisplayList = DisplayList { displayListID :: GLuint }
   deriving ( Eq, Ord, Show )

instance ObjectName DisplayList where
   isObjectName = liftIO . fmap unmarshalGLboolean . glIsList . displayListID
   deleteObjectNames =
     liftIO . mapM_ (uncurry glDeleteLists) . combineConsecutive

instance CanBeLabeled DisplayList where
   objectLabel = objectNameLabel gl_DISPLAY_LIST . displayListID

combineConsecutive :: [DisplayList] -> [(GLuint, GLsizei)]
combineConsecutive [] = []
combineConsecutive (z:zs) = (displayListID z, len) : combineConsecutive rest
   where (len, rest) = run (0 :: GLsizei) z zs
         run n x xs = case n + 1 of
                         m -> case xs of
                                 []                          -> (m, [])
                                 (y:ys) | x `isFollowedBy` y -> run m y ys
                                        | otherwise          -> (m, xs)
         DisplayList x `isFollowedBy` DisplayList y = x + 1 == y

instance GeneratableObjectName DisplayList where
   genObjectNames n = liftIO $ do
      first <- glGenLists (fromIntegral n)
      if DisplayList first == noDisplayList
         then do recordOutOfMemory
                 return []
         else return [ DisplayList l
                     | l <- [ first .. first + fromIntegral n - 1 ] ]

--------------------------------------------------------------------------------

data ListMode =
     Compile
   | CompileAndExecute
   deriving ( Eq, Ord, Show )

marshalListMode :: ListMode -> GLenum
marshalListMode x = case x of
   Compile -> gl_COMPILE
   CompileAndExecute -> gl_COMPILE_AND_EXECUTE

unmarshalListMode :: GLenum -> ListMode
unmarshalListMode x
   | x == gl_COMPILE = Compile
   | x == gl_COMPILE_AND_EXECUTE = CompileAndExecute
   | otherwise = error ("unmarshalListMode: illegal value " ++ show x)

--------------------------------------------------------------------------------

defineList :: DisplayList -> ListMode -> IO a -> IO a
defineList dl mode =
   bracket_ (glNewList (displayListID dl) (marshalListMode mode)) glEndList

defineNewList :: ListMode -> IO a -> IO DisplayList
defineNewList mode action = do
   lst <- genObjectName
   _ <- defineList lst mode action
   return lst

--------------------------------------------------------------------------------

listIndex :: GettableStateVar (Maybe DisplayList)
listIndex =
   makeGettableStateVar
      (do l <- getEnum1 (DisplayList . fromIntegral) GetListIndex
          return $ if l == noDisplayList then Nothing else Just l)

noDisplayList :: DisplayList
noDisplayList = DisplayList 0

listMode :: GettableStateVar ListMode
listMode = makeGettableStateVar (getEnum1 unmarshalListMode GetListMode)

maxListNesting :: GettableStateVar GLsizei
maxListNesting = makeGettableStateVar (getSizei1 id GetMaxListNesting)

--------------------------------------------------------------------------------

callList :: DisplayList -> IO ()
callList = glCallList . displayListID

callLists :: GLsizei -> DataType -> Ptr a -> IO ()
callLists n = glCallLists n . marshalDataType

--------------------------------------------------------------------------------

listBase :: StateVar DisplayList
listBase =
   makeStateVar
      (getEnum1 (DisplayList . fromIntegral) GetListBase)
      (glListBase . displayListID)
