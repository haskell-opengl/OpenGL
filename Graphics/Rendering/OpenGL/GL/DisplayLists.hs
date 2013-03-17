--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.DisplayLists
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 5.4 (Display Lists) of the OpenGL 2.1
-- specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.DisplayLists (
   -- * Defining Display Lists
   DisplayList(..), ListMode(..), defineList, defineNewList, listIndex,
   listMode, maxListNesting,

   -- * Calling Display Lists
   callList, callLists, listBase,

   -- * Deprecated Functions
   genLists, deleteLists, isList,
) where

import Foreign.Ptr
import Graphics.Rendering.OpenGL.GL.ObjectName
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.DataType
import Graphics.Rendering.OpenGL.GL.Exception
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (
 glCallList, glCallLists, glDeleteLists, glEndList, glGenLists, glIsList,
 glListBase, glNewList, gl_COMPILE, gl_COMPILE_AND_EXECUTE )
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

newtype DisplayList = DisplayList GLuint
   deriving ( Eq, Ord, Show )

instance ObjectName DisplayList where
   genObjectNames = genLists_
   deleteObjectNames = deleteLists_
   isObjectName = isList_

--------------------------------------------------------------------------------

{-# DEPRECATED genLists "use `genObjectNames' instead" #-}
genLists :: GLsizei -> IO [DisplayList]
genLists = genLists_ . fromIntegral

genLists_ :: Int -> IO [DisplayList]
genLists_ n = do
   first <- glGenLists (fromIntegral n)
   if DisplayList first == noDisplayList
      then do recordOutOfMemory
              return []
      else return [ DisplayList l | l <- [ first .. first + fromIntegral n - 1 ] ]

--------------------------------------------------------------------------------

{-# DEPRECATED deleteLists "use `deleteObjectNames' instead" #-}
deleteLists :: [DisplayList] -> IO ()
deleteLists = deleteLists_

deleteLists_ :: [DisplayList] -> IO ()
deleteLists_ = mapM_ (uncurry glDeleteLists) . combineConsecutive

combineConsecutive :: [DisplayList] -> [(GLuint, GLsizei)]
combineConsecutive []     = []
combineConsecutive (z@(DisplayList dl) :zs) = (dl, len) : combineConsecutive rest
   where (len, rest) = run (0 :: GLsizei) z zs
         run n x xs = case n + 1 of
                         m -> case xs of
                                 []                          -> (m, [])
                                 (y:ys) | x `isFollowedBy` y -> run m y ys
                                        | otherwise          -> (m, xs)
         DisplayList x `isFollowedBy` DisplayList y = x + 1 == y

--------------------------------------------------------------------------------

{-# DEPRECATED isList "use `isObjectName' instead" #-}
isList :: DisplayList -> IO Bool
isList = isList_

isList_ :: DisplayList -> IO Bool
isList_ (DisplayList dl) = fmap unmarshalGLboolean (glIsList dl)

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
defineList (DisplayList dl) mode = bracket_ (glNewList dl (marshalListMode mode)) glEndList

defineNewList :: ListMode -> IO a -> IO DisplayList
defineNewList mode action = do
   lists <- genLists 1
   if null lists
      then do recordOutOfMemory
              return noDisplayList
      else do let lst = head lists
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
callList (DisplayList dl) = glCallList dl

callLists :: GLsizei -> DataType -> Ptr a -> IO ()
callLists n = glCallLists n . marshalDataType

--------------------------------------------------------------------------------

listBase :: StateVar DisplayList
listBase =
   makeStateVar
      (getEnum1 (DisplayList . fromIntegral) GetListBase)
       (\(DisplayList dl) -> glListBase dl)
