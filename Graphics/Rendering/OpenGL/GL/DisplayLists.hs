--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.DisplayLists
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 5.4 (Display Lists) of the OpenGL 1.4
-- specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.DisplayLists (
   -- * Resource Management
   DisplayList, genLists, deleteLists, isList,

   -- * Defining Display Lists
   ListMode(..), defineList, defineNewList, listIndex, listMode, maxListNesting,

   -- * Calling Display Lists
   callList, callLists, listBase
) where

import Control.Monad ( liftM )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLuint, GLsizei, GLenum )
import Graphics.Rendering.OpenGL.GL.DataType ( marshalDataType )
import Graphics.Rendering.OpenGL.GL.VertexArrays ( DataType )
import Graphics.Rendering.OpenGL.GL.Exception ( finally )
import Graphics.Rendering.OpenGL.GL.GLboolean ( GLboolean, unmarshalGLboolean )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetListIndex,GetListMode,GetMaxListNesting,GetListBase),
   getEnum1, getSizei1 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )

--------------------------------------------------------------------------------

newtype DisplayList = DisplayList GLuint
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

genLists :: GLsizei -> IO [DisplayList]
genLists n = do
   first <- glGenLists n
   return $
      if first == 0
         then []
         else [ DisplayList l | l <- [ first .. first + fromIntegral n - 1 ] ]

foreign import CALLCONV unsafe "glGenLists" glGenLists :: GLsizei -> IO GLuint

--------------------------------------------------------------------------------

deleteLists :: [DisplayList] -> IO ()
deleteLists = mapM_ (uncurry glDeleteLists) . combineConsecutive

foreign import CALLCONV unsafe "glDeleteLists" glDeleteLists ::
   DisplayList -> GLsizei -> IO ()

combineConsecutive :: [DisplayList] -> [(DisplayList, GLsizei)]
combineConsecutive []     = []
combineConsecutive (z:zs) = (z, len) : combineConsecutive rest
   where (len, rest) = run 0 z zs
         run n x xs = case n + 1 of
                         m -> case xs of
                                 []                          -> (m, [])
                                 (y:ys) | x `isFollowedBy` y -> run m y ys
                                        | otherwise          -> (m, xs)
         DisplayList x `isFollowedBy` DisplayList y = x + 1 == y

--------------------------------------------------------------------------------

isList :: DisplayList -> IO Bool
isList = liftM unmarshalGLboolean . glIsList

foreign import CALLCONV unsafe "glIsList" glIsList ::
   DisplayList -> IO GLboolean

--------------------------------------------------------------------------------

data ListMode =
     Compile
   | CompileAndExecute
   deriving ( Eq, Ord, Show )

marshalListMode :: ListMode -> GLenum
marshalListMode x = case x of
   Compile -> 0x1300
   CompileAndExecute -> 0x1301

unmarshalListMode :: GLenum -> ListMode
unmarshalListMode x
   | x == 0x1300 = Compile
   | x == 0x1301 = CompileAndExecute
   | otherwise = error ("unmarshalListMode: illegal value " ++ show x)

--------------------------------------------------------------------------------

defineList :: DisplayList -> ListMode -> IO a -> IO a
defineList lst mode action =
   (do glNewList lst (marshalListMode mode) ; action) `finally` glEndList

foreign import CALLCONV unsafe "glNewList" glNewList ::
   DisplayList -> GLenum -> IO ()

foreign import CALLCONV unsafe "glEndList" glEndList :: IO ()

defineNewList :: ListMode -> IO a -> IO DisplayList
defineNewList mode action = do
   lists <- genLists 1
   if null lists
      then return $ DisplayList 0
      else do let lst = head lists
              defineList lst mode action
              return lst

--------------------------------------------------------------------------------

listIndex :: GettableStateVar DisplayList
listIndex = makeGettableStateVar (getEnum1 DisplayList GetListIndex)

listMode :: GettableStateVar ListMode
listMode = makeGettableStateVar (getEnum1 unmarshalListMode GetListMode)

maxListNesting :: GettableStateVar GLsizei
maxListNesting = makeGettableStateVar (getSizei1 id GetMaxListNesting)

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glCallList" callList :: DisplayList -> IO ()

callLists :: GLsizei -> DataType -> Ptr a -> IO ()
callLists n = glCallLists n . marshalDataType

foreign import CALLCONV unsafe "glCallLists" glCallLists ::
   GLsizei -> GLenum -> Ptr a -> IO ()

--------------------------------------------------------------------------------

listBase :: StateVar DisplayList
listBase = makeStateVar (getEnum1 DisplayList GetListBase) glListBase

foreign import CALLCONV unsafe "glListBase" glListBase :: DisplayList -> IO ()
