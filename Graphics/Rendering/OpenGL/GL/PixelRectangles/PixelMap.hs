--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelRectangles.PixelMap
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to a part of section 3.6.1 (Pixel Storage Modes) of
-- the OpenGL 1.5 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PixelRectangles.PixelMap (
   PixelMapTarget(..), PixelMapComponent, PixelMap(..), GLpixelmap,
   maxPixelMapTable, pixelMap, pixelMapIToRGBA, pixelMapRGBAToRGBA,
) where

import Data.List ( zipWith4 )
import Data.Word
import Foreign.ForeignPtr ( ForeignPtr, mallocForeignPtrArray, withForeignPtr )
import Foreign.Marshal.Array ( allocaArray, peekArray, pokeArray, withArrayLen )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable(..) )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLushort, GLuint, GLsizei, GLfloat )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetMaxPixelMapTable,GetPixelMapIToISize,GetPixelMapSToSSize,
            GetPixelMapIToRSize,GetPixelMapIToGSize,GetPixelMapIToBSize,
            GetPixelMapIToASize,GetPixelMapRToRSize,GetPixelMapGToGSize,
            GetPixelMapBToBSize,GetPixelMapAToASize),
   getInteger1, getSizei1 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasSetter(($=)), HasGetter(get), GettableStateVar, makeGettableStateVar,
   StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Color4(..) )

--------------------------------------------------------------------------------

#include "HsOpenGLTypes.h"

--------------------------------------------------------------------------------

data PixelMapTarget =
     IToI
   | SToS
   | IToR
   | IToG
   | IToB
   | IToA
   | RToR
   | GToG
   | BToB
   | AToA

marshalPixelMapTarget :: PixelMapTarget -> GLenum
marshalPixelMapTarget x = case x of
   IToI -> 0xc70
   SToS -> 0xc71
   IToR -> 0xc72
   IToG -> 0xc73
   IToB -> 0xc74
   IToA -> 0xc75
   RToR -> 0xc76
   GToG -> 0xc77
   BToB -> 0xc78
   AToA -> 0xc79

pixelMapTargetToGetPName :: PixelMapTarget -> GetPName
pixelMapTargetToGetPName x = case x of
   IToI -> GetPixelMapIToISize
   SToS -> GetPixelMapSToSSize
   IToR -> GetPixelMapIToRSize
   IToG -> GetPixelMapIToGSize
   IToB -> GetPixelMapIToBSize
   IToA -> GetPixelMapIToASize
   RToR -> GetPixelMapRToRSize
   GToG -> GetPixelMapGToGSize
   BToB -> GetPixelMapBToBSize
   AToA -> GetPixelMapAToASize

--------------------------------------------------------------------------------

maxPixelMapTable :: GettableStateVar GLsizei
maxPixelMapTable = makeGettableStateVar $ getSizei1 id GetMaxPixelMapTable

--------------------------------------------------------------------------------

class Storable c => PixelMapComponent c where
   getPixelMapv :: GLenum -> Ptr c -> IO ()
   pixelMapv :: GLenum -> GLsizei -> Ptr c -> IO ()

instance PixelMapComponent GLushort_ where
   getPixelMapv = glGetPixelMapusv
   pixelMapv = glPixelMapusv

foreign import CALLCONV unsafe "glGetPixelMapusv" glGetPixelMapusv ::
   GLenum -> Ptr GLushort -> IO ()

foreign import CALLCONV unsafe "glPixelMapusv" glPixelMapusv ::
   GLenum -> GLsizei -> Ptr GLushort -> IO ()

instance PixelMapComponent GLuint_ where
   getPixelMapv = glGetPixelMapuiv
   pixelMapv = glPixelMapuiv

foreign import CALLCONV unsafe "glGetPixelMapuiv" glGetPixelMapuiv ::
   GLenum -> Ptr GLuint -> IO ()

foreign import CALLCONV unsafe "glPixelMapuiv" glPixelMapuiv ::
   GLenum -> GLsizei -> Ptr GLuint -> IO ()

instance PixelMapComponent GLfloat_ where
   getPixelMapv = glGetPixelMapfv
   pixelMapv = glPixelMapfv

foreign import CALLCONV unsafe "glGetPixelMapfv" glGetPixelMapfv ::
   GLenum -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glPixelMapfv" glPixelMapfv ::
   GLenum -> GLsizei -> Ptr GLfloat -> IO ()

--------------------------------------------------------------------------------

class PixelMap m where
   withNewPixelMap ::
      PixelMapComponent c => Int -> (Ptr c -> IO ()) -> IO (m c)
   withPixelMap ::
      PixelMapComponent c => m c -> (Int -> Ptr c -> IO a) -> IO a
   newPixelMap :: PixelMapComponent c => [c] -> IO (m c)
   getPixelMapComponents :: PixelMapComponent c => m c -> IO [c]

   withNewPixelMap size act =
      allocaArray size $ \p -> do
         act p
         components <- peekArray size p
         newPixelMap components

   withPixelMap m act = do
      components <- getPixelMapComponents m
      withArrayLen components act

   newPixelMap elements =
      withNewPixelMap (length elements) $ flip pokeArray elements

   getPixelMapComponents m =
      withPixelMap m peekArray

--------------------------------------------------------------------------------

data GLpixelmap a = GLpixelmap Int (ForeignPtr a)
#ifdef __HADDOCK__
-- Help Haddock a bit, because it doesn't do any instance inference.
instance Eq (GLpixelmap a)
instance Ord (GLpixelmap a)
instance Show (GLpixelmap a)
#else
   deriving ( Eq, Ord, Show )
#endif

instance PixelMap GLpixelmap where
   withNewPixelMap size f = do
      fp <- mallocForeignPtrArray size
      withForeignPtr fp f
      return $ GLpixelmap size fp

   withPixelMap (GLpixelmap size fp) f = withForeignPtr fp (f size)

--------------------------------------------------------------------------------

pixelMap :: (PixelMap m, PixelMapComponent c) => PixelMapTarget -> StateVar (m c)
pixelMap pm =
   makeStateVar
      (do size <- pixelMapSize pm
          withNewPixelMap size $ getPixelMapv (marshalPixelMapTarget pm))
      (\theMap -> withPixelMap theMap $ pixelMapv (marshalPixelMapTarget pm) . fromIntegral)

pixelMapSize :: PixelMapTarget -> IO Int
pixelMapSize = getInteger1 fromIntegral . pixelMapTargetToGetPName

--------------------------------------------------------------------------------

-- | Convenience state variable

pixelMapIToRGBA :: PixelMapComponent c => StateVar [Color4 c]
pixelMapIToRGBA = pixelMapXToY (IToR, IToG, IToB, IToA)

-- | Convenience state variable

pixelMapRGBAToRGBA :: PixelMapComponent c => StateVar [Color4 c]
pixelMapRGBAToRGBA = pixelMapXToY (RToR, GToG, BToB, AToA)

pixelMapXToY :: PixelMapComponent c =>
      (PixelMapTarget, PixelMapTarget, PixelMapTarget, PixelMapTarget)
   -> StateVar [Color4 c]
pixelMapXToY targets =
   makeStateVar (getPixelMapXToY targets) (setPixelMapXToY targets)

getPixelMapXToY :: PixelMapComponent c
   => (PixelMapTarget, PixelMapTarget, PixelMapTarget, PixelMapTarget)
   -> IO [Color4 c]
getPixelMapXToY (toR, toG, toB, toA) = do
   withPixelMapFor toR $ \sizeR bufR ->
      withPixelMapFor toG $ \sizeG bufG ->
         withPixelMapFor toB $ \sizeB bufB ->
            withPixelMapFor toA $ \sizeA bufA -> do
               let maxSize = sizeR `max` sizeG `max` sizeB `max` sizeA
               r <- sample sizeR bufR maxSize
               g <- sample sizeR bufG maxSize
               b <- sample sizeR bufB maxSize
               a <- sample sizeR bufA maxSize
               return $ zipWith4 Color4 r g b a

withPixelMapFor ::
    PixelMapComponent c => PixelMapTarget -> (Int -> Ptr c -> IO a) -> IO a
withPixelMapFor target f = do
    theMap <- get (pixelMap target)
    withGLpixelmap theMap f

withGLpixelmap :: PixelMapComponent c
               => GLpixelmap c -> (Int -> Ptr c -> IO a) -> IO a
withGLpixelmap = withPixelMap

sample :: Storable a => Int -> Ptr a -> Int -> IO [a]
sample len ptr newLen = f (fromIntegral (newLen - 1)) []
   where scale :: Float
         scale = fromIntegral len / fromIntegral newLen
         f l acc | l < 0     = return acc
                 | otherwise = do e <- peekElemOff ptr (truncate (l * scale))
                                  f (l - 1) (e : acc)

setPixelMapXToY :: PixelMapComponent c
   => (PixelMapTarget, PixelMapTarget, PixelMapTarget, PixelMapTarget)
   -> [Color4 c] -> IO ()
setPixelMapXToY (toR, toG, toB, toA) colors = do
   (pixelMap toR $=) =<< newGLpixelmap [ r | Color4 r _ _ _ <- colors ]
   (pixelMap toG $=) =<< newGLpixelmap [ g | Color4 _ g _ _ <- colors ]
   (pixelMap toB $=) =<< newGLpixelmap [ b | Color4 _ _ b _ <- colors ]
   (pixelMap toA $=) =<< newGLpixelmap [ a | Color4 _ _ _ a <- colors ]

newGLpixelmap :: PixelMapComponent c => [c] -> IO (GLpixelmap c)
newGLpixelmap = newPixelMap
