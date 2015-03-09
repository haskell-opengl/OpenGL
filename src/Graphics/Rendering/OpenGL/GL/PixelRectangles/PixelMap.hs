--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelRectangles.PixelMap
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to a part of section 3.6.1 (Pixel Storage Modes) of
-- the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances #-}

module Graphics.Rendering.OpenGL.GL.PixelRectangles.PixelMap (
   PixelMapTarget(..), PixelMapComponent, PixelMap(..), GLpixelmap,
   maxPixelMapTable, pixelMap, pixelMapIToRGBA, pixelMapRGBAToRGBA,
) where

import Data.List
import Data.StateVar
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.Raw

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
   deriving ( Eq, Ord, Show )

marshalPixelMapTarget :: PixelMapTarget -> GLenum
marshalPixelMapTarget x = case x of
   IToI -> gl_PIXEL_MAP_I_TO_I
   SToS -> gl_PIXEL_MAP_S_TO_S
   IToR -> gl_PIXEL_MAP_I_TO_R
   IToG -> gl_PIXEL_MAP_I_TO_G
   IToB -> gl_PIXEL_MAP_I_TO_B
   IToA -> gl_PIXEL_MAP_I_TO_A
   RToR -> gl_PIXEL_MAP_R_TO_R
   GToG -> gl_PIXEL_MAP_G_TO_G
   BToB -> gl_PIXEL_MAP_B_TO_B
   AToA -> gl_PIXEL_MAP_A_TO_A

pixelMapTargetToGetPName :: PixelMapTarget -> PName1I
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

instance PixelMapComponent GLushort where
   getPixelMapv = glGetPixelMapusv
   pixelMapv = glPixelMapusv

instance PixelMapComponent GLuint where
   getPixelMapv = glGetPixelMapuiv
   pixelMapv = glPixelMapuiv

instance PixelMapComponent GLfloat where
   getPixelMapv = glGetPixelMapfv
   pixelMapv = glPixelMapfv

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
   deriving ( Eq, Ord, Show )

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
