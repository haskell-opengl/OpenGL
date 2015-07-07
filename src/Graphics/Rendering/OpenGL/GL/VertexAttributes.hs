{-# LANGUAGE DeriveDataTypeable, CPP #-}
{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.VertexAttributes
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for auxiliary vertex attributes.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.VertexAttributes (
   TexCoord1(..), TexCoord2(..), TexCoord3(..), TexCoord4(..),
   Normal3(..),
   FogCoord1(..),
   Color3(..), Color4(..),
   Index1(..)
) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ( Applicative(..) )
#endif
import Control.Monad
import Data.Foldable
import Data.Ix
import Data.Traversable
import Data.Typeable
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

--------------------------------------------------------------------------------

-- | Texture coordinates with /t/=0, /r/=0, and /q/=1.
newtype TexCoord1 a = TexCoord1 a
   deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)

instance Functor TexCoord1 where
   fmap f (TexCoord1 x) = TexCoord1 (f x)

instance Applicative TexCoord1 where
   pure a = TexCoord1 a
   TexCoord1 f <*> TexCoord1 x = TexCoord1 (f x)

instance Foldable TexCoord1 where
   foldr f a (TexCoord1 x) = x `f ` a
   foldl f a (TexCoord1 x) = a `f` x
   foldr1 _ (TexCoord1 x) = x
   foldl1 _ (TexCoord1 x) = x

instance Traversable TexCoord1 where
   traverse f (TexCoord1 x) = pure TexCoord1 <*> f x
   sequenceA (TexCoord1 x) =  pure TexCoord1 <*> x
   mapM f (TexCoord1 x) = return TexCoord1 `ap` f x
   sequence (TexCoord1 x) = return TexCoord1 `ap` x

instance Storable a => Storable (TexCoord1 a) where
   sizeOf    ~(TexCoord1 s) = sizeOf s
   alignment ~(TexCoord1 s) = alignment s
   peek = peekApplicativeTraversable
   poke = pokeFoldable

--------------------------------------------------------------------------------

-- | Texture coordinates with /r/=0 and /q/=1.
data TexCoord2 a = TexCoord2 !a !a
   deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)

instance Functor TexCoord2 where
   fmap f (TexCoord2 x y) = TexCoord2 (f x) (f y)

instance Applicative TexCoord2 where
   pure a = TexCoord2 a a
   TexCoord2 f g <*> TexCoord2 x y = TexCoord2 (f x) (g y)

instance Foldable TexCoord2 where
   foldr f a (TexCoord2 x y) = x `f ` (y `f` a)
   foldl f a (TexCoord2 x y) = (a `f` x) `f` y
   foldr1 f (TexCoord2 x y) = x `f` y
   foldl1 f (TexCoord2 x y) = x `f` y

instance Traversable TexCoord2 where
   traverse f (TexCoord2 x y) = pure TexCoord2 <*> f x <*> f y
   sequenceA (TexCoord2 x y) =  pure TexCoord2 <*> x <*> y
   mapM f (TexCoord2 x y) = return TexCoord2 `ap` f x `ap` f y
   sequence (TexCoord2 x y) = return TexCoord2 `ap` x `ap` y

instance Storable a => Storable (TexCoord2 a) where
   sizeOf ~(TexCoord2 x _) = 2 * sizeOf x
   alignment ~(TexCoord2 x _) = alignment x
   peek = peekApplicativeTraversable
   poke = pokeFoldable

--------------------------------------------------------------------------------

-- | Texture coordinates with /q/=1.
data TexCoord3 a = TexCoord3 !a !a !a
   deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)

instance Functor TexCoord3 where
   fmap f (TexCoord3 x y z) = TexCoord3 (f x) (f y) (f z)

instance Applicative TexCoord3 where
   pure a = TexCoord3 a a a
   TexCoord3 f g h <*> TexCoord3 x y z = TexCoord3 (f x) (g y) (h z)

instance Foldable TexCoord3 where
   foldr f a (TexCoord3 x y z) = x `f ` (y `f` (z `f` a))
   foldl f a (TexCoord3 x y z) = ((a `f` x) `f` y) `f` z
   foldr1 f (TexCoord3 x y z) = x `f` (y `f` z)
   foldl1 f (TexCoord3 x y z) = (x `f` y) `f` z

instance Traversable TexCoord3 where
   traverse f (TexCoord3 x y z) = pure TexCoord3 <*> f x <*> f y <*> f z
   sequenceA (TexCoord3 x y z) =  pure TexCoord3 <*> x <*> y <*> z
   mapM f (TexCoord3 x y z) = return TexCoord3 `ap` f x `ap` f y `ap` f z
   sequence (TexCoord3 x y z) = return TexCoord3 `ap` x `ap` y `ap` z

instance Storable a => Storable (TexCoord3 a) where
   sizeOf ~(TexCoord3 x _ _) = 3 * sizeOf x
   alignment ~(TexCoord3 x _ _) = alignment x
   peek = peekApplicativeTraversable
   poke = pokeFoldable

--------------------------------------------------------------------------------

-- | Fully-fledged four-dimensional texture coordinates.
data TexCoord4 a = TexCoord4 !a !a !a !a
   deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)

instance Functor TexCoord4 where
   fmap f (TexCoord4 x y z w) = TexCoord4 (f x) (f y) (f z) (f w)

instance Applicative TexCoord4 where
   pure a = TexCoord4 a a a a
   TexCoord4 f g h i <*> TexCoord4 x y z w = TexCoord4 (f x) (g y) (h z) (i w)

instance Foldable TexCoord4 where
   foldr f a (TexCoord4 x y z w) = x `f ` (y `f` (z `f` (w `f` a)))
   foldl f a (TexCoord4 x y z w) = (((a `f` x) `f` y) `f` z) `f` w
   foldr1 f (TexCoord4 x y z w) = x `f` (y `f` (z `f` w))
   foldl1 f (TexCoord4 x y z w) = ((x `f` y) `f` z) `f` w

instance Traversable TexCoord4 where
   traverse f (TexCoord4 x y z w) = pure TexCoord4 <*> f x <*> f y <*> f z <*> f w
   sequenceA (TexCoord4 x y z w) =  pure TexCoord4 <*> x <*> y <*> z <*> w
   mapM f (TexCoord4 x y z w) = return TexCoord4 `ap` f x `ap` f y `ap` f z `ap` f w
   sequence (TexCoord4 x y z w) = return TexCoord4 `ap` x `ap` y `ap` z `ap` w

instance Storable a => Storable (TexCoord4 a) where
   sizeOf ~(TexCoord4 x _ _ _) = 4 * sizeOf x
   alignment ~(TexCoord4 x _ _ _) = alignment x
   peek = peekApplicativeTraversable
   poke = pokeFoldable

--------------------------------------------------------------------------------

-- A three-dimensional normal.
data Normal3 a = Normal3 !a !a !a
   deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable) 
instance Functor Normal3 where
   fmap f (Normal3 x y z) = Normal3 (f x) (f y) (f z)

instance Applicative Normal3 where
   pure a = Normal3 a a a
   Normal3 f g h <*> Normal3 x y z = Normal3 (f x) (g y) (h z)

instance Foldable Normal3 where
   foldr f a (Normal3 x y z) = x `f ` (y `f` (z `f` a))
   foldl f a (Normal3 x y z) = ((a `f` x) `f` y) `f` z
   foldr1 f (Normal3 x y z) = x `f` (y `f` z)
   foldl1 f (Normal3 x y z) = (x `f` y) `f` z

instance Traversable Normal3 where
   traverse f (Normal3 x y z) = pure Normal3 <*> f x <*> f y <*> f z
   sequenceA (Normal3 x y z) =  pure Normal3 <*> x <*> y <*> z
   mapM f (Normal3 x y z) = return Normal3 `ap` f x `ap` f y `ap` f z
   sequence (Normal3 x y z) = return Normal3 `ap` x `ap` y `ap` z

instance Storable a => Storable (Normal3 a) where
   sizeOf ~(Normal3 x _ _) = 3 * sizeOf x
   alignment ~(Normal3 x _ _) = alignment x
   peek = peekApplicativeTraversable
   poke = pokeFoldable

--------------------------------------------------------------------------------

-- | A fog coordinate.
newtype FogCoord1 a = FogCoord1 a
   deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)

instance Functor FogCoord1 where
   fmap f (FogCoord1 x) = FogCoord1 (f x)

instance Applicative FogCoord1 where
   pure a = FogCoord1 a
   FogCoord1 f <*> FogCoord1 x = FogCoord1 (f x)

instance Foldable FogCoord1 where
   foldr f a (FogCoord1 x) = x `f ` a
   foldl f a (FogCoord1 x)  = a `f` x
   foldr1 _ (FogCoord1 x) = x
   foldl1 _ (FogCoord1 x) = x

instance Traversable FogCoord1 where
   traverse f (FogCoord1 x) = pure FogCoord1 <*> f x
   sequenceA (FogCoord1 x) =  pure FogCoord1 <*> x
   mapM f (FogCoord1 x) = return FogCoord1 `ap` f x
   sequence (FogCoord1 x) = return FogCoord1 `ap` x

instance Storable a => Storable (FogCoord1 a) where
   sizeOf    ~(FogCoord1 s) = sizeOf s
   alignment ~(FogCoord1 s) = alignment s
   peek = peekApplicativeTraversable
   poke = pokeFoldable

--------------------------------------------------------------------------------

-- An RGBA color with /A/=1.
data Color3 a = Color3 !a !a !a
   deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)

instance Functor Color3 where
   fmap f (Color3 x y z) = Color3 (f x) (f y) (f z)

instance Applicative Color3 where
   pure a = Color3 a a a
   Color3 f g h <*> Color3 x y z = Color3 (f x) (g y) (h z)

instance Foldable Color3 where
   foldr f a (Color3 x y z) = x `f ` (y `f` (z `f` a))
   foldl f a (Color3 x y z) = ((a `f` x) `f` y) `f` z
   foldr1 f (Color3 x y z) = x `f` (y `f` z)
   foldl1 f (Color3 x y z) = (x `f` y) `f` z

instance Traversable Color3 where
   traverse f (Color3 x y z) = pure Color3 <*> f x <*> f y <*> f z
   sequenceA (Color3 x y z) =  pure Color3 <*> x <*> y <*> z
   mapM f (Color3 x y z) = return Color3 `ap` f x `ap` f y `ap` f z
   sequence (Color3 x y z) = return Color3 `ap` x `ap` y `ap` z

instance Storable a => Storable (Color3 a) where
   sizeOf ~(Color3 x _ _) = 3 * sizeOf x
   alignment ~(Color3 x _ _) = alignment x
   peek = peekApplicativeTraversable
   poke = pokeFoldable

--------------------------------------------------------------------------------

-- | A fully-fledged RGBA color.
data Color4 a = Color4 !a !a !a !a
   deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)

instance Functor Color4 where
   fmap f (Color4 x y z w) = Color4 (f x) (f y) (f z) (f w)

instance Applicative Color4 where
   pure a = Color4 a a a a
   Color4 f g h i <*> Color4 x y z w = Color4 (f x) (g y) (h z) (i w)

instance Foldable Color4 where
   foldr f a (Color4 x y z w) = x `f ` (y `f` (z `f` (w `f` a)))
   foldl f a (Color4 x y z w) = (((a `f` x) `f` y) `f` z) `f` w
   foldr1 f (Color4 x y z w) = x `f` (y `f` (z `f` w))
   foldl1 f (Color4 x y z w) = ((x `f` y) `f` z) `f` w

instance Traversable Color4 where
   traverse f (Color4 x y z w) = pure Color4 <*> f x <*> f y <*> f z <*> f w
   sequenceA (Color4 x y z w) =  pure Color4 <*> x <*> y <*> z <*> w
   mapM f (Color4 x y z w) = return Color4 `ap` f x `ap` f y `ap` f z `ap` f w
   sequence (Color4 x y z w) = return Color4 `ap` x `ap` y `ap` z `ap` w

instance Storable a => Storable (Color4 a) where
   sizeOf ~(Color4 x _ _ _) = 4 * sizeOf x
   alignment ~(Color4 x _ _ _) = alignment x
   peek = peekApplicativeTraversable
   poke = pokeFoldable

--------------------------------------------------------------------------------

-- | A color index.
newtype Index1 a = Index1 a
   deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)

instance Functor Index1 where
   fmap f (Index1 x) = Index1 (f x)

instance Applicative Index1 where
   pure a = Index1 a
   Index1 f <*> Index1 x = Index1 (f x)

instance Foldable Index1 where
   foldr f a (Index1 x) = x `f ` a
   foldl f a (Index1 x)  = a `f` x
   foldr1 _ (Index1 x) = x
   foldl1 _ (Index1 x) = x

instance Traversable Index1 where
   traverse f (Index1 x) = pure Index1 <*> f x
   sequenceA (Index1 x) =  pure Index1 <*> x
   mapM f (Index1 x) = return Index1 `ap` f x
   sequence (Index1 x) = return Index1 `ap` x

instance Storable a => Storable (Index1 a) where
   sizeOf    ~(Index1 s) = sizeOf s
   alignment ~(Index1 s) = alignment s
   peek = peekApplicativeTraversable
   poke = pokeFoldable

--------------------------------------------------------------------------------

peekApplicativeTraversable :: (Applicative t, Traversable t, Storable a) => Ptr (t a) -> IO (t a)
peekApplicativeTraversable = Data.Traversable.mapM peek . addresses

addresses :: (Applicative t, Traversable t, Storable a) => Ptr (t a) -> t (Ptr a)
addresses = snd . mapAccumL nextPtr 0 . pure . castPtr

nextPtr :: Storable a => Int -> Ptr a -> (Int, Ptr a)
nextPtr offset ptr = (offset + 1, advancePtr ptr offset)

--------------------------------------------------------------------------------

pokeFoldable :: (Foldable t, Storable a) => Ptr (t a) -> t a -> IO ()
pokeFoldable ptr xs = foldlM pokeAndAdvance (castPtr ptr) xs >> return ()

pokeAndAdvance :: Storable a => Ptr a -> a -> IO (Ptr a)
pokeAndAdvance ptr value = do
   poke ptr value
   return $ ptr `plusPtr` sizeOf value
