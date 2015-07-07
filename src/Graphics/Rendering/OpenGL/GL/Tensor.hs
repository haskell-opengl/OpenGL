{-# LANGUAGE DeriveDataTypeable, CPP #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Tensor
-- Copyright   :  (c) Sven Panne 2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This package contains tensor data types and their instances for some basic
-- type classes.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Tensor (
   Vertex1(..), Vertex2(..), Vertex3(..), Vertex4(..),
   Vector1(..), Vector2(..), Vector3(..), Vector4(..)
) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ( Applicative(..) )
import Data.Foldable ( Foldable(..) )
#endif
import Control.Monad ( ap )
import Data.Foldable ( foldlM )
import Data.Ix ( Ix )
import Data.Traversable ( Traversable(..), mapAccumL )
import Data.Typeable ( Typeable )
import Foreign.Marshal.Array ( advancePtr )
import Foreign.Ptr ( Ptr, plusPtr, castPtr )
import Foreign.Storable ( Storable(..) )

--------------------------------------------------------------------------------

-- | A vertex with /y/=0, /z/=0 and /w/=1.
newtype Vertex1 a = Vertex1 a
   deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)

instance Functor Vertex1 where
   fmap f (Vertex1 x) = Vertex1 (f x)

instance Applicative Vertex1 where
   pure a = Vertex1 a
   Vertex1 f <*> Vertex1 x = Vertex1 (f x)

instance Foldable Vertex1 where
   foldr f a (Vertex1 x) = x `f ` a
   foldl f a (Vertex1 x) = a `f` x
   foldr1 _ (Vertex1 x) = x
   foldl1 _ (Vertex1 x) = x

instance Traversable Vertex1 where
   traverse f (Vertex1 x) = pure Vertex1 <*> f x
   sequenceA (Vertex1 x) =  pure Vertex1 <*> x
   mapM f (Vertex1 x) = return Vertex1 `ap` f x
   sequence (Vertex1 x) = return Vertex1 `ap` x

instance Storable a => Storable (Vertex1 a) where
   sizeOf    ~(Vertex1 s) = sizeOf s
   alignment ~(Vertex1 s) = alignment s
   peek = peekApplicativeTraversable
   poke = pokeFoldable

--------------------------------------------------------------------------------

-- | A vertex with /z/=0 and /w/=1.
data Vertex2 a = Vertex2 !a !a
   deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)

instance Functor Vertex2 where
   fmap f (Vertex2 x y) = Vertex2 (f x) (f y)

instance Applicative Vertex2 where
   pure a = Vertex2 a a
   Vertex2 f g <*> Vertex2 x y = Vertex2 (f x) (g y)

instance Foldable Vertex2 where
   foldr f a (Vertex2 x y) = x `f ` (y `f` a)
   foldl f a (Vertex2 x y) = (a `f` x) `f` y
   foldr1 f (Vertex2 x y) = x `f` y
   foldl1 f (Vertex2 x y) = x `f` y

instance Traversable Vertex2 where
   traverse f (Vertex2 x y) = pure Vertex2 <*> f x <*> f y
   sequenceA (Vertex2 x y) =  pure Vertex2 <*> x <*> y
   mapM f (Vertex2 x y) = return Vertex2 `ap` f x `ap` f y
   sequence (Vertex2 x y) = return Vertex2 `ap` x `ap` y

instance Storable a => Storable (Vertex2 a) where
   sizeOf ~(Vertex2 x _) = 2 * sizeOf x
   alignment ~(Vertex2 x _) = alignment x
   peek = peekApplicativeTraversable
   poke = pokeFoldable

--------------------------------------------------------------------------------

-- | A vertex with /w/=1.
data Vertex3 a = Vertex3 !a !a !a
   deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)

instance Functor Vertex3 where
   fmap f (Vertex3 x y z) = Vertex3 (f x) (f y) (f z)

instance Applicative Vertex3 where
   pure a = Vertex3 a a a
   Vertex3 f g h <*> Vertex3 x y z = Vertex3 (f x) (g y) (h z)

instance Foldable Vertex3 where
   foldr f a (Vertex3 x y z) = x `f ` (y `f` (z `f` a))
   foldl f a (Vertex3 x y z) = ((a `f` x) `f` y) `f` z
   foldr1 f (Vertex3 x y z) = x `f` (y `f` z)
   foldl1 f (Vertex3 x y z) = (x `f` y) `f` z

instance Traversable Vertex3 where
   traverse f (Vertex3 x y z) = pure Vertex3 <*> f x <*> f y <*> f z
   sequenceA (Vertex3 x y z) =  pure Vertex3 <*> x <*> y <*> z
   mapM f (Vertex3 x y z) = return Vertex3 `ap` f x `ap` f y `ap` f z
   sequence (Vertex3 x y z) = return Vertex3 `ap` x `ap` y `ap` z

instance Storable a => Storable (Vertex3 a) where
   sizeOf ~(Vertex3 x _ _) = 3 * sizeOf x
   alignment ~(Vertex3 x _ _) = alignment x
   peek = peekApplicativeTraversable
   poke = pokeFoldable

--------------------------------------------------------------------------------

-- | A fully-fledged four-dimensional vertex.
data Vertex4 a = Vertex4 !a !a !a !a
   deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)

instance Functor Vertex4 where
   fmap f (Vertex4 x y z w) = Vertex4 (f x) (f y) (f z) (f w)

instance Applicative Vertex4 where
   pure a = Vertex4 a a a a
   Vertex4 f g h i <*> Vertex4 x y z w = Vertex4 (f x) (g y) (h z) (i w)

instance Foldable Vertex4 where
   foldr f a (Vertex4 x y z w) = x `f ` (y `f` (z `f` (w `f` a)))
   foldl f a (Vertex4 x y z w) = (((a `f` x) `f` y) `f` z) `f` w
   foldr1 f (Vertex4 x y z w) = x `f` (y `f` (z `f` w))
   foldl1 f (Vertex4 x y z w) = ((x `f` y) `f` z) `f` w

instance Traversable Vertex4 where
   traverse f (Vertex4 x y z w) = pure Vertex4 <*> f x <*> f y <*> f z <*> f w
   sequenceA (Vertex4 x y z w) =  pure Vertex4 <*> x <*> y <*> z <*> w
   mapM f (Vertex4 x y z w) = return Vertex4 `ap` f x `ap` f y `ap` f z `ap` f w
   sequence (Vertex4 x y z w) = return Vertex4 `ap` x `ap` y `ap` z `ap` w

instance Storable a => Storable (Vertex4 a) where
   sizeOf ~(Vertex4 x _ _ _) = 4 * sizeOf x
   alignment ~(Vertex4 x _ _ _) = alignment x
   peek = peekApplicativeTraversable
   poke = pokeFoldable

--------------------------------------------------------------------------------

-- | A one-dimensional vector.
newtype Vector1 a = Vector1 a
   deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)

instance Functor Vector1 where
   fmap f (Vector1 x) = Vector1 (f x)

instance Applicative Vector1 where
   pure a = Vector1 a
   Vector1 f <*> Vector1 x = Vector1 (f x)

instance Foldable Vector1 where
   foldr f a (Vector1 x) = x `f ` a
   foldl f a (Vector1 x) = a `f` x
   foldr1 _ (Vector1 x) = x
   foldl1 _ (Vector1 x) = x

instance Traversable Vector1 where
   traverse f (Vector1 x) = pure Vector1 <*> f x
   sequenceA (Vector1 x) =  pure Vector1 <*> x
   mapM f (Vector1 x) = return Vector1 `ap` f x
   sequence (Vector1 x) = return Vector1 `ap` x

instance Storable a => Storable (Vector1 a) where
   sizeOf    ~(Vector1 s) = sizeOf s
   alignment ~(Vector1 s) = alignment s
   peek = peekApplicativeTraversable
   poke = pokeFoldable

--------------------------------------------------------------------------------

-- | A two-dimensional vector.
data Vector2 a = Vector2 !a !a
   deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)

instance Functor Vector2 where
   fmap f (Vector2 x y) = Vector2 (f x) (f y)

instance Applicative Vector2 where
   pure a = Vector2 a a
   Vector2 f g <*> Vector2 x y = Vector2 (f x) (g y)

instance Foldable Vector2 where
   foldr f a (Vector2 x y) = x `f ` (y `f` a)
   foldl f a (Vector2 x y) = (a `f` x) `f` y
   foldr1 f (Vector2 x y) = x `f` y
   foldl1 f (Vector2 x y) = x `f` y

instance Traversable Vector2 where
   traverse f (Vector2 x y) = pure Vector2 <*> f x <*> f y
   sequenceA (Vector2 x y) =  pure Vector2 <*> x <*> y
   mapM f (Vector2 x y) = return Vector2 `ap` f x `ap` f y
   sequence (Vector2 x y) = return Vector2 `ap` x `ap` y

instance Storable a => Storable (Vector2 a) where
   sizeOf ~(Vector2 x _) = 2 * sizeOf x
   alignment ~(Vector2 x _) = alignment x
   peek = peekApplicativeTraversable
   poke = pokeFoldable

--------------------------------------------------------------------------------

-- | A three-dimensional vector.
data Vector3 a = Vector3 !a !a !a
   deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)

instance Functor Vector3 where
   fmap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)

instance Applicative Vector3 where
   pure a = Vector3 a a a
   Vector3 f g h <*> Vector3 x y z = Vector3 (f x) (g y) (h z)

instance Foldable Vector3 where
   foldr f a (Vector3 x y z) = x `f ` (y `f` (z `f` a))
   foldl f a (Vector3 x y z) = ((a `f` x) `f` y) `f` z
   foldr1 f (Vector3 x y z) = x `f` (y `f` z)
   foldl1 f (Vector3 x y z) = (x `f` y) `f` z

instance Traversable Vector3 where
   traverse f (Vector3 x y z) = pure Vector3 <*> f x <*> f y <*> f z
   sequenceA (Vector3 x y z) =  pure Vector3 <*> x <*> y <*> z
   mapM f (Vector3 x y z) = return Vector3 `ap` f x `ap` f y `ap` f z
   sequence (Vector3 x y z) = return Vector3 `ap` x `ap` y `ap` z

instance Storable a => Storable (Vector3 a) where
   sizeOf ~(Vector3 x _ _) = 3 * sizeOf x
   alignment ~(Vector3 x _ _) = alignment x
   peek = peekApplicativeTraversable
   poke = pokeFoldable

--------------------------------------------------------------------------------

-- | A four-dimensional vector.
data Vector4 a = Vector4 !a !a !a !a
   deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)

instance Functor Vector4 where
   fmap f (Vector4 x y z w) = Vector4 (f x) (f y) (f z) (f w)

instance Applicative Vector4 where
   pure a = Vector4 a a a a
   Vector4 f g h i <*> Vector4 x y z w = Vector4 (f x) (g y) (h z) (i w)

instance Foldable Vector4 where
   foldr f a (Vector4 x y z w) = x `f ` (y `f` (z `f` (w `f` a)))
   foldl f a (Vector4 x y z w) = (((a `f` x) `f` y) `f` z) `f` w
   foldr1 f (Vector4 x y z w) = x `f` (y `f` (z `f` w))
   foldl1 f (Vector4 x y z w) = ((x `f` y) `f` z) `f` w

instance Traversable Vector4 where
   traverse f (Vector4 x y z w) = pure Vector4 <*> f x <*> f y <*> f z <*> f w
   sequenceA (Vector4 x y z w) =  pure Vector4 <*> x <*> y <*> z <*> w
   mapM f (Vector4 x y z w) = return Vector4 `ap` f x `ap` f y `ap` f z `ap` f w
   sequence (Vector4 x y z w) = return Vector4 `ap` x `ap` y `ap` z `ap` w

instance Storable a => Storable (Vector4 a) where
   sizeOf ~(Vector4 x _ _ _) = 4 * sizeOf x
   alignment ~(Vector4 x _ _ _) = alignment x
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
