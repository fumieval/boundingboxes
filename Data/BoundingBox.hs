{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, Rank2Types #-}
module Data.BoundingBox where

import Linear
import Control.Lens
import Data.Foldable
import Data.Typeable
import Control.Applicative
import Data.Foldable as Foldable
import Data.Traversable as Traversable

data Box f a = Box (f a) (f a) deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Read)

instance Applicative f => Applicative (Box f) where
    pure a = Box (pure a) (pure a)
    {-# INLINE pure #-}
    Box f g <*> Box a b = Box (f <*> a) (g <*> b)
    {-# INLINE (<*>) #-}

isInside :: (Applicative f, Foldable f, Ord a) => f a -> Box f a -> Bool
isInside v (Box p q) = Foldable.and (liftA2 (<=) p v) && Foldable.and (liftA2 (<=) v q)

isCanonical :: (Applicative f, Foldable f, Ord a) => Box f a -> Bool
isCanonical (Box p q) = Foldable.and (liftA2 (<=) p q)

intersect :: (Applicative f, Ord a) => Box f a -> Box f a -> Box f a
intersect (Box p q) (Box r s) = Box (liftA2 max p r) (liftA2 min q s)

corners :: (Applicative f, Traversable f) => Box f a -> [f a]
corners (Box p q) = Traversable.sequence $ liftA2 (\a b -> [a, b]) p q

sizePos :: (Applicative f, Num a) => f a -> Iso' (Box f a) (f a, f a)
sizePos k = iso f g where
    f (Box p q) = (liftA2 (-) q p, (+) <$> liftA2 (*) (fmap (1-) k) p <*> liftA2 (*) k q)
    g (s, v) = Box ((-) <$> v <*> liftA2 (*) k s) ((+) <$> v <*> liftA2 (*) (fmap (1-) k) s)

position :: (Applicative f, Num a) => f a -> Lens' (Box f a) (f a)
position ref = sizePos ref . _2

size :: (Applicative f, Num a) => f a -> Lens' (Box f a) (f a)
size ref = sizePos ref . _1