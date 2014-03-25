{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable, Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BoundingBox.Dim2
-- Copyright   :  (C) 2014 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
-- The type and accessors for 2D bounding boxes
----------------------------------------------------------------------------
module Data.BoundingBox.Dim2 (
    BoundingBox(..)
    , inBoundingBox
    , intersect
    , enclose
    , _TLBR
    , _BLTR
    , _Corners
    , Reference(..)
    , sizePos
    , position
    , size
    ) where

import Linear
import Control.Lens
import Data.Foldable
import Data.Typeable
import Control.Applicative

data BoundingBox a = BoundingBox !a !a !a !a deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Read, Typeable)

instance Applicative BoundingBox where
    pure a = BoundingBox a a a a
    BoundingBox f0 g0 f1 g1 <*> BoundingBox x0 y0 x1 y1 = BoundingBox (f0 x0) (g0 y0) (f1 x1) (g1 y1)

-- | Determine whether the given point is in the 'BoundingBox'.
inBoundingBox :: Ord a => V2 a -> BoundingBox a -> Bool
inBoundingBox (V2 x y) (BoundingBox x0 y0 x1 y1) = x0 <= x && x <= x1 && y0 <= y && y <= y1

-- | Intersection between two boundingboxes.
intersect :: Ord a => BoundingBox a -> BoundingBox a -> Maybe (BoundingBox a)
intersect (BoundingBox x0 y0 x1 y1) (BoundingBox x2 y2 x3 y3)
    | x4 > x5 = Nothing
    | y4 > y5 = Nothing
    | otherwise = Just $ BoundingBox x4 y4 x5 y5
    where
        x4 = max x0 x2
        y4 = max y0 y2
        x5 = min x1 x3
        y5 = min y1 y3

enclose :: (Num a, Ord a) => V2 a -> BoundingBox a -> BoundingBox a
enclose (V2 x y) (BoundingBox x0 y0 x1 y1) = BoundingBox (min x x0) (min y y0) (max x x1) (max y y1)

-- | The type of reference points.
-- @
-- TL--T--TR
-- |       |
-- L   C   R
-- |       |
-- BL--B--BR
-- @
data Reference = TL | T | TR
               |  L | C | R
               | BL | B | BR
    deriving (Show, Eq, Ord, Read, Enum, Bounded)

-- |
-- @
-- fst----+
--  |     |
--  +----snd
-- @
_TLBR :: Iso' (BoundingBox a) (V2 a, V2 a)
_TLBR = iso (\(BoundingBox x0 y0 x1 y1) -> (V2 x0 y0, V2 x1 y1)) (\(V2 x0 y0, V2 x1 y1) -> BoundingBox x0 y0 x1 y1)

-- |
-- @
--  +----snd
--  |     |
-- fst----+
-- @
_BLTR :: Iso' (BoundingBox a) (V2 a, V2 a)
_BLTR = iso (\(BoundingBox x0 y0 x1 y1) -> (V2 x0 y1, V2 x1 y0)) (\(V2 x0 y1, V2 x1 y0) -> BoundingBox x0 y0 x1 y1)

sizePos :: Fractional a => Reference -> Iso' (BoundingBox a) (V2 a, V2 a)
sizePos ref = iso f g where
    f (BoundingBox x0 y0 x1 y1) = (V2 (x1 - x0) (y1 - y0), V2 x0 y0 * (1 - k) + V2 x1 y1 * k)
    g (s@(V2 w h), p) = BoundingBox x0 y0 x1 y1 where
        V2 x0 y0 = p - k * s
        V2 x1 y1 = p + (1 - k) * s
    k = case ref of
        TL -> V2 0 0
        T -> V2 0.5 0
        TR -> V2 1 0
        L -> V2 0 0.5
        C -> V2 0.5 0.5
        R -> V2 1 0.5
        BL -> V2 0 1
        B -> V2 0.5 1
        BR -> V2 1 1

_Corners :: Traversal' (BoundingBox a) (V2 a)
_Corners f (BoundingBox x0 y0 x1 y1) = go <$> f (V2 x0 y0) <*> f (V2 x1 y0) <*> f (V2 x1 y1) <*> f (V2 x0 y1) where
    go (V2 x0' _) (V2 _ y1') (V2 x2' _) (V2 _ y3') = BoundingBox x0' y1' x2' y3'

position :: Fractional a => Reference -> Lens' (BoundingBox a) (V2 a)
position ref = sizePos ref . _2

size :: Fractional a => Reference -> Lens' (BoundingBox a) (V2 a)
size ref = sizePos ref . _1