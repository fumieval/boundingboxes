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
    , _TLBR
    , _BLTR
    , _Corners
    , Reference(..)
    , position
    , size
    ) where

import Linear
import Control.Lens
import Data.Foldable
import Data.Typeable
import Control.Applicative

data BoundingBox a = BoundingBox a a a a deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Read, Typeable)

-- | Determine whether the given point is in the 'BoundingBox'.
inBoundingBox :: Ord a => V2 a -> BoundingBox a -> Bool
inBoundingBox (V2 x y) (BoundingBox x0 y0 x1 y1) = x0 <= x && x <= x1 && y0 <= y && y <= y1

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
    deriving (Show, Eq, Ord, Read)

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

_Corners :: Traversal' (BoundingBox a) (V2 a)
_Corners f (BoundingBox x0 y0 x1 y1) = go <$> f (V2 x0 y0) <*> f (V2 x1 y0) <*> f (V2 x1 y1) <*> f (V2 x0 y1) where
    go (V2 x0' _) (V2 _ y1') (V2 x2' _) (V2 _ y3') = BoundingBox x0' y1' x2' y3'

position :: Fractional a => Reference -> Lens' (BoundingBox a) (V2 a)
position ref f (BoundingBox x0 y0 x1 y1) = f (V2 x0 y0 + offset)
    <&> \v -> let V2 x y = v - offset in BoundingBox x y (x + w) (y + h) where
    w = x1 - x0
    h = y1 - y0
    offset = case ref of
        TL -> V2 0 0
        T -> V2 (w / 2) 0
        TR -> V2 w 0
        L -> V2 0 (h / 2)
        C -> V2 (w / 2) (h / 2)
        R -> V2 w (h / 2)
        BL -> V2 0 h
        B -> V2 (w / 2) h
        BR -> V2 w h

size :: Fractional a => Reference -> Lens' (BoundingBox a) (V2 a)
size ref f (BoundingBox x0 y0 x1 y1) = f (V2 w h)
    <&> \(V2 w' h') -> BoundingBox (x0 - p * (w' - w)) (y0 - q * (h' - h)) (x1 + (1 - p) * (w' - w)) (y1 + (1 - q) * (h' - h))
    where
        w = x1 - x0
        h = y1 - y0
        p = case ref of
            TL -> 0
            T -> 0.5
            TR -> 1
            L -> 0
            C -> 0.5
            R -> 1
            BL -> 0
            B -> 0.5
            BR -> 1
        q = case ref of
            TL -> 0
            L -> 0.5
            BL -> 1
            T -> 0
            C -> 0.5
            B -> 1
            TR -> 0
            R -> 0.5
            BR -> 1