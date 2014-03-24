{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable, Rank2Types #-}
module Data.BoundingBox.Dim2 (
    BoundingBox(..)
    , _TLBR
    , _BLTR
    , Reference(..)
    , position
    , size
    ) where

import Linear
import Control.Lens
import Data.Foldable
import Data.Typeable

data BoundingBox a = BoundingBox a a a a deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Read, Typeable)

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