{-# LANGUAGE TemplateHaskell, ViewPatterns, FlexibleInstances #-}
import Data.BoundingBox
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Control.Lens
import Linear
import Control.Applicative
import System.Random

instance (Num a, Random a) => Arbitrary (Box V2 a) where
    arbitrary = sized $ \n -> let k = fromIntegral n in do
        x0 <- choose (-k, 0)
        y0 <- choose (-k, 0)
        x1 <- choose (0, k)
        y1 <- choose (0, k)
        return (Box (V2 x0 y0) (V2 x1 y1))

newtype Reference = Reference { getReference :: V2 Float } deriving Show

instance Arbitrary Reference where
    arbitrary = fmap Reference $ V2 <$> oneof [pure 0, pure 0.5, pure 1] <*> oneof [pure 0, pure 0.5, pure 1]

instance Arbitrary a => Arbitrary (V2 a) where
    arbitrary = V2 <$> arbitrary <*> arbitrary

prop_resize bb (getReference -> ref) sz = nearZero $ bb ^. position ref - bb' ^. position ref where
    bb' = bb & size ref .~ getPositive sz :: Box V2 Float

prop_rearrange (getReference -> ref) bb pos = norm (bb ^. size 0 - bb' ^. size 0) < 10e-4 where
    bb' = bb & position ref .~ pos :: Box V2 Float

prop_construct (getReference -> ref) pos (getPositive -> sz) = nearZero (bb ^. size 0 - sz)
    .&&. nearZero (bb ^. position ref - pos) where
    
    bb = sizePos ref # (sz, pos) :: Box V2 Float

main = $(defaultMainGenerator)