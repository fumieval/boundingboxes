{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
import Data.BoundingBox.Dim2
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Control.Lens
import Linear
import Control.Applicative
import System.Random

instance (Num a, Random a) => Arbitrary (BoundingBox a) where
    arbitrary = sized $ \n -> let k = fromIntegral n in do
        x0 <- choose (-k, 0)
        y0 <- choose (-k, 0)
        x1 <- choose (0, k)
        y1 <- choose (0, k)
        return (BoundingBox x0 y0 x1 y1)

instance Arbitrary Reference where
    arbitrary = toEnum <$> choose (0, 8)

instance Arbitrary a => Arbitrary (V2 a) where
    arbitrary = V2 <$> arbitrary <*> arbitrary

prop_resize bb ref sz = nearZero $ bb ^. position ref - bb' ^. position ref where
    bb' = bb & size ref .~ getPositive sz :: BoundingBox Float

prop_rearrange ref bb pos = nearZero $ bb ^. size C - bb' ^. size C where
    bb' = bb & position ref .~ pos :: BoundingBox Float

prop_construct ref pos (getPositive -> sz) = nearZero (bb ^. size C - sz)
    .&&. nearZero (bb ^. position ref - pos) where
    
    bb = sizePos ref # (sz, pos) :: BoundingBox Float

prop_enclosure xs = and [inBoundingBox p (foldr enclose (pure 0) ps) | p <- ps] where
    ps = map (uncurry V2) xs :: [V2 Float]

main = $(defaultMainGenerator)