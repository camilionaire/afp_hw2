module Main where

import Control.Applicative
import Data.Finite -- (Finite, finite) I think can just pull everything in.
import Data.Proxy (Proxy(..))
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GHC.TypeNats
import Test.QuickCheck

import Grid

instance (KnownNat n, Arbitrary a) => Arbitrary (Vector n a) where
    arbitrary = sequenceA (pure arbitrary) --genVector

gen2DVector ::
    forall w h a.
    (KnownBounds w h, Arbitrary a) =>
    Gen (Vector h (Vector w a))
gen2DVector = arbitrary

instance (KnownBounds w h, Arbitrary a) => Arbitrary (Grid w h a) where
    arbitrary = fmap Grid arbitrary

instance (KnownNat n, 1 <= n) => Arbitrary (Finite n) where
    arbitrary = 
        fmap finite 
            (chooseInteger (0, fromIntegral (natVal @n Proxy) - 1))

main :: IO ()
main = do
    putStrLn "TEST!!!"
    -- this test tests this thing:
    -- index (replace i x g) i == x
    quickCheck 
            (forAll arbitrary (\(i :: Index 3 5 , j, g, x :: Int) ->
                        index (replace i x g) j ===
                            if i == j then
                                x
                            else
                                index g j))
