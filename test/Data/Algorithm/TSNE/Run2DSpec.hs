module Data.Algorithm.TSNE.Run2DSpec (main, spec) where

import Data.Default (def)

import Test.Hspec
import Data.Algorithm.TSNE.Run2D
import Data.Algorithm.TSNE.Stepping
import Data.Algorithm.TSNE.Checks
import Data.Algorithm.TSNE.Types
import Data.Algorithm.TSNE.Utils
import Data.Algorithm.TSNE.TestInput
import Data.Algorithm.TSNE.TestMisc

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

u = undefined

spec :: Spec
spec = do
    let n = inputSize testInput
        w = inputValueSize testInput

    describe "initSolution2D" $ do
        it "is right shape" $
            initSolution2D n >>= (`shouldSatisfy` has2DShape (n,2))

    describe "initState" $ do
        it "is valid state" $
            initState2D n >>= (`shouldSatisfy` isRight . (isValidStateForInput 2 testInput))
