module Data.Algorithm.TSNE.PreparationSpec (main, spec) where

import Data.Default (def)

import Test.Hspec
import Data.Algorithm.TSNE.Preparation
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

    describe "testInput" $ do
        it "is right shape" $ do
            testInput `shouldSatisfy` has2DShape (64, 20)
        it "is valid" $ do
            inputIsValid testInput `shouldBe` Right ()
        it "has right size" $ do
            inputSize testInput `shouldBe` 20
        it "has right value size" $ do
            inputValueSize testInput `shouldBe` 64

    describe "neighbourProbabilities" $ do
        it "is right shape" $ do
            testNeighbourProbs `shouldSatisfy` has2DShape (n,n)


