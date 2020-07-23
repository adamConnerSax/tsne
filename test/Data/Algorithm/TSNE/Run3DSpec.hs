module Data.Algorithm.TSNE.Run3DSpec (main, spec) where

import Data.Default (def)

import Test.Hspec
import Data.Algorithm.TSNE.Run3D
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

    describe "initSolution3D" $ do
        it "is right shape" $
            initSolution3D n >>= (`shouldSatisfy` has2DShape (n,3))

    describe "initState" $ do
        it "is valid state" $
            initState3D n >>= (`shouldSatisfy` isRight . (isValidStateForInput 3 testInput))

    describe "qdist" $ do
        it "is right shape" $ do
            s <- initSolution3D n
            qdist s `shouldSatisfy` has2DShape (n,n)        

    describe "qdist'" $ do
        it "is right shape" $ do
            s <- initSolution3D n
            qdist' s `shouldSatisfy` has2DShape (n,n)        

    describe "gradients" $ do
        it "is right shape" $ do
            s <- initState3D n
            gradients testNeighbourProbs s `shouldSatisfy` has2DShape (n,3)
