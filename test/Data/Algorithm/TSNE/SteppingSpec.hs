module Data.Algorithm.TSNE.SteppingSpec (main, spec) where

import Data.Default (def)

import Test.Hspec
import Data.Algorithm.TSNE.Stepping
import Data.Algorithm.TSNE.Run3D
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

    describe "stepTSNE" $ do
        it "works" $ do 
            s <- initState3D n
            stepTSNE def testInput testNeighbourProbs s `shouldSatisfy` isRight . (isValidStateForInput 3 testInput)
