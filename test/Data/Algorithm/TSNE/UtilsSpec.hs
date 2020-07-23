module Data.Algorithm.TSNE.UtilsSpec (main, spec) where

import Test.Hspec
import Data.Algorithm.TSNE.Utils

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

u = undefined

spec :: Spec
spec = do
    describe "infinity" $ do
        it "greater than 1e99" $ do
            infinity `shouldSatisfy` (> 1e99)

    describe "symmetricalMatrixFromTopRight" $ do
        it "passed nothing returns nothing" $ do
            symmetricalMatrixFromTopRight [] `shouldBe` ([] :: [[Double]])
        it "passed one value returns one value" $ do
            symmetricalMatrixFromTopRight [[42]] `shouldBe` ([[42]] :: [[Double]])
        it "passed 2x2 top right" $ do
            symmetricalMatrixFromTopRight [["a1", "a2"], ["b2"]] `shouldBe` [["a1", "a2"], ["a2", "b2"]] 
        it "passed 3x3 top right" $ do
            symmetricalMatrixFromTopRight [["a1", "a2", "a3"], ["b2", "b3"], ["c3"]] `shouldBe` [["a1", "a2", "a3"], ["a2", "b2", "b3"], ["a3", "b3", "c3"]] 

    describe "qdist" $ do
        it "passed nothing return nothing" $ do
            pending
            --qdist [] `shouldBe` ([] :: [[Double]])

