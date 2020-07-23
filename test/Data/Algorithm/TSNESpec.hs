module Data.Algorithm.TSNESpec (main, spec) where

import Test.Hspec
import Data.Default (def)
import Data.Algorithm.TSNE

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "tsne" $ do
    it "passed nothing returns nothing" $ do
        pending
      --tsne def [[]] `shouldReturn` []