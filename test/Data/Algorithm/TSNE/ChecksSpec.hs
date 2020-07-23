module Data.Algorithm.TSNE.ChecksSpec (main, spec) where

import Test.Hspec
import Data.Algorithm.TSNE.Checks

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

u = undefined

spec :: Spec
spec = do
    describe "isSquare" $ do
        it "empty is 0x0 square" $ do
            isSquare 0 [] `shouldBe` True
        it "empty is not 1x1 square" $ do
            isSquare 1 [] `shouldBe` False
        it "single is 1x1 square" $ do
            isSquare 1 [[u]] `shouldBe` True
        it "single is not 0x0 square" $ do
            isSquare 0 [[u]] `shouldBe` False
        it "2x2 square" $ do
            isSquare 2 [[u,u],[u,u]] `shouldBe` True
        it "2x2 is not 0x0" $ do
            isSquare 0 [[u,u],[u,u]] `shouldBe` False
        it "2x2 is not 0x0" $ do
            isSquare 1 [[u,u],[u,u]] `shouldBe` False
        it "2x2 is not 3x3" $ do
            isSquare 3 [[u,u],[u,u]] `shouldBe` False
        it "2x1 is not 1x1" $ do
            isSquare 1 [[u,u]] `shouldBe` False
        it "2x1 is not 2x2" $ do
            isSquare 2 [[u,u]] `shouldBe` False
        it "1x2 is not 1x1" $ do
            isSquare 1 [[u,u]] `shouldBe` False
        it "1x2 is not 2x2" $ do
            isSquare 2 [[u,u]] `shouldBe` False

    describe "has2DShape" $ do
        it "empty is 0x0" $ do
            has2DShape (0,0) [] `shouldBe` True
        --it "empty is not 1x0" $ do
        --    has2DShape (1,0) [] `shouldBe` False
        it "empty is not 0x1" $ do
            has2DShape (0,1) [] `shouldBe` False
        it "1x2 is 1x2" $ do
            has2DShape (1,2) [[u],[u]] `shouldBe` True
        it "2x1 is 2x1" $ do
            has2DShape (2,1) [[u,u]] `shouldBe` True
        it "1x2 is not 2x1" $ do
            has2DShape (2,1) [[u],[u]] `shouldBe` False
        it "2x1 is not 1x2" $ do
            has2DShape (1,2) [[u,u]] `shouldBe` False
