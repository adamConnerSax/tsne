module Data.Algorithm.TSNE.TestMisc where

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True
