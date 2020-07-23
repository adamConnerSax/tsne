module Data.Algorithm.TSNE.Checks where

import Data.Algorithm.TSNE.Types

isSquare :: Int -> [[a]] -> Bool
isSquare 0 [] = True
isSquare _ [] = False
isSquare n xss = length xss == n && all (\xs -> length xs == n) xss

has2DShape :: (Int, Int) -> [[a]] -> Bool
has2DShape (w,h) xss = length xss == h && all (\xs -> length xs == w) xss

shape2D :: [[a]] -> (Int, Int)
shape2D [] = (undefined, 0)
shape2D xss = (length (head xss), length xss)

isRectangular :: [[a]] -> Bool
isRectangular xss = has2DShape (shape2D xss) xss

inputSize :: TSNEInput -> Int
inputSize = length

inputValueSize :: TSNEInput -> Int
inputValueSize i = w 
    where (w,h) = shape2D i 

inputIsValid :: TSNEInput -> Either String ()
inputIsValid [] = Left "empty input data"
inputIsValid xss
    | not (isRectangular xss) = Left "input data values are not all the same length"
    | otherwise = Right () 

isValidStateForInput :: Int -> TSNEInput -> TSNEState -> Either String ()
isValidStateForInput d i st
    | not (has2DShape (n,d) s) = Left $ "solution is wrong shape: " ++ show (shape2D s) 
    | otherwise = Right ()
        where
            n = inputSize i
            s = stSolution st  

