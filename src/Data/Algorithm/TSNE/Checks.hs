{-# LANGUAGE FlexibleContexts #-}
module Data.Algorithm.TSNE.Checks where

import Data.Algorithm.TSNE.Types

import qualified Data.Massiv.Array as MA

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

-- Massiv versions
isSquareM :: MA.Load r MA.Ix2 a => Int -> MA.Matrix r a -> Bool
isSquareM n mat = let (MA.Sz2 r c) = MA.size mat in (r == c) && (r == n)
{-# INLINEABLE isSquareM #-}

has2DShapeM :: MA.Load r MA.Ix2 a => (Int, Int) -> MA.Matrix r a -> Bool
has2DShapeM (w, h) mat = let (MA.Sz2 r c) = MA.size mat in (h == r) && (w == c)
{-# INLINEABLE has2DShapeM #-}

-- This is flipped from row, col for some reason
shape2D_M :: MA.Load r MA.Ix2 a => MA.Matrix r a -> (Int, Int)
shape2D_M mat = let (MA.Sz2 r c) = MA.size mat in (c, r)
{-# INLINEABLE shape2D_M #-}

-- isRectangular makes no sense for Matrix.  They are always rectangular

inputSizeM :: TSNEInputM -> Int
inputSizeM i = let (MA.Sz2 r _) = MA.size i in r
{-# INLINEABLE inputSizeM #-}

inputValueSizeM :: TSNEInputM -> Int
inputValueSizeM i = let (MA.Sz2 _ c) = MA.size i in c
{-# INLINEABLE inputValueSizeM #-}

inputIsValidM :: TSNEInputM -> Either String ()
inputIsValidM i = if (MA.isEmpty i) then Left "empty input data" else Right ()
{-# INLINEABLE inputIsValidM #-}

-- stSolution is transposed for some reason.  I think?
isValidStateForInputM :: Int -> TSNEInputM -> TSNEStateM -> Either String ()
isValidStateForInputM d i st
  | not (has2DShapeM (n, d) s) = Left $ "solution is wrong shape: should be (" ++ show n ++ "," ++ show d ++ "), but is " ++ show (shape2D_M s)
  | otherwise = Right ()
  where
    n = inputSizeM i
    s = stSolutionM st
{-# INLINEABLE isValidStateForInputM #-}

