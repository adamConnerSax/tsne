{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Data.Algorithm.TSNE.Types where

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Default


data TSNEOptions = TSNEOptions {
    tsnePerplexity :: Int,
    tsneLearningRate :: Double
}

type TSNEInputValue =  [Double]
type TSNEInput =  [TSNEInputValue]

type Position3D = (Double,Double,Double)

data TSNEOutput3D = TSNEOutput3D {
    tsneIteration3D :: Int,
    tsneSolution3D :: [Position3D],
    tsneCost3D :: Double
} deriving (Show, Eq)

type Position2D = (Double,Double)

data TSNEOutput2D = TSNEOutput2D {
    tsneIteration2D :: Int,
    tsneSolution2D :: [Position2D],
    tsneCost2D :: Double
} deriving (Show, Eq)

instance Default TSNEOptions where
    def = TSNEOptions 30 10

type Probability = Double
type Gain = Double
type Delta = Double
type Gradient = Double
type Entropy = Double

data TSNEState = TSNEState {
    stIteration :: Int,
    stSolution :: [[Double]],
    stGains :: [[Gain]],
    stDeltas :: [[Delta]]
} deriving (Show, Generic, NFData)