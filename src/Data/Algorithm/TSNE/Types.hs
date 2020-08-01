{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleContexts #-}

module Data.Algorithm.TSNE.Types where

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Default
import qualified Data.Massiv.Core as MA
import qualified Data.Massiv.Array as MA

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


-- Massiv Versions

--type Position2Dm = V.Vector U Double
--type Position3Dm = V.Vector U Double

type TSNEInputValueM =  MA.Vector MA.U Double
type TSNEInputM =  MA.Matrix MA.U Double


data TSNEOutput3D_M = TSNEOutput3D_M {
    tsneIteration3D_M :: Int,
    tsneSolution3_M :: MA.Vector MA.U Position3D,
    tsneCost3D_M :: Double
} deriving (Show, Eq)


data TSNEOutput2D_M = TSNEOutput2D_M {
    tsneIteration2D_M :: Int,
    tsneSolution2D_M :: MA.Vector MA.U Position2D,
    tsneCost2D_M :: Double
} deriving (Show, Eq)


data TSNEStateM = TSNEStateM {
    stIterationM :: !Int,
    stSolutionM :: !(MA.Matrix MA.U Double), -- dimension (solution) x length (inputs)
    stGainsM :: !(MA.Matrix MA.U Gain), -- dimension (solution) x length (inputs)
    stDeltasM :: !(MA.Matrix MA.U Delta) -- dimension (solution) x length (inputs)
} deriving (Show, Generic, NFData)


solutionToList :: MA.Source r MA.Ix1 a => MA.Vector r a -> [a]
solutionToList = MA.toList
