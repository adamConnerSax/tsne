module Data.Algorithm.TSNE.Run2D where

import Control.Applicative
import Control.DeepSeq
import Data.Random.Normal (normalsIO')
import Pipes

import Data.Algorithm.TSNE.Types
import Data.Algorithm.TSNE.Preparation
import Data.Algorithm.TSNE.Stepping


initState2D :: Int -> IO TSNEState
initState2D n = do
    s <- initSolution2D n
    return $ TSNEState 0 s (rr 1) (rr 0)
        where
            rr = repeat.repeat

initSolution2D :: Int -> IO [[Double]]
initSolution2D n = do
    let ns = normalsIO' (0, 1e-4)
    xs <- ns
    ys <- ns
    return $ take n <$> [xs,ys]

runTSNE2D :: TSNEOptions -> TSNEInput -> [[Probability]] -> TSNEState -> Producer TSNEOutput2D IO ()
runTSNE2D opts vs ps st = do
    yield $ output2D ps st
    let st' = force $ stepTSNE opts vs ps st
    runTSNE2D opts vs ps st'

solution2D :: [[Double]] -> [Position2D]
solution2D (xs:ys:_) = zip xs ys

output2D :: [[Double]] -> TSNEState -> TSNEOutput2D
output2D pss st = TSNEOutput2D i s c
    where
        i = stIteration st
        s = (solution2D . stSolution) st
        c = cost pss st

