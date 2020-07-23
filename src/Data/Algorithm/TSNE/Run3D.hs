module Data.Algorithm.TSNE.Run3D where

import Control.Applicative
import Control.DeepSeq
import Data.Random.Normal (normalsIO')
import Pipes

import Data.Algorithm.TSNE.Types
import Data.Algorithm.TSNE.Preparation
import Data.Algorithm.TSNE.Stepping


initState3D :: Int -> IO TSNEState
initState3D n = do
    s <- initSolution3D n
    return $ TSNEState 0 s (rr 1) (rr 0)
        where
            rr = repeat.repeat

initSolution3D :: Int -> IO [[Double]]
initSolution3D n = do
    let ns = normalsIO' (0, 1e-4)
    xs <- ns
    ys <- ns
    zs <- ns
    return $ take n <$> [xs,ys,zs]

runTSNE3D :: TSNEOptions -> TSNEInput -> [[Probability]] -> TSNEState -> Producer TSNEOutput3D IO ()
runTSNE3D opts vs ps st = do
    yield $ output3D ps st
    let st' = force $ stepTSNE opts vs ps st
    runTSNE3D opts vs ps st'

solution3D :: [[Double]] -> [Position3D]
solution3D (xs:ys:zs:_) = zip3 xs ys zs

output3D :: [[Double]] -> TSNEState -> TSNEOutput3D
output3D pss st = TSNEOutput3D i s c
    where
        i = stIteration st
        s = (solution3D . stSolution) st
        c = cost pss st

