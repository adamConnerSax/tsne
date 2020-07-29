module Data.Algorithm.TSNE.Run3D where

import Control.Applicative
import Control.DeepSeq
import Data.Random.Normal (normalsIO', normal')
import qualified System.Random as Random 
import Pipes

import Data.Algorithm.TSNE.Types
import Data.Algorithm.TSNE.Preparation
import Data.Algorithm.TSNE.Stepping

import qualified Data.Massiv.Array as MA

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


-- massiv versions

initState3D_M :: Maybe Int -> Int -> IO TSNEStateM
initState3D_M seedM n = do
    s <- initSolution3D_M seedM n
    return $ TSNEStateM 0 s (MA.compute $ MA.replicate MA.Seq (MA.Sz2 n 3) 1) (MA.compute $ MA.replicate MA.Seq (MA.Sz2 n 3) 0)

-- we add the ability to specify the seed here so we can get deterministic output
initSolution3D_M :: Maybe Int -> Int -> IO (MA.Matrix MA.U Double)
initSolution3D_M seedM n = do
  g <- case seedM of
    Nothing -> Random.getStdGen
    Just s -> return $ Random.mkStdGen s
  return $ snd $ MA.randomArrayS g (MA.Sz2 3 n) (normal' (0, 1e-4))

runTSNE3D_M :: TSNEOptions
            -> TSNEInputM
            -> MA.Matrix MA.U Probability
            -> TSNEStateM
            -> Producer TSNEOutput3D IO ()
runTSNE3D_M opts vs ps st = do
    yield $ output3D_M ps st
    st' <- force <$> stepTSNE_M opts vs ps st
    runTSNE3D_M opts vs ps st'

solution3D_M :: MA.Matrix MA.U Double -> [Position3D]
solution3D_M = solution3D . MA.toLists2

output3D_M :: MA.Matrix MA.U Double -> TSNEStateM -> TSNEOutput3D
output3D_M pss st = TSNEOutput3D i s c
    where
        i = stIterationM st
        s = (solution3D_M . stSolutionM) st
        c = costM pss st
