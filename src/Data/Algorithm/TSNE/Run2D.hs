module Data.Algorithm.TSNE.Run2D where

import Control.Applicative
import Control.DeepSeq
import Data.Random.Normal (normalsIO', normal')
import qualified System.Random as Random 
import Pipes

import Data.Algorithm.TSNE.Types
import Data.Algorithm.TSNE.Preparation
import Data.Algorithm.TSNE.Stepping

import qualified Data.Massiv.Array as MA


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

-- massiv versions

initState2D_M :: Maybe Int -> Int -> IO TSNEStateM
initState2D_M seedM n = do
    s <- initSolution2D_M seedM n
    return
      $ TSNEStateM
      0
      s
      (MA.compute $ MA.replicate MA.Seq (MA.Sz2 2 n) 1)
      (MA.compute $ MA.replicate MA.Seq (MA.Sz2 2 n) 0)
{-# INLINEABLE initState2D_M #-}

-- we add the ability to specify the seed here so we can get deterministic output
initSolution2D_M :: Maybe Int -> Int -> IO (MA.Matrix MA.U Double)
initSolution2D_M seedM n = do
  g <- case seedM of
    Nothing -> Random.getStdGen
    Just s -> return $ Random.mkStdGen s
  return $ snd $ MA.randomArrayS g (MA.Sz2 2 n) (normal' (0, 1e-4))
{-# INLINEABLE initSolution2D_M #-}

runTSNE2D_M :: TSNEOptions
            -> TSNEInputM
            -> MA.Matrix MA.U Probability
            -> TSNEStateM
            -> Producer TSNEOutput2D IO ()
runTSNE2D_M opts vs ps st = do
    yield $ output2D_M ps st
    st' <- force <$> stepTSNE_M opts vs ps st
    runTSNE2D_M opts vs ps st'
{-# INLINEABLE runTSNE2D_M #-}

solution2D_M :: MA.Matrix MA.U Double -> [Position2D]
solution2D_M = solution2D . MA.toLists2
{-# INLINEABLE solution2D_M #-}

output2D_M :: MA.Matrix MA.U Double -> TSNEStateM -> TSNEOutput2D
output2D_M pss st = TSNEOutput2D i s c
    where
        i = stIterationM st
        s = (solution2D_M . stSolutionM) st
        c = costM pss st
{-# INLINEABLE output2D_M #-}
