{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Algorithm.TSNE.Stepping where

import Control.Applicative
import Control.Exception (assert)
import Data.List(zipWith4)

import Data.Algorithm.TSNE.Types
import Data.Algorithm.TSNE.Utils

import qualified Data.Massiv.Array as MA

import Data.Coerce (coerce)
import Data.Foldable as F (toList, length)
import qualified Data.List as L (uncons)

stepTSNE :: TSNEOptions -> TSNEInput -> [[Probability]] -> TSNEState -> TSNEState
stepTSNE opts vs ps st = TSNEState i' s'' g' d'
    where
        i = stIteration st
        s = stSolution st
        g = stGains st
        d = stDeltas st
        gr = gradients ps st
        i' = i + 1
        s' = recenter $ z (+) s d'
        g' = z3 newGain g d gr
        d' = z3 (newDelta (tsneLearningRate opts) i) g' d gr
        z = zipWith.zipWith
        z3 = zipWith3.zipWith3
        s'' = assert (length s' == length vs) s'

newGain :: Gain -> Delta -> Gradient -> Gain
newGain g d gr = max 0.01 g'
    where
        g' = if signum d == signum gr 
                then g * 0.8
                else g + 0.2  

newDelta :: Double -> Int -> Gain -> Delta -> Gradient -> Delta
newDelta e i g' d gr = (m * d) - (e * g' * gr)
    where
        m = if i < 250 then 0.5 else 0.8

gradients :: [[Probability]] -> TSNEState -> [[Gradient]]
gradients pss st = gradient <$> ss
    where
        gradient :: [Double] -> [Gradient]
        gradient s = zipWith4 (f s) s pss qss qss'
        ss = stSolution st
        i = stIteration st
        qss = qdist ss
        qss' = qdist' ss 
        f :: [Double] -> Double -> [Double] -> [Double] -> [Double] -> Gradient
        f s x ps qs qs' = sum $ zipWith4 g s ps qs qs'
            where
                g y p q q' = m * (x - y)
                    where
                        m = 4 * (k * p - q') * q
                        k = if i < 100 then 4 else 1

cost :: [[Double]] -> TSNEState -> Double
cost pss st = sumsum $ (zipWith.zipWith) c pss (qdist' (stSolution st))
    where
        c p q = -p * log q 

-- massiv versions

stepTSNE_M :: MA.MonadThrow m => TSNEOptions -> TSNEInputM -> MA.Matrix MA.U Probability -> TSNEStateM -> m TSNEStateM
stepTSNE_M opts vs ps st =  do
  let i = stIterationM st
      s = stSolutionM st
      g = stGainsM st
      d = stDeltasM st
{-
  MA.liftIO $ putStrLn $ "size of input=" ++ show (MA.size ps)
  MA.liftIO $ putStrLn $ "size of current solution=" ++ show (MA.size s)
  MA.liftIO $ putStrLn $ "size of gains=" ++ show (MA.size g)
  MA.liftIO $ putStrLn $ "size of delta=" ++ show (MA.size d)
-}
  gr <- gradientsM ps st
--  MA.liftIO $ putStrLn $ "size of gradient=" ++ show (MA.size gr)    
  let i' = i + 1
      g' = MA.computeAs MA.U $ MA.zipWith3 newGain g d gr
      d' = MA.computeAs MA.U $ MA.zipWith3 (newDelta (tsneLearningRate opts) i) g' d gr
--  MA.liftIO $ putStrLn $ "size of g'=" ++ show (MA.size g')
--  MA.liftIO $ putStrLn $ "size of d'=" ++ show (MA.size d')    
      s' = MA.computeAs MA.U $ recenterM $ MA.zipWith (+) s d'
--  MA.liftIO $ putStrLn $ "size of s' = recentered s+d'=" ++ show (MA.size s')    
  return $ TSNEStateM i' s' g' d'

gradientsM :: MA.MonadThrow m => MA.Matrix MA.U Probability -> TSNEStateM -> m (MA.Matrix MA.U Gradient)
gradientsM pss st = fromOuterSlices $ MA.map gradient ssV
    where
        ss = stSolutionM st
        ssV :: MA.Vector MA.D (MA.Vector MA.M Double) = asVectorsM ss
        pssV  = asVectorsM pss
--        gradient :: MA.Vector MA.U Double -> MA.Vector MA.U Gradient
        gradient s = zipWith4M (f s) s pssV qssV qssV'
        MA.Sz2 _ cols = MA.size ss
        i = stIterationM st -- Int
        qssV  :: MA.Vector MA.D (MA.Vector MA.M Double) = asVectorsM $ qdistM ss -- MA.Matrix MA.D Double 
        qssV' :: MA.Vector MA.D (MA.Vector MA.M Double) = asVectorsM $ MA.computeAs MA.U $ qdistM' ss -- MA.Matrix MA.D Double 
        f s x ps qs qs' = MA.sum $ zipWith4M g s ps qs qs'
            where
                g y p q q' = m * (x - y)
                    where
                        m = 4 * (k * p - q') * q
                        k = if i < 100 then 4 else 1
{-# INLINEABLE gradientsM #-}


costM :: MA.Source r MA.Ix2 Double => MA.Matrix r Double -> TSNEStateM -> Double
costM pss st = MA.sum $ MA.zipWith c pss (qdistM' (stSolutionM st))
  where
    c p q = -p * log q
{-# INLINEABLE costM #-}
