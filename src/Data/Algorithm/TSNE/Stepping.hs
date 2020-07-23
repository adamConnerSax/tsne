module Data.Algorithm.TSNE.Stepping where

import Control.Applicative
import Control.Exception (assert)
import Data.List(zipWith4)

import Data.Algorithm.TSNE.Types
import Data.Algorithm.TSNE.Utils


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

