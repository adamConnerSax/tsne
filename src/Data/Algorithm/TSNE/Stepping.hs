{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Algorithm.TSNE.Stepping where

import Control.Applicative
import Control.Exception (assert)
import Data.List(zipWith4)

import Data.Algorithm.TSNE.Types
import Data.Algorithm.TSNE.Utils

import qualified Data.Massiv.Array as MA
import Data.Massiv.Array.Delayed.Pull (D(DArray))
import Data.Coerce (coerce)

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
{-
gradientsM :: MA.Matrix r Probability -> TSNEStateM -> MA.Matrix r Gradient
gradientsM pss st = gradient <$> ss
    where
        gradient :: MA.Vector MA.U Double -> MA.Vector U Gradient
        gradient s = zipWith4 (f s) s pss qss qss'
        ss = stSolution st
        i = stIteration st
        qss = qdist ss
        qss' = qdist' ss 
        f :: MA.Vector MA.U Double
          -> Double
          -> MA.Vector MA.U Double
          -> MA.Vector MA.U Double
          -> MA.Vector MA.U Double
          -> Gradient
        f s x ps qs qs' = sum $ zipWith4 g s ps qs qs'
            where
                g y p q q' = m * (x - y)
                    where
                        m = 4 * (k * p - q') * q
                        k = if i < 100 then 4 else 1
{-# INLINEABLE gradientsM #-}
-}

izipWith4
  :: (MA.Source r1 ix e1, MA.Source r2 ix e2, MA.Source r3 ix e3, MA.Source r4 ix e4)
  => (ix -> e1 -> e2 -> e3 -> e4 -> e)
  -> MA.Array r1 ix e1
  -> MA.Array r2 ix e2
  -> MA.Array r3 ix e3
  -> MA.Array r4 ix e4
  -> MA.Array MA.D ix e
izipWith4 f arr1 arr2 arr3 arr4 =
  MA.DArray
    (MA.getComp arr1 <> MA.getComp arr2 <> MA.getComp arr3 <> MA.getComp arr4)
    (MA.SafeSz
       (MA.liftIndex2
          min
          (MA.liftIndex2 min (coerce (MA.size arr1)) (coerce (MA.size arr2)))
          (MA.liftIndex2 min (coerce (MA.size arr3)) (coerce (MA.size arr4))))) $ \ !ix ->
    f ix (MA.unsafeIndex arr1 ix) (MA.unsafeIndex arr2 ix) (MA.unsafeIndex arr3 ix) (MA.unsafeIndex arr4 ix)
{-# INLINE izipWith4 #-}

costM :: MA.Source r MA.Ix2 Double => MA.Matrix r Double -> TSNEStateM -> Double
costM pss st = MA.sum $ MA.zipWith c pss (qdistM' (stSolutionM st))
  where
    c p q = -p * log q
{-# INLINEABLE costM #-}
