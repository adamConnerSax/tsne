{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Data.Algorithm.TSNE.Preparation where

import Data.Algorithm.TSNE.Types
import Data.Algorithm.TSNE.Utils

import qualified Data.Massiv.Array as MA
import qualified Data.Monoid as Monoid

targetEntropy :: TSNEOptions -> Entropy
targetEntropy = log.realToFrac.tsnePerplexity

data Beta = Beta {
    betaValue :: Double,
    betaMin :: Double,
    betaMax :: Double
}

neighbourProbabilities :: TSNEOptions -> TSNEInput -> [[Probability]]
neighbourProbabilities opts vs = symmetrize $ rawNeighbourProbabilities opts vs

rawNeighbourProbabilities :: TSNEOptions -> TSNEInput -> [[Probability]]
rawNeighbourProbabilities opts vs = map np vs
    where
        np a = aps (beta a) vs a
        beta a = betaValue $ binarySearchBeta opts vs a

        aps :: Double -> TSNEInput -> TSNEInputValue -> [Probability]
        aps beta bs a = map pj' bs
            where
                psum = sum $ map pj bs
                pj b
                    | a == b    = 0
                    | otherwise = exp $ -(distanceSquared a b) * beta
                pj' b = pj b / psum

binarySearchBeta :: TSNEOptions -> TSNEInput -> TSNEInputValue -> Beta
binarySearchBeta opts vs = binarySearchBeta' opts vs 1e-4 0 (Beta 1 (-infinity) infinity)

binarySearchBeta' :: TSNEOptions -> TSNEInput -> Double -> Int -> Beta -> TSNEInputValue -> Beta
binarySearchBeta' opts bs tol i beta a
    | i == 50            = beta
    | abs (e - t) < tol  = beta
    | e > t              = r $ incPrecision beta
    | otherwise          = r $ decPrecision beta
        where
            t = targetEntropy opts
            e = entropyForInputValue (betaValue beta) bs a
            incPrecision (Beta b _ bmax)
                | bmax == infinity = Beta (b * 2) b bmax
                | otherwise        = Beta ((b + bmax) / 2) b bmax
            decPrecision (Beta b bmin _)
                | bmin == -infinity = Beta (b / 2) bmin b
                | otherwise         = Beta ((b + bmin) / 2) bmin b
            r beta' = binarySearchBeta' opts bs tol (i+1) beta' a

entropyForInputValue :: Double -> TSNEInput -> TSNEInputValue -> Entropy
entropyForInputValue beta bs a = sum $ map h bs
    where
        h b = if x > 1e-7 then -x * log x else 0
            where x = pj' b
        psum = sum $ map pj bs
        pj b
            | a == b    = 0
            | otherwise = exp $ -(distanceSquared a b) * beta
        pj' b = pj b / psum



-- Massiv versions

neighbourProbabilitiesM :: MA.MonadThrow m => TSNEOptions -> TSNEInputM -> m (MA.Matrix MA.U Probability)
neighbourProbabilitiesM opts vs = MA.compute @MA.U . symmetrizeSqM . MA.computeAs MA.U <$> rawNeighbourProbabilitiesM opts vs
{-# INLINEABLE neighbourProbabilitiesM #-}

-- compute all the distances once and then all the rest is much faster.
rawNeighbourProbabilitiesM :: MA.MonadThrow m => TSNEOptions -> TSNEInputM -> m (MA.Matrix MA.DL Probability)
rawNeighbourProbabilitiesM opts vs = MA.stackOuterSlicesM 
                                     $ MA.makeArray @MA.D MA.Seq (MA.Sz1 inputRows) np 
    where
      MA.Sz2 inputRows inputCols = MA.size vs
      distances :: MA.Matrix MA.U Double
      distances = MA.computeAs MA.U $ symmetric MA.Seq (MA.Sz1 inputRows)
                  $ \(r MA.:. c) ->  if r == c
                                     then 0
                                     else distanceSquaredM (vs MA.!> r) (vs MA.!> c) 
      np :: MA.Ix1 -> MA.Vector MA.U Probability
      np r = aps (beta r) r
      beta r = betaValue $ binarySearchBetaM opts distances r
      aps :: Double -> MA.Ix1 -> MA.Vector MA.U Probability
      aps beta r = MA.makeArray MA.Seq (MA.Sz1 inputRows) pj'
        where
          pj :: MA.Ix1 -> Double
          pj r'
            | r' == r = 0
            | otherwise = exp $ -(distances MA.!> r MA.!> r') * beta
          psum :: Double  
          psum = MA.sum $ MA.makeArray @MA.U MA.Seq (MA.Sz1 inputRows) (\r' -> pj r') --Monoid.getSum $ MA.foldOuterSlice (\r -> Monoid.Sum $ pj r) bs
          pj' :: MA.Ix1 -> Double
          pj' r' = pj r' / psum
{-# INLINEABLE rawNeighbourProbabilitiesM #-}

binarySearchBetaM :: TSNEOptions -> MA.Matrix MA.U Double -> MA.Ix1 -> Beta
binarySearchBetaM opts dists = binarySearchBetaM' opts dists 1e-4 0 (Beta 1 (-infinity) infinity)
{-# INLINEABLE binarySearchBetaM #-}

binarySearchBetaM' :: TSNEOptions -> MA.Matrix MA.U Double -> Double -> Int -> Beta -> MA.Ix1 -> Beta
binarySearchBetaM' opts dists tol i beta r
    | i == 50            = beta
    | abs (e - t) < tol  = beta
    | e > t              = f $ incPrecision beta
    | otherwise          = f $ decPrecision beta
        where
            t = targetEntropy opts
            e = entropyForInputValueM (betaValue beta) dists r
            incPrecision (Beta b _ bmax)
                | bmax == infinity = Beta (b * 2) b bmax
                | otherwise        = Beta ((b + bmax) / 2) b bmax
            decPrecision (Beta b bmin _)
                | bmin == -infinity = Beta (b / 2) bmin b
                | otherwise         = Beta ((b + bmin) / 2) bmin b
            f beta' = binarySearchBetaM' opts dists tol (i+1) beta' r
{-# INLINEABLE binarySearchBetaM' #-}

entropyForInputValueM :: Double -> MA.Matrix MA.U Double -> MA.Ix1 -> Entropy
entropyForInputValueM beta dists r = MA.sum $ MA.makeArray @MA.D MA.Seq (MA.Sz1 inputRows) (\r' -> h r')
  where
    MA.Sz2 inputRows _ = MA.size dists
    pj :: MA.Ix1 -> Double
    pj r' 
      | r == r' = 0
      | otherwise = exp $ -(dists MA.!> r MA.!> r') * beta
    psum :: Double
    psum = MA.sum $ MA.makeArray @MA.D MA.Seq (MA.Sz1 inputRows) (\r' -> pj r')
    pj' :: MA.Ix1 -> Double
    pj' r' = pj r' / psum
    h :: MA.Ix1 -> Double
    h r' = let x = pj' r' in if x > 1e-7 then -x * log x else 0
{-# INLINEABLE entropyForInputValueM #-}
