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

neighbourProbabilitiesM :: TSNEOptions -> TSNEInputM -> MA.Matrix MA.U Probability
neighbourProbabilitiesM opts vs = symmetrizeSqM $ rawNeighbourProbabilitiesM opts vs
{-# INLINEABLE neighbourProbabilitiesM #-}


rawNeighbourProbabilitiesM :: TSNEOptions -> TSNEInputM -> MA.Matrix MA.D Probability
rawNeighbourProbabilitiesM opts vs = MA.expandWithin @_ @_ @MA.N MA.Dim1 (MA.Sz1 inputRows) (\r j -> r MA.! j)
                                     $ MA.makeArray MA.Par(MA.Sz1 inputRows) $ \r -> np (vs MA.!> r)
    where
        np :: MA.Vector MA.M Double -> MA.Vector MA.U Probability
        np a = aps (beta a) vs a
        beta a = betaValue $ binarySearchBetaM opts vs a
        MA.Sz2 inputRows inputCols = MA.size vs
        aps :: Double -> TSNEInputM -> MA.Vector MA.M Double -> MA.Vector MA.U Probability
        aps beta bs a = MA.makeArray MA.Seq (MA.Sz1 inputRows) (\r -> pj' (vs MA.!> r))
          where
            pj :: MA.Vector MA.M Double -> Double
            pj b
              | a == b = 0
              | otherwise = exp $ -(distanceSquaredM (MA.delay a) (MA.delay b)) * beta
            psum :: Double  
            psum = Monoid.getSum $ MA.foldOuterSlice (\r -> Monoid.Sum $ pj r) bs
            pj' :: MA.Vector MA.M Double -> Double
            pj' b = pj b / psum
{-# INLINEABLE rawNeighbourProbabilitiesM #-}

binarySearchBetaM :: TSNEOptions -> TSNEInputM -> MA.Vector MA.M Double -> Beta
binarySearchBetaM opts vs = binarySearchBetaM' opts vs 1e-4 0 (Beta 1 (-infinity) infinity)
{-# INLINEABLE binarySearchBetaM #-}

binarySearchBetaM' :: TSNEOptions -> TSNEInputM -> Double -> Int -> Beta -> MA.Vector MA.M Double -> Beta
binarySearchBetaM' opts bs tol i beta a
    | i == 50            = beta
    | abs (e - t) < tol  = beta
    | e > t              = r $ incPrecision beta
    | otherwise          = r $ decPrecision beta 
        where
            t = targetEntropy opts
            e = entropyForInputValueM (betaValue beta) bs a
            incPrecision (Beta b _ bmax) 
                | bmax == infinity = Beta (b * 2) b bmax
                | otherwise        = Beta ((b + bmax) / 2) b bmax
            decPrecision (Beta b bmin _) 
                | bmin == -infinity = Beta (b / 2) bmin b
                | otherwise         = Beta ((b + bmin) / 2) bmin b
            r beta' = binarySearchBetaM' opts bs tol (i+1) beta' a 
{-# INLINEABLE binarySearchBetaM' #-}

entropyForInputValueM :: Double -> TSNEInputM -> MA.Vector MA.M Double -> Entropy
entropyForInputValueM beta bs a = Monoid.getSum $ MA.foldOuterSlice (\r -> Monoid.Sum $ h r) bs
  where
    pj :: MA.Vector MA.M Double -> Double
    pj b
      | (MA.toManifest a) == b = 0
      | otherwise = exp $ -(distanceSquaredM (MA.delay a) (MA.delay b)) * beta
    psum :: Double  
    psum = Monoid.getSum $ MA.foldOuterSlice (\r -> Monoid.Sum $ pj r) bs
    pj' :: MA.Vector MA.M Double -> Double
    pj' b = pj b / psum
    h :: MA.Vector MA.M Double -> Double
    h b = let x = pj' b in if x > 1e-7 then -x * log x else 0
{-# INLINEABLE entropyForInputValueM #-}

