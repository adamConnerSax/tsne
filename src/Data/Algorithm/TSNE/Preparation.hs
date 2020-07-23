module Data.Algorithm.TSNE.Preparation where

import Data.Algorithm.TSNE.Types
import Data.Algorithm.TSNE.Utils


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


