{-# LANGUAGE FlexibleContexts #-}
module Data.Algorithm.TSNE.Utils where

import Data.List(foldr, transpose)
import qualified Data.Massiv.Core as MA
import qualified Data.Massiv.Array as MA
import qualified Data.Massiv.Array.Numeric as MA
import qualified Data.Massiv.Core.Operations as MA
import qualified Data.Massiv.Vector as MA

infinity :: Double
infinity = read "Infinity"
 
distanceSquared :: [Double] -> [Double] -> Double
distanceSquared as bs = foldr d 0 (zip as bs)
    where d (a,b) t  = t + (a-b) * (a-b)

symmetrize :: [[Double]] -> [[Double]]
symmetrize m = (zipWith.zipWith) f m (transpose m)
    where 
        f :: Double -> Double -> Double
        f x y = max a 1e-100
            where a = (x + y) / (2 * (realToFrac.length) m) 

recenter :: [[Double]] -> [[Double]]
recenter ss = map r ss
    where 
        r s = subtract (mean s) <$> s
        mean s = sum s / (realToFrac.length) s
         
qdist :: [[Double]] -> [[Double]]
qdist ss = symmetricalMatrixFromTopRight $ qd (transpose ss)
    where
        qd [] = []
        qd ps = [qr ps] ++ qd (tail ps)
        qr :: [[Double]] -> [Double]
        qr ps =  [0::Double] ++ (q (head ps) <$> tail ps)
        q as bs = 1 / (1 + s)
            where
                s = sum $ zipWith f as bs
                f a b = (a-b) * (a-b) 

qdist' :: [[Double]] -> [[Double]]
qdist' ss = (map.map) f qd
    where
        qd = qdist ss
        f :: Double -> Double 
        f q = max (q / sumsum qd) 1e-100
 
sumsum :: [[Double]] -> Double
sumsum m = sum $ sum <$> m 

reprep :: a -> [[a]]
reprep = repeat.repeat

symmetricalMatrixFromTopRight :: [[a]] -> [[a]]
symmetricalMatrixFromTopRight tr = zipWith (++) bl tr
    where
        bl = zipWith take [0..] (transpose m)
        m = zipWith (++) ebl tr
        ebl = zipWith take [0..] (reprep undefined)



--
distanceSquaredM :: (MA.Source r MA.Ix1 Double
                    , MA.MonadThrow m)
                 => MA.Numeric r Double => MA.Vector r Double -> MA.Vector r Double -> m Double
distanceSquaredM as bs = fmap MA.sum $ (MA..*.) as bs
{-# INLINEABLE distanceSquaredM #-}

-- this is now delayed.
symmetrizeSqM :: (MA.Numeric r Double
                 , MA.Source r MA.Ix2 Double)
              => MA.Matrix r Double -> MA.Matrix MA.D Double
symmetrizeSqM m = MA.zipWith f m (MA.transpose m) where
  MA.Sz2 r _ = MA.size m 
  f :: Double -> Double -> Double
  f x y = max a 1e-100
    where a = (x + y) / (2 * realToFrac r) 
{-# INLINEABLE symmetrizeSqM #-}

{-
recenter :: [[Double]] -> [[Double]]
recenter ss = map r ss
    where 
        r s = subtract (mean s) <$> s
        mean s = sum s / (realToFrac.length) s
-}
recenterM :: MA.Matrix r Double -> MA.Matrix MA.D Double
recenterM m = MA.imap f m 
    where
        f :: MA.Ix2 -> Double -> Double
        f ix x = let MA.Sz2 (r MA.:> c) = ix in x - (means !> r)
        means v = fmap (/realToFrac c) $ MA.foldlInner (+) 0 m
        MA.Sz2 _ c = MA.size m
