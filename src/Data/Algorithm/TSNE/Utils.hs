{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Algorithm.TSNE.Utils where

import Data.List(foldr, transpose)
import qualified Data.Massiv.Core as MA
import qualified Data.Massiv.Array as MA
import qualified Data.Massiv.Array.Numeric as MA
import qualified Data.Massiv.Core.Operations as MA
import qualified Data.Massiv.Array.Stencil as MA
import qualified Data.Massiv.Vector as MA
import qualified Data.Massiv.Array.Unsafe as MA

import Data.Foldable as F (toList, length)
import qualified Data.List as L (uncons)
import Data.Proxy (Proxy(..))

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


-- Massiv versions
distanceSquaredM :: (MA.Source r MA.Ix1 Double
                    , MA.Source r' MA.Ix1 Double
                    )

                 => MA.Vector r Double -> MA.Vector r' Double -> Double
distanceSquaredM as bs = MA.sum $ MA.zipWith (\a b -> let x = a - b in x * x) as bs
{-# INLINEABLE distanceSquaredM #-}

symmetrizeSqM :: (MA.Source r MA.Ix2 Double)
              => MA.Matrix r Double -> MA.Matrix MA.D Double
symmetrizeSqM m = MA.zipWith f m (MA.transpose m) where
  MA.Sz2 r _ = MA.size m
  f :: Double -> Double -> Double
  f x y = max a 1e-100
    where a = (x + y) / (2 * realToFrac r)
{-# INLINEABLE symmetrizeSqM #-}

recenterM :: MA.Manifest r MA.Ix2 Double => MA.Matrix r Double -> MA.Matrix MA.D Double
recenterM m = MA.imap (\(i MA.:. _) e -> e - (meansV MA.! i)) m
    where
      meansV = MA.computeAs MA.U $ MA.map (/ fromIntegral c) $ MA.foldlInner (+) 0 m -- fold over columns
      MA.Sz2 r c = MA.size m
{-# INLINEABLE recenterM #-}

-- compute upper triangle and then we symmetrize
-- (i, j) element is distance between ith and jth row
qdistM :: MA.Matrix MA.U Double -> MA.Matrix MA.U Double
qdistM ss =
  let MA.Sz2 c r = MA.size ss
      dist (i MA.:. j) =
        let s = distanceSquaredM (ss MA.<! i) (ss MA.<!  j)
        in 1 / (1 + s)
  in MA.computeAs MA.U $ symmetric MA.Seq (MA.Sz1 r) dist
{-# INLINEABLE qdistM #-}

symmetric :: Num e => MA.Comp -> MA.Sz1 -> (MA.Ix2 -> e) -> MA.Matrix MA.DL e
symmetric comp (MA.Sz1 n) f =
  let sz = MA.Sz2 n n
   in MA.makeLoadArray comp sz 0 $ \_scheduler wr ->
        MA.forM_ (0 MA...: n) $ \i ->
          MA.forM_ ((i + 1) MA...: n) $ \j ->
            let ix = i MA.:. j
                e = f ix
             in wr ix e >> wr (j MA.:. i) e
{-# INLINE symmetric #-}


qdistM' :: MA.Matrix MA.U Double -> MA.Matrix MA.D Double
qdistM' = qdistM'' . qdistM
{-# INLINEABLE qdistM' #-}

qdistM'' :: MA.Matrix MA.U Double -> MA.Matrix MA.D Double
qdistM'' qd =
  let sumQD = MA.sum qd
      f q = max (q / sumQD) 1e-100
  in MA.map f qd
{-# INLINEABLE qdistM'' #-}

-- zipWith4M :: (MA.Index ix
--              , MA.Source r1 ix e1
--              , MA.Source r2 ix e2
--              , MA.Source r3 ix e3
--              , MA.Source r4 ix e4
--              )
--           => (e1 -> e2 -> e3 -> e4 -> e)
--          -> MA.Array r1 ix e1
--          -> MA.Array r2 ix e2
--          -> MA.Array r3 ix e3
--          -> MA.Array r4 ix e4
--          -> MA.Array MA.D ix e
-- zipWith4M f a1 a2 a3 a4 = MA.zipWith (\(e1, e2) (e3, e4) ->  f e1 e2 e3 e4) (MA.zip a1 a2) (MA.zip a3 a4)
-- {-# INLINEABLE zipWith4M #-}

-- asVectorsM :: MA.OuterSlice r MA.Ix2 e
--            => MA.Matrix r e -> MA.Vector MA.D (MA.Vector (MA.R r) e)
-- asVectorsM m =
--   let MA.Sz2 r c = MA.size m
--   in MA.makeArray MA.Seq (MA.Sz1 r) $ \r -> (m MA.!> r)
-- {-# INLINEABLE asVectorsM #-}

-- The issue here is that the vectors might not all be the same size.  So we give a size and raise and exception (??)
-- if we're wrong
-- asMatrixM ::
--   forall e r r'
--   .(MA.Manifest r MA.Ix1 (MA.Array r' MA.Ix1 e)
--    , MA.OuterSlice r' MA.Ix1 e
--    ) => MA.Vector r (MA.Vector r' e) -> MA.Matrix MA.U e
asMatrixM ::
     ( MA.Mutable r1 MA.Ix2 e
     , MA.Source r2 Int (MA.Array r3 Int e)
     , MA.Source r3 Int e
     )
  => MA.Array r2 MA.Ix1 (MA.Array r3 MA.Ix1 e)
  -> MA.Array r1 MA.Ix2 e
asMatrixM vs =
  MA.createArrayST_ sz $ \marr ->
    MA.iforM_ vs $ \ i v -> do
      MA.iforM_ v $ \ j e -> do
        MA.writeM marr (i MA.:. j) e
  where v0 = MA.evaluate' vs 0
        sz = MA.consSz (MA.size vs) (MA.size v0)
  --MA.expandWithin MA.Dim1 (MA.Sz1 cols) (\v j -> v MA.!> j) vs
{-# INLINEABLE asMatrixM #-}

fromOuterSlices ::
     forall r r' ix e f m.
     ( MA.Construct r ix e
     , MA.Source r' (MA.Lower ix) e
     , MA.Mutable r (MA.Lower ix) e
     , MA.Resize r (MA.Lower ix)
     , Foldable f
     , MA.MonadThrow m
     )
  => f (MA.Array r' (MA.Lower ix) e)
  -> m (MA.Array r ix e)
fromOuterSlices arrsF =
  case L.uncons (F.toList arrsF) of
    Nothing -> pure MA.empty
    Just (a, _) -> do
      arr <- MA.concatM (MA.dimensions (Proxy :: Proxy (MA.Lower ix))) arrsF
      MA.resizeM (MA.consSz (MA.SafeSz (F.length arrsF)) (MA.size a)) $ MA.compute arr
{-# INLINEABLE fromOuterSlices #-}
