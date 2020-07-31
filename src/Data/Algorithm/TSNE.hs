module Data.Algorithm.TSNE (
        TSNEOptions(..),
        tsne3D,
        forTsne3D,
        TSNEOutput3D(..),
        TSNEOutput3D_M(..),
        tsne2D,
        forTsne2D,
        TSNEOutput2D(..),
        tsne3D_M,
        forTsne3D_M,
        tsne2D_M,
        forTsne2D_M
    ) where

import Pipes

import Data.Algorithm.TSNE.Types
import Data.Algorithm.TSNE.Utils
import Data.Algorithm.TSNE.Preparation
import Data.Algorithm.TSNE.Run3D
import Data.Algorithm.TSNE.Run2D

import qualified Data.Massiv.Array as MA


-- | Generates an infinite stream of 3D tSNE iterations.
tsne3D :: TSNEOptions -> TSNEInput -> Producer TSNEOutput3D IO ()
tsne3D opts input = do
    st <- liftIO $ initState3D $ length input
    runTSNE3D opts input ps st
        where ps = neighbourProbabilities opts input

-- | Executes an IO action for each iteration of the 3D tSNE algorithm.
forTsne3D :: (TSNEOutput3D -> IO ()) -> TSNEOptions -> TSNEInput -> IO ()
forTsne3D action opts input = do
    runEffect $ for (tsne3D opts input) $ \o -> do
        lift $ action o

-- | Generates an infinite stream of 2D tSNE iterations.
tsne2D :: TSNEOptions -> TSNEInput -> Producer TSNEOutput2D IO ()
tsne2D opts input = do
    st <- liftIO $ initState2D $ length input
    runTSNE2D opts input ps st
        where ps = neighbourProbabilities opts input

-- | Executes an IO action for each iteration of the 2D tSNE algorithm.
forTsne2D :: (TSNEOutput2D -> IO ()) -> TSNEOptions -> TSNEInput -> IO ()
forTsne2D action opts input = do
    runEffect $ for (tsne2D opts input) $ \o -> do
        lift $ action o

-- massiv versions

-- | Generates an infinite stream of 3D tSNE iterations.
tsne3D_M :: TSNEOptions -> Maybe Int -> TSNEInputM -> Producer TSNEOutput3D_M IO ()
tsne3D_M opts seedM input = do
  let MA.Sz2 inputLength _ = MA.size input
  st <- liftIO $ initState3D_M seedM inputLength
  runTSNE3D_M opts input ps st
    where ps = neighbourProbabilitiesM opts input

-- | Executes an IO action for each iteration of the 3D tSNE algorithm.
forTsne3D_M :: (TSNEOutput3D_M -> IO ()) -> TSNEOptions -> Maybe Int -> TSNEInputM -> IO ()
forTsne3D_M action opts seedM input = do
    runEffect $ for (tsne3D_M opts seedM input) $ \o -> do
        lift $ action o

-- | Generates an infinite stream of 2D tSNE iterations.
tsne2D_M :: TSNEOptions -> Maybe Int -> TSNEInputM -> Producer TSNEOutput2D IO ()
tsne2D_M opts seedM input = do
  let MA.Sz2 inputLength _ = MA.size input
  st <- liftIO $ initState2D_M seedM inputLength
  runTSNE2D_M opts input ps st
    where ps = neighbourProbabilitiesM opts input

-- | Executes an IO action for each iteration of the 2D tSNE algorithm.
forTsne2D_M :: (TSNEOutput2D -> IO ()) -> TSNEOptions -> Maybe Int -> TSNEInputM -> IO ()
forTsne2D_M action opts seedM input = do
    runEffect $ for (tsne2D_M opts seedM input) $ \o -> do
        lift $ action o
