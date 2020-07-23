module Data.Algorithm.TSNE ( 
        TSNEOptions(..),
        tsne3D,
        forTsne3D,
        TSNEOutput3D(..),
        tsne2D,
        forTsne2D,
        TSNEOutput2D(..)
    ) where

import Pipes

import Data.Algorithm.TSNE.Types
import Data.Algorithm.TSNE.Utils
import Data.Algorithm.TSNE.Preparation
import Data.Algorithm.TSNE.Run3D
import Data.Algorithm.TSNE.Run2D


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
