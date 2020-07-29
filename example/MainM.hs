module Main where

import System.Environment(getArgs)
import System.Exit
import Control.Monad(when)
import Data.Default(def)
import Pipes
import Data.Algorithm.TSNE

import qualified Data.Massiv.Array as MA

main :: IO ()
main = do
    putStrLn "Haskell tSNE example"

    args <- getArgs
    let argc = length args
    when (argc < 1 || argc > 2) $ do
        putStrLn "Usage haskell_tsne_example {input file name} [{num input values}]"
        exitFailure

    let inputFileName = head args
    inputData <- readDataFile inputFileName

    let n = length inputData
    putStrLn $ "total input: " ++ show n

    let n' = if (argc < 2) then n else (read (args !! 1)) 

    putStrLn $ "using: " ++ show n'

    inputMatrix <- MA.fromListsM MA.Seq $ take n' inputData
    forTsne3D_M outputResult def Nothing inputMatrix
    
    --runEffect $ for (tsne3D def $ take n' inputData) $ \r -> do
    --    lift $ outputResult r

readDataFile :: FilePath -> IO [[Double]]
readDataFile f = do
    d <- readFile f
    return $ map read (lines d)

outputResult :: TSNEOutput3D -> IO ()
outputResult s = do
    putStrLn $ "iteration: " ++ (show.tsneIteration3D) s
    putStrLn $ "cost: " ++ (show.tsneCost3D) s

