module Main where

import System.Environment(getArgs)
import System.Exit
import Control.Monad(when)
import Data.Default(def)
import Data.Time
import Data.Algorithm.TSNE



main :: IO ()
main = do
    putStrLn "Haskell tSNE Timing"

    args <- getArgs
    let argc = length args
    when (argc < 1 || argc > 2) $ do
        putStrLn "Usage haskell_tsne_timing {input file name} [{num input values}]"
        exitFailure

    let inputFileName = head args
    inputData <- readDataFile inputFileName

    let n = length inputData
    putStrLn $ "total input: " ++ show n

    let n' = if (argc < 2) then n else (read (args !! 1)) 

    putStrLn $ "using: " ++ show n'

    t0 <- getCurrentTime
    forTsne3D (outputResult t0) def $ take n' inputData
    
readDataFile :: FilePath -> IO [[Double]]
readDataFile f = do
    d <- readFile f
    return $ map read (lines d)

outputResult :: UTCTime -> TSNEOutput3D -> IO ()
outputResult t0 s = do
    let i = tsneIteration3D s

    when (i > 0 && (i == 1 || i `mod` 100 == 0)) $ do 
        putStrLn $ "iteration: " ++ show i
        t <- getCurrentTime
        let dt = diffUTCTime t t0
            ms = round $ 1000.0 * realToFrac dt / realToFrac i
        putStrLn $ "  elapsed time: " ++ show dt
        putStrLn $ "  average iteration time: " ++ show ms ++ "ms"

