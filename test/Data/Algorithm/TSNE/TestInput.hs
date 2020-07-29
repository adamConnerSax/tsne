module Data.Algorithm.TSNE.TestInput where

import Data.Default (def)

import Data.Algorithm.TSNE.Types
import Data.Algorithm.TSNE.Preparation

import qualified Data.Massiv.Array as MA

-- first 20 digits from the Python sklearn digits dataset
testInput :: TSNEInput
testInput = [
                [0.0,0.0,5.0,13.0,9.0,1.0,0.0,0.0,0.0,0.0,13.0,15.0,10.0,15.0,5.0,0.0,0.0,3.0,15.0,2.0,0.0,11.0,8.0,0.0,0.0,4.0,12.0,0.0,0.0,8.0,8.0,0.0,0.0,5.0,8.0,0.0,0.0,9.0,8.0,0.0,0.0,4.0,11.0,0.0,1.0,12.0,7.0,0.0,0.0,2.0,14.0,5.0,10.0,12.0,0.0,0.0,0.0,0.0,6.0,13.0,10.0,0.0,0.0,0.0],
                [0.0,0.0,0.0,12.0,13.0,5.0,0.0,0.0,0.0,0.0,0.0,11.0,16.0,9.0,0.0,0.0,0.0,0.0,3.0,15.0,16.0,6.0,0.0,0.0,0.0,7.0,15.0,16.0,16.0,2.0,0.0,0.0,0.0,0.0,1.0,16.0,16.0,3.0,0.0,0.0,0.0,0.0,1.0,16.0,16.0,6.0,0.0,0.0,0.0,0.0,1.0,16.0,16.0,6.0,0.0,0.0,0.0,0.0,0.0,11.0,16.0,10.0,0.0,0.0],
                [0.0,0.0,0.0,4.0,15.0,12.0,0.0,0.0,0.0,0.0,3.0,16.0,15.0,14.0,0.0,0.0,0.0,0.0,8.0,13.0,8.0,16.0,0.0,0.0,0.0,0.0,1.0,6.0,15.0,11.0,0.0,0.0,0.0,1.0,8.0,13.0,15.0,1.0,0.0,0.0,0.0,9.0,16.0,16.0,5.0,0.0,0.0,0.0,0.0,3.0,13.0,16.0,16.0,11.0,5.0,0.0,0.0,0.0,0.0,3.0,11.0,16.0,9.0,0.0],
                [0.0,0.0,7.0,15.0,13.0,1.0,0.0,0.0,0.0,8.0,13.0,6.0,15.0,4.0,0.0,0.0,0.0,2.0,1.0,13.0,13.0,0.0,0.0,0.0,0.0,0.0,2.0,15.0,11.0,1.0,0.0,0.0,0.0,0.0,0.0,1.0,12.0,12.0,1.0,0.0,0.0,0.0,0.0,0.0,1.0,10.0,8.0,0.0,0.0,0.0,8.0,4.0,5.0,14.0,9.0,0.0,0.0,0.0,7.0,13.0,13.0,9.0,0.0,0.0],
                [0.0,0.0,0.0,1.0,11.0,0.0,0.0,0.0,0.0,0.0,0.0,7.0,8.0,0.0,0.0,0.0,0.0,0.0,1.0,13.0,6.0,2.0,2.0,0.0,0.0,0.0,7.0,15.0,0.0,9.0,8.0,0.0,0.0,5.0,16.0,10.0,0.0,16.0,6.0,0.0,0.0,4.0,15.0,16.0,13.0,16.0,1.0,0.0,0.0,0.0,0.0,3.0,15.0,10.0,0.0,0.0,0.0,0.0,0.0,2.0,16.0,4.0,0.0,0.0],
                [0.0,0.0,12.0,10.0,0.0,0.0,0.0,0.0,0.0,0.0,14.0,16.0,16.0,14.0,0.0,0.0,0.0,0.0,13.0,16.0,15.0,10.0,1.0,0.0,0.0,0.0,11.0,16.0,16.0,7.0,0.0,0.0,0.0,0.0,0.0,4.0,7.0,16.0,7.0,0.0,0.0,0.0,0.0,0.0,4.0,16.0,9.0,0.0,0.0,0.0,5.0,4.0,12.0,16.0,4.0,0.0,0.0,0.0,9.0,16.0,16.0,10.0,0.0,0.0],
                [0.0,0.0,0.0,12.0,13.0,0.0,0.0,0.0,0.0,0.0,5.0,16.0,8.0,0.0,0.0,0.0,0.0,0.0,13.0,16.0,3.0,0.0,0.0,0.0,0.0,0.0,14.0,13.0,0.0,0.0,0.0,0.0,0.0,0.0,15.0,12.0,7.0,2.0,0.0,0.0,0.0,0.0,13.0,16.0,13.0,16.0,3.0,0.0,0.0,0.0,7.0,16.0,11.0,15.0,8.0,0.0,0.0,0.0,1.0,9.0,15.0,11.0,3.0,0.0],
                [0.0,0.0,7.0,8.0,13.0,16.0,15.0,1.0,0.0,0.0,7.0,7.0,4.0,11.0,12.0,0.0,0.0,0.0,0.0,0.0,8.0,13.0,1.0,0.0,0.0,4.0,8.0,8.0,15.0,15.0,6.0,0.0,0.0,2.0,11.0,15.0,15.0,4.0,0.0,0.0,0.0,0.0,0.0,16.0,5.0,0.0,0.0,0.0,0.0,0.0,9.0,15.0,1.0,0.0,0.0,0.0,0.0,0.0,13.0,5.0,0.0,0.0,0.0,0.0],
                [0.0,0.0,9.0,14.0,8.0,1.0,0.0,0.0,0.0,0.0,12.0,14.0,14.0,12.0,0.0,0.0,0.0,0.0,9.0,10.0,0.0,15.0,4.0,0.0,0.0,0.0,3.0,16.0,12.0,14.0,2.0,0.0,0.0,0.0,4.0,16.0,16.0,2.0,0.0,0.0,0.0,3.0,16.0,8.0,10.0,13.0,2.0,0.0,0.0,1.0,15.0,1.0,3.0,16.0,8.0,0.0,0.0,0.0,11.0,16.0,15.0,11.0,1.0,0.0],
                [0.0,0.0,11.0,12.0,0.0,0.0,0.0,0.0,0.0,2.0,16.0,16.0,16.0,13.0,0.0,0.0,0.0,3.0,16.0,12.0,10.0,14.0,0.0,0.0,0.0,1.0,16.0,1.0,12.0,15.0,0.0,0.0,0.0,0.0,13.0,16.0,9.0,15.0,2.0,0.0,0.0,0.0,0.0,3.0,0.0,9.0,11.0,0.0,0.0,0.0,0.0,0.0,9.0,15.0,4.0,0.0,0.0,0.0,9.0,12.0,13.0,3.0,0.0,0.0],
                [0.0,0.0,1.0,9.0,15.0,11.0,0.0,0.0,0.0,0.0,11.0,16.0,8.0,14.0,6.0,0.0,0.0,2.0,16.0,10.0,0.0,9.0,9.0,0.0,0.0,1.0,16.0,4.0,0.0,8.0,8.0,0.0,0.0,4.0,16.0,4.0,0.0,8.0,8.0,0.0,0.0,1.0,16.0,5.0,1.0,11.0,3.0,0.0,0.0,0.0,12.0,12.0,10.0,10.0,0.0,0.0,0.0,0.0,1.0,10.0,13.0,3.0,0.0,0.0],
                [0.0,0.0,0.0,0.0,14.0,13.0,1.0,0.0,0.0,0.0,0.0,5.0,16.0,16.0,2.0,0.0,0.0,0.0,0.0,14.0,16.0,12.0,0.0,0.0,0.0,1.0,10.0,16.0,16.0,12.0,0.0,0.0,0.0,3.0,12.0,14.0,16.0,9.0,0.0,0.0,0.0,0.0,0.0,5.0,16.0,15.0,0.0,0.0,0.0,0.0,0.0,4.0,16.0,14.0,0.0,0.0,0.0,0.0,0.0,1.0,13.0,16.0,1.0,0.0],
                [0.0,0.0,5.0,12.0,1.0,0.0,0.0,0.0,0.0,0.0,15.0,14.0,7.0,0.0,0.0,0.0,0.0,0.0,13.0,1.0,12.0,0.0,0.0,0.0,0.0,2.0,10.0,0.0,14.0,0.0,0.0,0.0,0.0,0.0,2.0,0.0,16.0,1.0,0.0,0.0,0.0,0.0,0.0,6.0,15.0,0.0,0.0,0.0,0.0,0.0,9.0,16.0,15.0,9.0,8.0,2.0,0.0,0.0,3.0,11.0,8.0,13.0,12.0,4.0],
                [0.0,2.0,9.0,15.0,14.0,9.0,3.0,0.0,0.0,4.0,13.0,8.0,9.0,16.0,8.0,0.0,0.0,0.0,0.0,6.0,14.0,15.0,3.0,0.0,0.0,0.0,0.0,11.0,14.0,2.0,0.0,0.0,0.0,0.0,0.0,2.0,15.0,11.0,0.0,0.0,0.0,0.0,0.0,0.0,2.0,15.0,4.0,0.0,0.0,1.0,5.0,6.0,13.0,16.0,6.0,0.0,0.0,2.0,12.0,12.0,13.0,11.0,0.0,0.0],
                [0.0,0.0,0.0,8.0,15.0,1.0,0.0,0.0,0.0,0.0,1.0,14.0,13.0,1.0,1.0,0.0,0.0,0.0,10.0,15.0,3.0,15.0,11.0,0.0,0.0,7.0,16.0,7.0,1.0,16.0,8.0,0.0,0.0,9.0,16.0,13.0,14.0,16.0,5.0,0.0,0.0,1.0,10.0,15.0,16.0,14.0,0.0,0.0,0.0,0.0,0.0,1.0,16.0,10.0,0.0,0.0,0.0,0.0,0.0,10.0,15.0,4.0,0.0,0.0],
                [0.0,5.0,12.0,13.0,16.0,16.0,2.0,0.0,0.0,11.0,16.0,15.0,8.0,4.0,0.0,0.0,0.0,8.0,14.0,11.0,1.0,0.0,0.0,0.0,0.0,8.0,16.0,16.0,14.0,0.0,0.0,0.0,0.0,1.0,6.0,6.0,16.0,0.0,0.0,0.0,0.0,0.0,0.0,5.0,16.0,3.0,0.0,0.0,0.0,1.0,5.0,15.0,13.0,0.0,0.0,0.0,0.0,4.0,15.0,16.0,2.0,0.0,0.0,0.0],
                [0.0,0.0,0.0,8.0,15.0,1.0,0.0,0.0,0.0,0.0,0.0,12.0,14.0,0.0,0.0,0.0,0.0,0.0,3.0,16.0,7.0,0.0,0.0,0.0,0.0,0.0,6.0,16.0,2.0,0.0,0.0,0.0,0.0,0.0,7.0,16.0,16.0,13.0,5.0,0.0,0.0,0.0,15.0,16.0,9.0,9.0,14.0,0.0,0.0,0.0,3.0,14.0,9.0,2.0,16.0,2.0,0.0,0.0,0.0,7.0,15.0,16.0,11.0,0.0],
                [0.0,0.0,1.0,8.0,15.0,10.0,0.0,0.0,0.0,3.0,13.0,15.0,14.0,14.0,0.0,0.0,0.0,5.0,10.0,0.0,10.0,12.0,0.0,0.0,0.0,0.0,3.0,5.0,15.0,10.0,2.0,0.0,0.0,0.0,16.0,16.0,16.0,16.0,12.0,0.0,0.0,1.0,8.0,12.0,14.0,8.0,3.0,0.0,0.0,0.0,0.0,10.0,13.0,0.0,0.0,0.0,0.0,0.0,0.0,11.0,9.0,0.0,0.0,0.0],
                [0.0,0.0,10.0,7.0,13.0,9.0,0.0,0.0,0.0,0.0,9.0,10.0,12.0,15.0,2.0,0.0,0.0,0.0,4.0,11.0,10.0,11.0,0.0,0.0,0.0,0.0,1.0,16.0,10.0,1.0,0.0,0.0,0.0,0.0,12.0,13.0,4.0,0.0,0.0,0.0,0.0,0.0,12.0,1.0,12.0,0.0,0.0,0.0,0.0,1.0,10.0,2.0,14.0,0.0,0.0,0.0,0.0,0.0,11.0,14.0,5.0,0.0,0.0,0.0],
                [0.0,0.0,6.0,14.0,4.0,0.0,0.0,0.0,0.0,0.0,11.0,16.0,10.0,0.0,0.0,0.0,0.0,0.0,8.0,14.0,16.0,2.0,0.0,0.0,0.0,0.0,1.0,12.0,12.0,11.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,11.0,3.0,0.0,0.0,0.0,0.0,0.0,0.0,5.0,11.0,0.0,0.0,0.0,1.0,4.0,4.0,7.0,16.0,2.0,0.0,0.0,7.0,16.0,16.0,13.0,11.0,1.0]
            ]

testNeighbourProbs :: [[Probability]]
testNeighbourProbs = neighbourProbabilities def testInput

testInputM :: TSNEInputM
testInputM = MA.fromLists' MA.Seq testInput

testNeighbourProbsM :: MA.Matrix MA.U Probability
testNeighbourProbsM = neighbourProbabilitiesM def testInputM
