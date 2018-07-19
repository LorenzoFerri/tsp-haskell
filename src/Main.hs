module Main where

import Parse
import System.Environment  (getArgs)
import Text.Parsec
import Definitions
import System.Random
import System.Random.Shuffle
import Data.List
import Data.Matrix as Mat
import TwoOpt
import Debug.Trace
import Data.Vector as Vec

main :: IO ()
main = do
  args <- getArgs
  if Data.List.null args then
    putStrLn "No args"
  else do
    let file = Data.List.head args
    rng <- createGenerator $ Data.List.tail args
    input <- readFile file
    case parse parseInput file input of
      Right distanceMatrix -> do
        let l = Data.List.length $ getRow 1 distanceMatrix
        let xs = [1..l]
        let p = Vec.fromList $ generatePairs [0 .. (l - 1)]
        ys <- shuffleM xs -- Random solution here
        let vecY = Vec.fromList ys
        print $ getSolutionCost vecY distanceMatrix
        let (new,gain) = twoOpt vecY distanceMatrix 0 p
        print new
        print gain
        print $ getSolutionCost new distanceMatrix
      Left err ->
        print err

createGenerator :: [String] -> IO StdGen
createGenerator seedList =
  if Data.List.null seedList then
    getStdGen
  else do
    let seed = read $ Data.List.head seedList
    setStdGen (mkStdGen seed)
    getStdGen

getSolutionCost :: Tour -> DM -> Int
getSolutionCost t dm = do
  let pairs = constructPairs t
  let costs = Vec.map (getPairCost dm) pairs
  Vec.sum costs

getPairCost :: DM -> (Int,Int) -> Int
getPairCost dm (x,y) = dm Mat.! (x,y) 

constructPairs :: Tour -> Vector (Int,Int)
constructPairs t = do
  let pairs = Vec.zip t $ Vec.tail t
  pairs Vec.++ Vec.fromList [(Vec.head t , Vec.last t)]
