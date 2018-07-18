module Main where

import Parse
import System.Environment  (getArgs)
import Text.Parsec
import Definitions
import System.Random
import System.Random.Shuffle
import Data.List
import TwoOpt
import Debug.Trace

main :: IO ()
main = do
  args <- getArgs
  if null args then
    putStrLn "No args"
  else do
    let file = head args
    rng <- createGenerator $ tail args
    input <- readFile file
    case parse parseInput file input of
      Right distanceMatrix -> do
        let l = length distanceMatrix
        let xs = [1..l]
        ys <- shuffleM xs -- Random solution here
        print ys
        print $ getSolutionCost ys distanceMatrix
        let (new,gain) = twoOpt ys distanceMatrix 0
        print new
        print gain
        print $ getSolutionCost new distanceMatrix
      Left err ->
        print err

createGenerator :: [String] -> IO StdGen
createGenerator seedList =
  if null seedList then
    getStdGen
  else do
    let seed = read $ head seedList
    setStdGen (mkStdGen seed)
    getStdGen

isSolutionCorrect :: Tour -> Tour -> Bool
isSolutionCorrect a b = sort a == sort b

getSolutionCost :: Tour -> DM -> Int
getSolutionCost t dm = do
  let pairs = constructPairs t
  let costs = map (getPairCost dm) pairs
  sum costs

getPairCost :: DM -> (Int,Int) -> Int
getPairCost dm (x,y) = dm !! (x-1) !! (y-1) 

constructPairs :: Tour -> [(Int,Int)]
constructPairs t = do
  let pairs = zip t $ tail t
  pairs ++ [(head t , last t)]
