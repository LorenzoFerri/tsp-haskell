module Main where

import Parse
import System.Environment  (getArgs)
import Text.Parsec
import Definitions
import System.Random
import System.Random.Shuffle
import Data.List as Lis
import Data.Matrix as Mat
import TwoOpt
import Debug.Trace
import Data.Vector as Vec

main :: IO ()
main = do
  args <- getArgs
  if Lis.null args then
    putStrLn "No args"
  else do
    let file = Lis.head args
    rng <- createGenerator $ Lis.tail args
    input <- readFile file
    case parse parseInput file input of
      Right (l,distanceMatrix) -> do
        shuffle <- shuffleM [1..l] -- Random solution here
        let ys = Vec.fromList shuffle -- To a vector
        let pairs =  Vec.fromList $ generatePairs [0..(l-1)]
        let (solution,gain) = twoOpt ys distanceMatrix 0 pairs
        print solution
        print $ getSolutionCost solution distanceMatrix
      Left err ->
        print err

createGenerator :: [String] -> IO StdGen
createGenerator seedList =
  if Lis.null seedList then
    getStdGen
  else do
    let seed = read $ Lis.head seedList
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

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

generatePairs :: [Int] -> [(Int,Int)]
generatePairs t = Lis.filter (\(x,y) -> y >= x+2) (pairs t)
