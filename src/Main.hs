module Main where

import Parse
import System.Environment  (getArgs)
import Text.Parsec
import Definitions
import System.Random
import System.Random.Shuffle
import Data.List

main :: IO ()
main = do
  args <- getArgs
  if null args then
    putStrLn "No args"
  else do
    let file = head args
    rng <- createGenerator $ tail args
    input <- readFile $ file
    case parse parseInput file input of
      Right distanceMatrix -> do
        let l = length distanceMatrix
        let xs = [1..l]
        ys <- shuffleM xs -- Random solution here
        putStrLn $ show ys
        putStrLn $ show $ getSolutionCost ys distanceMatrix
      Left err ->
        putStrLn $ show err

createGenerator :: [[Char]] -> IO StdGen
createGenerator seedList =
  if null seedList then do
    getStdGen
  else do
    let seed = read $ head seedList
    setStdGen (mkStdGen seed)
    getStdGen

isSolutionCorrect :: Tour -> Tour -> Bool
isSolutionCorrect a b = (sort a) == (sort b)

getSolutionCost :: Tour -> DM -> Int
getSolutionCost t dm = do
  let pairs = constructPairs t
  let costs = map (getPairCost dm) pairs
  foldr (+) 0 costs

getPairCost :: DM -> (Int,Int) -> Int
getPairCost dm (x,y) = dm !! (x-1) !! (y-1) 

constructPairs :: Tour -> [(Int,Int)]
constructPairs t = do
  let pairs = zip t $ tail t
  pairs ++ [(head t , last t)]
