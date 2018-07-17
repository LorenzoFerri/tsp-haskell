module Main where

import Parse
import System.Environment  (getArgs)
import Text.Parsec
import Definitions
import System.Random
import System.Random.Shuffle

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
        putStrLn $ show $ ys
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

