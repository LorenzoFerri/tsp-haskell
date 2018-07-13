module Main where

import Parse
import System.Environment  (getArgs)
import Text.Parsec
import Definitions
import System.Random

main :: IO ()
main = do
  args <- getArgs
  if null args then
    putStrLn "No args"
  else do
    let file = head args
    let seed = read $ head $ tail args
    let gen = mkStdGen seed
    input <- readFile $ file
    case parse parseInput file input of
      Right distanceMatrix -> do
        randomRIO (3,999)
      Left err ->
        putStrLn $ show err

-- createGenerator [] = mkStdGen (take 1 (randoms newStdGen))
-- createGenerator seed = 