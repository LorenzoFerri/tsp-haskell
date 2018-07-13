module Main where

import Parse
import System.Environment  (getArgs)
import Text.Parsec

main :: IO ()
main = do
  args <- getArgs
  if null args then
    putStrLn "No args"
  else do
    input <- readFile $ head args
    case parse parseInput (head args) input of
      Right s -> do
        putStrLn $ show s
      Left err ->
        putStrLn $ show err