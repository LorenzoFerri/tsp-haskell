module TwoOpt where
import Definitions
import Data.List
import Data.Maybe
import Debug.Trace
import Data.Matrix

twoOptSwap :: Tour -> Int -> Int -> Tour
twoOptSwap t a b = 
    if a >= b then
        t
    else do
        let newT = swap a b t
        twoOptSwap newT (a+1) (b-1)

swap :: Int -> Int -> Tour -> Tour
swap a b list = list1 ++ [list !! b] ++ list2 ++ [list !! a] ++ list3
    where   list1 = take a list;
            list2 = drop (succ a) (take b list);
            list3 = drop (succ b) list

calculateGain :: DM -> (Int,Int,Int,Int) -> Int
calculateGain dm (a, b, c, d) = 
    - (dm ! (a, b))
    - (dm ! (c, d))
    + (dm ! (a, c))
    + (dm ! (b, d))

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

generatePairs :: Tour -> [(Int,Int)]
generatePairs t = filter (\(x,y) -> y >= x+2) (pairs t)

generateCityFourtuple :: Tour -> (Int,Int) -> (Int,Int,Int,Int)
generateCityFourtuple t (i,j) = do
    let a = t !! i;
    let b = t !! (i + 1);
    let c = t !! j;
    let d = t !! ((j + 1) `mod` length t);
    (a,b,c,d)

firstImprovement :: [(Int,Int)] -> Tour -> DM -> Int -> (Int, Int)
firstImprovement [] _ _ _ = (-1, 0)
firstImprovement (p:ps) t dm index = do
    let fourTuple = generateCityFourtuple t p
    let gain = calculateGain dm fourTuple
    if gain < 0 then
        (index, gain)
    else
        firstImprovement ps t dm (index+1)

twoOpt :: Tour -> DM -> Int -> [(Int,Int)] -> (Tour,Int)
twoOpt t dm g p = do
    let (index,gain) = firstImprovement p t dm 0
    if gain < 0 then do
        let (i,j) = p !! index
        let newT = twoOptSwap t (i+1) j
        twoOpt newT dm (g+gain) p
    else
        (t,g)
