module TwoOpt where
import Definitions
import Data.List
import Data.Maybe
import Debug.Trace

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
    - (dm !! a !! b)
    - (dm !! c !! d)
    + (dm !! a !! c)
    + (dm !! b !! d)

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

maybeToInt :: Maybe Int -> Int
maybeToInt Nothing = 0
maybeToInt (Just n) = n

generatePairs :: Tour -> [(Int,Int)]
generatePairs t = filter (\(x,y) -> y >= x+2) (pairs t)

generateCityFourtuple :: Tour -> (Int,Int) -> (Int,Int,Int,Int)
generateCityFourtuple t (i,j) = do
    let a = (t !! i) - 1;
    let b = (t !! (i + 1)) - 1;
    let c = (t !! j) - 1;
    let d = (t !! ((j + 1) `mod` length t)) - 1;
    (a,b,c,d)

-- Unused
filterCityFourtuple :: DM -> (Int,Int,Int,Int) -> Bool
filterCityFourtuple dm (a,b,c,d)= do
    let firstCondition = (a /= c) && (d /= a)
    let secondCondition = ((dm !! a !! b) > (dm !! b !! c)) || ((dm !! c !! d) > (dm !! c !! a))
    firstCondition || secondCondition

{-                  Cities to swap      -> DM -> (fourtuple,Gain)  -}
firstImprovement :: [(Int,Int,Int,Int)] -> DM -> (Maybe (Int,Int,Int,Int),Int)
firstImprovement [] _ = (Nothing, 0)
firstImprovement (x:xs) dm = do
    let gain = calculateGain dm x
    if gain < 0 then
        (Just x,gain)
    else
        firstImprovement xs dm

twoOpt :: Tour -> DM -> Int -> (Tour,Int)
twoOpt t dm g= do
    let p = trace "gen pairs" generatePairs [0..(length t - 1)]
    let citiesFourtuples = trace "map" map (generateCityFourtuple t) p
    let (tuple,gain) = trace "fi" firstImprovement citiesFourtuples dm
    case tuple of
        Just tp -> do
            let index =  maybeToInt $ elemIndex tp citiesFourtuples
            let (i,j) = p !! index
            let newT = twoOptSwap t (i+1) j
            twoOpt newT dm (g+gain)
        Nothing -> (t,g)
