module TwoOpt where
import Definitions
import Data.List
import Data.Maybe
import Debug.Trace
import Data.Matrix as Mat
import Data.Vector as Vec

twoOptSwap :: Tour -> Int -> Int -> Tour
twoOptSwap t a b = 
    if a >= b then
        t
    else do
        let newT = swap a b t
        twoOptSwap newT (a+1) (b-1)

swap :: Int -> Int -> Tour -> Tour
swap a b vec = do
    let first = vec Vec.! a
    let second = vec Vec.! b
    vec // [(a,second),(b,first)]

calculateGain :: DM -> (Int,Int,Int,Int) -> Int
calculateGain dm (a, b, c, d) = 
    - (dm Mat.! (a, b))
    - (dm Mat.! (c, d))
    + (dm Mat.! (a, c))
    + (dm Mat.! (b, d))

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

generatePairs :: [Int] -> [(Int,Int)]
generatePairs t = Data.List.filter (\(x,y) -> y >= x+2) (pairs t)

generateCityFourtuple :: Tour -> (Int,Int) -> (Int,Int,Int,Int)
generateCityFourtuple t (i,j) = do
    let a = t Vec.! i;
    let b = t Vec.! (i + 1);
    let c = t Vec.! j;
    let d = t Vec.! ((j + 1) `mod` Vec.length t);
    (a,b,c,d)

firstImprovement :: Vector (Int,Int) -> Tour -> DM -> Int -> (Int, Int)
firstImprovement p t dm index =
    if index < Vec.length p then do
        let fourTuple = generateCityFourtuple t (p Vec.! index)
        let gain = calculateGain dm fourTuple
        if gain < 0 then
            (index, gain)
        else
            firstImprovement p t dm (index+1)
    else
        (-1,0)

twoOpt :: Tour -> DM -> Int -> Vector (Int,Int) -> (Tour,Int)
twoOpt t dm g p = do
    let (index,gain) = firstImprovement p t dm 0
    if gain < 0 then do
        let (i,j) = p Vec.! index
        let newT = twoOptSwap t (i+1) j
        twoOpt newT dm (g+gain) p
    else
        (t,g)
