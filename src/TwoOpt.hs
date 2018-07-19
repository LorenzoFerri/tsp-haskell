module TwoOpt where
import Definitions
import Data.List as Lis
import Data.Maybe
import Debug.Trace as T
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

generateCityFourtuple :: Tour -> (Int,Int) -> (Int,Int,Int,Int)
generateCityFourtuple t (i,j) = do
    let a = t Vec.! i;
    let b = t Vec.! (i + 1);
    let c = t Vec.! j;
    let d = t Vec.! ((j + 1) `mod` Vec.length t);
    (a,b,c,d)

iterateAndSwap :: Tour -> DM -> Int -> Vector (Int,Int) -> (Tour,Int)
iterateAndSwap t dm g p = 
    if Vec.length p > 0 then 
        do
            let (i,j) = Vec.head p
            let fourTuple = generateCityFourtuple t (i,j)
            let gain = calculateGain dm fourTuple
            if gain < 0 then do
                let newTour = twoOptSwap t (i+1) j
                iterateAndSwap newTour dm (g+gain) (Vec.tail p)
            else
                iterateAndSwap t dm g (Vec.tail p)
        else
            (t,g)
        

twoOpt :: Tour -> DM -> Int -> Vector (Int,Int) -> (Tour,Int)
twoOpt t dm g p = do
    let (newT, gain) = iterateAndSwap t dm 0 p
    if gain < 0 then
        twoOpt newT dm (g+gain) p
    else
        (t,g)
