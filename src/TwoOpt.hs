module TwoOpt where
import Definitions
import Data.List as Lis
import Data.Maybe
import Debug.Trace as T
import Data.Matrix as Mat
import Data.Vector as Vec

swap :: Int -> Int -> Tour -> Tour
swap a b vec =
    if a < b then do
        let start  = Vec.take a vec
        let middle = Vec.reverse $ Vec.slice a (b-a+1) vec
        let end    = Vec.drop (b+1) vec
        start Vec.++ middle Vec.++ end
    else
        vec

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

iterateAndSwap :: Tour -> DM -> Int -> Vector (Int,Int) -> Int -> (Tour,Int)
iterateAndSwap t dm g p index = 
    if Vec.length p > index then 
        do
            let (i,j) = p Vec.! index
            let fourTuple = generateCityFourtuple t (i,j)
            let gain = calculateGain dm fourTuple
            if gain < 0 then do
                let newTour = swap (i+1) j t
                iterateAndSwap newTour dm (g+gain) p (index+1)
            else
                iterateAndSwap t dm g p (index+1)
        else
            (t,g)
        

twoOpt :: Tour -> DM -> Int -> Vector (Int,Int) -> (Tour,Int)
twoOpt t dm g p = do
    let (newT, gain) = iterateAndSwap t dm 0 p 0
    if gain < 0 then
        twoOpt newT dm (g+gain) p
    else
        (t,g)
