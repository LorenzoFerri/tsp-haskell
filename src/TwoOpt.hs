module TwoOpt where
import Definitions

twoOptSwap :: Tour -> Int -> Int -> Tour
twoOptSwap t a b = 
    if a >= b then
        t
    else do
        let newT = swap a b t
        twoOptSwap newT (a+1) (b-1)

swap :: Int -> Int -> [a] -> [a]
swap a b list = list1 ++ [list !! b] ++ list2 ++ [list !! a] ++ list3
    where   list1 = take a list;
            list2 = drop (succ a) (take b list);
            list3 = drop (succ b) list