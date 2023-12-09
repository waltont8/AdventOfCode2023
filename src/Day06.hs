module Day06
    ( day06
    ) where

import Lazy

times1 = [ (53, 250), (91,1330), (67, 1081), (68, 1025) ]
times2 = [ (53916768, 250133010811025) ]

day06 :: String -> (String, String)
day06 _ = tidy (part1, part2)

part1 :: Int
part1 = product $ map m times1

part2 :: Int
part2 =  product $ map m times2

m :: (Double, Double) -> Int
m (t,d) = 
    let f op = ((-t `op` sqrt (t ** 2 - 4 * d)) / (-2))
    in ceiling (f (+)) - floor (f (-)) - 1

tidy :: (Int, Int) -> (String, String)
tidy (a,b) = (show a, show b)

