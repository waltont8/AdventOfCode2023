module Day09
    ( day09
    ) where

import Lazy

day09 :: String -> (String, String)
day09 = (lines >>> map parse >>> part1 &&& part2 >>> tidy)


parse :: String -> [Int]
parse = splitOn " " >>> map readSignedInt

part1 :: [[Int]] -> Int
part1 l = sum $ map last res
    where
        res = map nextVal l

nextVal :: [Int] -> [Int]
nextVal l
    | same = [last l + last next]
    | otherwise = l ++ [ last (nextVal next) + last l]
    where
        next = zipWith (-) (tail l) l
        same = (length (group next) == 1) && (head next == 0)

preVal:: [Int] -> [Int]
preVal l
    | same = [head l - head next]
    | otherwise = [ head l - head (preVal next)] ++ l 
    where
        next = zipWith (-) (tail l) l
        same = (length (group next) == 1) && (head next == 0)

part2 :: [[Int]] -> Int
part2 l = trace (show res) $ sum $ map head res
    where
        res = map preVal l

tidy :: (Int, Int) -> (String, String)
tidy (a,b) = (show a, show b)

