module Day01
    ( day01
    ) where

import Lazy

day01 :: String -> (String, String)
day01 = (lines >>> (map parse1 >>> part1) &&& (map parse2 >>> part2) >>> tidy)


parse1 :: String -> Int
parse1 s = read res
    where 
        nums = [ x | x <- s, x `elem` "0123456789" ]
        res = [head nums, last nums]

parse2 :: String -> Int
parse2 s = (head nums * 10) + (last nums)
    where 
        nums = swapit s

swapit :: String -> [Int]
swapit s@(x:xs) 
    | x `elem` "0123456789" = (readChar x):swapit xs
    | "one" `isPrefixOf` s = 1:swapit xs
    | "two" `isPrefixOf` s = 2:swapit xs
    | "three" `isPrefixOf` s = 3:swapit xs
    | "four" `isPrefixOf` s = 4:swapit xs
    | "five" `isPrefixOf` s = 5:swapit xs
    | "six" `isPrefixOf` s = 6:swapit xs
    | "seven" `isPrefixOf` s = 7:swapit xs
    | "eight" `isPrefixOf` s = 8:swapit xs
    | "nine" `isPrefixOf` s = 9:swapit xs
    | otherwise = swapit xs
swapit "" = []

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Int
part2 = sum

tidy :: (Int, Int) -> (String, String)
tidy (a,b) = (show a, show b)

