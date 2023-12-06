module Day04
    ( day04
    ) where

import Lazy

day04 :: String -> (String, String)
day04 = (lines >>> map parse >>> part1 &&& (part2) >>> tidy)


parse :: String -> (Int, [Int], [Int])
parse s = (cardNumber, myNums, winNums)
    where
        s1 = splitOn ":" s
        cardNumber = readInt $ last $ splitOn " " (head s1)
        numParts = splitOn "|" (last s1)
        myNums = map readInt $ filter (/="") $ splitOn " " (head numParts)
        winNums = map readInt $ filter (/="") $ splitOn " " (last numParts)


part1 :: [(Int, [Int], [Int])] -> Int
part1 ((c,m,w):xs) =  score + part1 xs
    where
        wins = length $ filter (`elem` m) w
        score = case wins of
                 0 -> 0
                 1 -> 1
                 _ -> 2 ^ (wins-1)

part1 [] = 0

part2 a = part2a (length a) a
part2a :: Int -> [(Int, [Int], [Int])] -> Int
part2a 0 _ = 0
part2a mults ((c,m,w):xs) = 1 + part2a (mults-1) xs + part2a wins xs
    where
        wins = length $ filter (`elem` m) w
part2a _ [] = 0


tidy :: (Int, Int) -> (String, String)
tidy (a,b) = (show a, show b)

