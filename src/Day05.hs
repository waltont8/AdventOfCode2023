module Day05
    ( day05
    ) where

import Lazy

day05 :: String -> (String, String)
day05 = (lines >>> parse >>> part1 &&& part2 >>> tidy)


parse :: [String] -> ([Int] , [[(Int, Int, Int)]])
parse s = (seeds, mappings)
    where 
        sections = listSplit (==[]) s
        seeds = map read $ tail $ splitOn " " $ snd $ break (==' ') (head s)
        mappings = map extractSection (tail sections)

extractSection :: [String] -> [(Int, Int, Int)]
extractSection a = map (\(a:b:c:_) -> (a,b,c)) $ map (splitOn " " >>> map read) $ tail a


part1 :: ([Int] , [[(Int, Int, Int)]]) -> Int
part1 (seeds, mapping) = minimum $ map (seedLookup mapping) seeds

seedLookup :: [[(Int, Int, Int)]] -> Int -> Int
seedLookup m s = res
    where
        res = foldl (\seed rows -> sectionLookup seed rows) s m

sectionLookup :: Int -> [(Int, Int, Int)] -> Int
sectionLookup s r = if (a == -1) then s else (s - b) + a
    where
        res = filter (\(a,b,c) -> s >= b && s < (b+c)) r
        ((a,b,c):[]) = if (length res == 0) then [(-1,-1,-1)] else res


part2 :: ([Int] , [[(Int, Int, Int)]]) -> Int
part2 (seeds, mapping) = minimum $ map (seedLookup mapping) seedPairs
    where
        seedPairs = getPairs seeds

getPairs (a:b:xs) = [a .. (a+b)] ++ getPairs xs
getPairs _ = []

tidy :: (Int, Int) -> (String, String)
tidy (a,b) = (show a, show b)

