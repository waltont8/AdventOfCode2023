module Day11
    ( day11
    ) where

import Lazy
import qualified Data.Map as M
import qualified Data.Set as S

type Coord = (Int, Int)
type Grid = M.Map Coord Char

day11 :: String -> (String, String)
day11 = lines >>> expandY >>> transpose >>> expandY >>> transpose >>> parse >>> (part1 &&& part2) >>> tidy

expandY (h:xs) 
    | all (=='.') h = h:h:expandY xs
    | otherwise = h:expandY xs
expandY [] = []

parse :: [String] -> Grid
parse input =  foldl (\m (n,c) -> M.insert (n `mod` width, n `div` width) c m) (M.empty :: Grid) $ zip [0..] (concat input)
    where 
        width = length (input!!0)


part1 :: Grid -> Int
part1 g = sum distances
    where
        galaxies = filter (\((x,y),c) -> c == '#') $ M.toList g
        allCombos = [(x,y) | x<-galaxies, y<-galaxies, x<y]
        distances = map distancePair allCombos

distancePair (((x1,y1), c1), ((x2,y2), c2)) = abs (x1-x2) + abs (y1-y2)

part2 :: Grid -> Int -- the whole grid - the path - the outersquares
part2 g = length $ M.toList g

tidy :: (Int, Int) -> (String, String)
tidy (a,b) = (show a, show b)

