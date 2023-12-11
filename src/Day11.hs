module Day11
    ( day11
    ) where

import Lazy
import qualified Data.Map as M
import qualified Data.Set as S

type Coord = (Int, Int)
type Grid = M.Map Coord Char

-- Part 1 and part 2 are the same so I just changed the number from 2 to 1000000 and dropped part 2
-- bit messy but it's a Monday morning and I am busy
day11 :: String -> (String, String)
day11 input = (show $ part g', "")
    where
        l = lines input
        blankRows = getBlankRows 0 l
        blankCols = getBlankRows 0 $ transpose l
        g = parse l
        g' = M.fromList $ map (stretch 1000000 blankRows blankCols) $ filter (\(_,c) -> c=='#') $ M.toList g

stretch :: Int -> [Int] -> [Int] -> (Coord, Char) -> (Coord, Char)
stretch fact rows cols ((x,y),c) = ((x+xm, y+ym),c)
    where
        xm = (*(fact-1)) $ length $ filter (<x) cols
        ym = (*(fact-1)) $ length $ filter (<y) rows

getBlankRows n (h:xs)
    | all (=='.') h = n:getBlankRows (n+1) xs
    | otherwise = getBlankRows (n+1) xs
getBlankRows _ [] = []

parse :: [String] -> Grid
parse input =  foldl (\m (n,c) -> M.insert (n `mod` width, n `div` width) c m) (M.empty :: Grid) $ zip [0..] (concat input)
    where 
        width = length (input!!0)


part :: Grid -> Int
part g = sum distances
    where
        galaxies = filter (\((x,y),c) -> c == '#') $ M.toList g
        allCombos = [(x,y) | x<-galaxies, y<-galaxies, x<y]
        distances = map distancePair allCombos

distancePair (((x1,y1), c1), ((x2,y2), c2)) = abs (x1-x2) + abs (y1-y2)

tidy :: (Int, Int) -> (String, String)
tidy (a,b) = (show a, show b)

