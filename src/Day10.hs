module Day10
    ( day10
    ) where

import Lazy
import qualified Data.Map as M
import qualified Data.Set as S

type Coord = (Int, Int)
type Grid = M.Map Coord Char

type CSet = S.Set Coord

day10 :: String -> (String, String)
day10 = lines >>> parse >>> (part1 &&& part2) >>> tidy

parse :: [String] -> Grid
parse input =  foldl (\m (n,c) -> M.insert (n `mod` width, n `div` width) c m) (M.empty :: Grid) $ zip [0..] (concat input)
    where 
        width = length (input!!0)

findStart :: Grid -> Coord
findStart g = fst $ head $ filter (\(_,v) -> v == 'S') $ M.toList g

canDown c = c `elem` "|JLS"
canUp c = c `elem` "|7FS"
canRight c = c `elem` "-7JS"
canLeft c = c `elem` "-LFS"

getDown (x,y) = (x,y+1)
getUp (x,y) = (x,y-1)
getRight (x,y) = (x+1,y)
getLeft (x,y) = (x-1,y)

getPipe :: Grid -> Coord -> Char
getPipe g c = if c `M.member` g then (g M.! c) else '.'

findNext cNow cLast g
    | current == '-' && canRight right && getRight cNow /= cLast = getRight cNow
    | current == '-' && canLeft left && getLeft cNow /= cLast = getLeft cNow
    | current == '|' && canUp up && getUp cNow /= cLast = getUp cNow
    | current == '|' && canDown down && getDown cNow /= cLast = getDown cNow
    | current == '7' && canLeft left && getLeft cNow /= cLast = getLeft cNow
    | current == '7' && canDown down && getDown cNow /= cLast = getDown cNow
    | current == 'F' && canRight right && getRight cNow /= cLast = getRight cNow
    | current == 'F' && canDown down && getDown cNow /= cLast = getDown cNow
    | current == 'J' && canLeft left && getLeft cNow /= cLast = getLeft cNow
    | current == 'J' && canUp up && getUp cNow /= cLast = getUp cNow
    | current == 'L' && canRight right && getRight cNow /= cLast = getRight cNow
    | current == 'L' && canUp up && getUp cNow /= cLast = getUp cNow
    | current == 'S' && canDown down && getDown cNow /= cLast = getDown cNow
    | current == 'S' && canLeft left && getLeft cNow /= cLast = getLeft cNow
    | current == 'S' && canUp up && getUp cNow /= cLast = getUp cNow
    | current == 'S' && canRight right && getRight cNow /= cLast = getRight cNow
    | otherwise = error $ "findNext error:" ++ "Current:" ++ (show current) ++ " " ++ (show cNow) ++ ":" ++ (show cLast) ++ " " ++ (show $ getDown cNow)
        where
            current = getPipe g cNow
            right = getPipe g (getRight cNow)
            left = getPipe g (getLeft cNow)
            up = getPipe g (getUp cNow)
            down = getPipe g (getDown cNow)

getLength :: Grid -> Coord -> Coord -> Int
getLength g l c@(x,y)
    | getPipe g nc == 'S' = 0
    | otherwise = 1 + getLength g c nc
    where
        nc = findNext c l g

findTile :: Grid -> Coord -> Coord -> Int -> Char
findTile g l c@(x,y) n
    | n == 0 = getPipe g c
    | otherwise = findTile g c nc (n-1)
    where
        nc = findNext c l g


part1 :: Grid -> Int
part1 g = (getLength g start start + 1) `div` 2
    where
        start = findStart g

part2 :: Grid -> Int -- the whole grid - the path - the outersquares
part2 g = 140*140 - (length path + length outer)
    where
        start = findStart g -- Get the start
        path = tracePath g start start -- Extract the path
        ng = M.fromList path -- Make a new map with just the path
        -- Floodfill from the outside
        outer = map (\(x, y) -> (x `div` 3, y `div` 3)) $ filter (\(x, y) -> (x-1) `mod` 3 == 0 && (y-1) `mod` 3 == 0) $ S.toList $ floodFill ng S.empty (0,0)


tracePath :: Grid -> Coord -> Coord -> [(Coord, Char)]
tracePath g l c@(x,y)
    | getPipe g nc == 'S' = [(c, getPipe g c)]
    | otherwise = ((x,y), getPipe g c) : tracePath g c nc
    where
        nc = findNext c l g

floodFill :: Grid -> CSet -> Coord -> CSet
floodFill g s c@(x,y)
    | x<0 || y<0 || y >= 140*3 || x >= 140*3 = s
    | subSample g c == 1 = s
    | c `S.member` s = s
    | otherwise = right
        where
            ns = S.insert c s
            up = (floodFill g ns (x,y-1))
            down = (floodFill g up (x,y+1))
            left = (floodFill g down (x-1,y))
            right = (floodFill g left (x+1,y))


subGrid '|' = [[0,1,0],[0,1,0],[0,1,0]]
subGrid '-' = [[0,0,0],[1,1,1],[0,0,0]]
subGrid 'L' = [[0,1,0],[0,1,1],[0,0,0]]
subGrid 'J' = [[0,1,0],[1,1,0],[0,0,0]]
subGrid '7' = [[0,0,0],[1,1,0],[0,1,0]]
subGrid 'F' = [[0,0,0],[0,1,1],[0,1,0]]
subGrid '.' = [[0,0,0],[0,0,0],[0,0,0]]
subGrid 'S' = [[1,1,1],[1,1,1],[1,1,1]]

subSample :: Grid -> Coord -> Int
subSample g (x,y) = (sg !! (y `mod` 3)) !! (x `mod` 3)
    where
        c = getPipe g (x `div` 3, y `div` 3)
        sg = subGrid c

tidy :: (Int, Int) -> (String, String)
tidy (a,b) = (show a, show b)

