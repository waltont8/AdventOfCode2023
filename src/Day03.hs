module Day03
    ( day03
    ) where

import Lazy
import Data.Int (Int)
import Data.Bool (Bool)
import Data.Char (Char)

data Map = Map {
    d :: [String],
    width :: Int,
    height :: Int
} deriving (Show)

day03 :: String -> (String, String)
day03 = lines >>> parse >>> part1 &&& part2 >>> tidy

parse :: [String] -> Map
parse s = Map s (length (head s)) (length s)

part1 :: Map -> Int
part1 m = sum . catMaybes $ allParts
    where
        allParts = [getPartNumber m (x, y) |
                      x <- [0 .. (width m - 1)], y <- [0 .. (height m - 1)]]

part2 :: Map -> Int
part2 m = sum ratios
    where
        allParts = [getGearNumber m (x, y) |
                      x <- [0 .. (width m - 1)], y <- [0 .. (height m - 1)]]
        sorted = sortBy (\(_,a) (_,b) -> compare a b) (catMaybes allParts)
        ratios = ratio sorted

tidy :: (Int, Int) -> (String, String)
tidy (a,b) = (show a, show b)

-- Calculate ratio of adjacent pairs with the same gear
ratio ((a,(ax,ay)):(b,(bx,by)):xs)
    | ax==bx && ay==by = (a*b):ratio xs
    | otherwise = ratio ((b,(bx,by)):xs)
ratio [a] = error "One left"
ratio [] = []


getPartNumber :: Map -> (Int, Int) -> Maybe Int
getPartNumber m (x,y) = case getNumber m x y of
    Just n -> checkParts m x y n
    Nothing -> Nothing

-- Also get the coordinate for matching
getGearNumber :: Map -> (Int, Int) -> Maybe (Int, (Int, Int))
getGearNumber m (x,y) = case (getNumber m x y) of
    Just n -> getGear m x y n
    Nothing -> Nothing

getGear :: Map -> Int -> Int -> Int -> Maybe (Int, (Int, Int))
getGear m x y n = if not (null coords) then Just (n, head coords) else Nothing
    where
        l = length (show n)
        sx = x-1
        sy = y-1
        ex = x+l
        ey = y+1
        topP = zip [sx .. ex] (repeat sy)
        botP = zip [sx .. ex] (repeat ey)
        leftRight = [(sx,y), (ex,y)]
        ps = leftRight ++ topP ++ botP
        coords = filter (\x -> fromMaybe '.' (getLoc m x) == '*') ps

checkParts :: Map -> Int -> Int -> Int -> Maybe Int
checkParts m x y n = if hasPart syms then Just n else Nothing
    where
        l = length (show n)
        sx = x-1
        sy = y-1
        ex = x+l
        ey = y+1
        topP = zip [sx .. ex] (repeat sy)
        botP = zip [sx .. ex] (repeat ey)
        leftRight = [(sx,y), (ex,y)]
        ps = leftRight ++ topP ++ botP
        syms = map (fromMaybe '.' . getLoc m) ps

hasPart :: String -> Bool
hasPart s = any isPart s
    where
        isPart s = not (isDigit s) && (s /= '.')


getNumber :: Map -> Int -> Int -> Maybe Int
getNumber m x y
    | isNumberStart m x y = Just n
    | otherwise = Nothing
        where
            n = read $ takeWhile isDigit (drop x (d m !! y))

isNumberStart :: Map -> Int -> Int -> Bool
isNumberStart m x y = start && left
    where
        start = isNum m x y
        left = not $ isNum m (x - 1) y

isNum :: Map -> Int -> Int -> Bool
isNum m x y = maybe False isDigit (getLoc m (x,y))

getLoc :: Map -> (Int, Int) -> Maybe Char
getLoc m (x,y)
    | x < 0 || x >= width m = Nothing
    | y < 0 || y >= height m = Nothing
    | otherwise = Just ((d m!!y)!!x)


