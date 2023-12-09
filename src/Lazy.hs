-- Lazy hacks for Advent of code, turn off some warnings
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-compat-unqualified-imports -Wno-unused-matches -Wno-incomplete-patterns -Wno-unused-imports#-}

module Lazy (
    module Control.Arrow,
    module Control.Monad,
    module Data.List,
    module Data.Char,
    module Data.List.Split,
    module Data.Function,
    module Debug.Trace,
    module System.Environment,
    module Data.Maybe,
    listSplit,
    mapInc,
    mapAdd,
    mapRead,
    flatten,
    hasAll,
    tupleMap,
    setGrid,
    getGrid,
    emptyIntGrid,
    coord2DFile,
    initGrid,
    bottomPointGrid,
    topPointGrid,
    leftPointGrid,
    rightPointGrid,
    readSignedInt,
    toDec,
    readChar,
    firstInt,
    readInt,
    replaceAll

) where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Char
import Data.List.Split
import Data.Maybe
import Data.Function (on)
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace
import System.Environment


-- Add to or increment a map
mapInc :: Ord a => (M.Map a Int) -> a -> (M.Map a Int)
mapInc m a = if (M.member a m) then (M.insert a ((m M.! a) + 1) m) else (M.insert a 1 m)

mapAdd :: Ord a => (M.Map a Int) -> a -> Int -> (M.Map a Int)
mapAdd m a i = if (M.member a m) then (M.insert a ((m M.! a) + i) m) else (M.insert a i m)

mapRead :: Ord a => (M.Map a Int) -> a -> Int
mapRead m a = if (M.member a m) then (m M.! a) else 0


flatten :: [[a]] -> [a]         
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

hasAll :: String -> String -> Bool
hasAll a b = all (`elem` b) a

tupleMap f (a,b) = (f a, f b)

--- Grid functions
data Grid a = Grid (M.Map (Int, Int) a) a deriving (Show, Read, Eq) 

setGrid :: Grid a -> (Int,Int) -> a -> Grid a
setGrid (Grid m d) (x,y) v = Grid  (M.insert (x,y) v m) d

getGrid :: Grid a -> (Int,Int) -> a
getGrid (Grid m d) p = if (M.member p m) then (m M.! p) else d

emptyIntGrid :: Grid Int
emptyIntGrid = Grid M.empty 0


coord2DFile :: String -> IO [(Int,Int)]
coord2DFile s = fmap (lines >>> map (splitOn ",") >>> map tupleIt) $ readFile s
    where
        tupleIt [a,b] = (read a, read b)
        tupleIt a = error $ "Failed to read coordinate file " ++ (show a)

initGrid :: Int -> [(Int, Int)] -> Grid Int
initGrid si pts = foldl (\g (x,y) -> setGrid g (x,y) si) (emptyIntGrid) pts

bottomPointGrid :: Grid a -> (Int, Int)
bottomPointGrid (Grid m _) = head $ sortBy (compare `on` snd) $ (M.keys m)

topPointGrid :: Grid a -> (Int, Int)
topPointGrid (Grid m _) = head $ sortBy (flip compare `on` snd) $ (M.keys m)

leftPointGrid :: Grid a -> (Int, Int)
leftPointGrid (Grid m _) = head $ sortBy (compare `on` fst) $ (M.keys m)

rightPointGrid :: Grid a -> (Int, Int)
rightPointGrid (Grid m _) = head $ sortBy (flip compare `on` fst) $ (M.keys m)

-- reads

readInt :: String -> Int
readInt = read

readSignedInt :: String -> Int
readSignedInt a@(h:xs)
                    | h == '-' = ((read xs)::Int) * (-1)
                    | h == '+' = (read xs)::Int
                    | isDigit h = (read a)::Int
                    | otherwise = error "+/- passed to readSignedInt"

readChar :: Char -> Int
readChar '0' = 0
readChar '1' = 1
readChar '2' = 2
readChar '3' = 3
readChar '4' = 4
readChar '5' = 5
readChar '6' = 6
readChar '7' = 7
readChar '8' = 8
readChar '9' = 9

-- Binary string to int
toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

--listSplit (==[]) s
listSplit :: Eq a => ([a] -> Bool) -> [[a]] -> [[[a]]]
listSplit p s = case dropWhile p s of
                [] -> []
                s' -> w : listSplit p s''
                  where (w, s'') = break p s'


firstInt :: String -> Int
firstInt (h:xs) = read num
  where
    afterDroppingNonDigits = dropWhile (not . isDigit) xs
    num = takeWhile isDigit afterDroppingNonDigits


replaceAll :: Eq t => t -> t -> [t] -> [t]
replaceAll x y = map (\h -> if h == x then y else h)