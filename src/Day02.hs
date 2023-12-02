module Day02
    ( day02
    ) where

import Lazy


--data Colour =  Red | Green | Blue deriving (Enum, Show)

data Draw = Draw {
    red :: Int,
    green :: Int,
    blue :: Int
} deriving Show

type Game = (Int, [Draw])


day02 :: String -> (String, String)
day02 = (lines >>> map parse >>> part1 &&& part2 >>> tidy)

parse :: String -> Game
parse s = (game, draws)
    where 
       colon = splitOn ":" s
       gameStrings = splitOn ";" $ last colon
       game = read (last $ splitOn " " (head colon)) :: Int
       draws = map parseDraw gameStrings

parseDraw :: String -> Draw
parseDraw s = g
    where
        cols = splitOn "," s
        g = foldl (rgb) (Draw 0 0 0) cols

rgb :: Draw -> String -> Draw
rgb g s 
    | "red" `isInfixOf` s = Draw (red g + firstInt s) (green g) (blue g)
    | "green" `isInfixOf` s = Draw (red g) (green g  + firstInt s) (blue g)
    | "blue" `isInfixOf` s = Draw (red g) (green g) (blue g  + firstInt s)
 
part1 :: [Game] -> Int
part1 ds = sum $ map (\(g,_) -> g) $ filter validDraw ds

validDraw :: Game -> Bool
validDraw (g, ds) = all (== True) $ map valid ds
    where
        valid a = (red a <= 12 && green a <= 13 && blue a <= 14)

part2 :: [Game] -> Int
part2 gs = sum $ map (\mg-> (red mg) * (blue mg) * (green mg)) $ map minDraw gs

minDraw :: Game -> Draw
minDraw (_, gs) = foldl (\t g -> Draw (max (red t) (red g)) (max (green t) (green g)) (max (blue t) (blue g))) (Draw 0 0 0) gs

tidy :: (Int, Int) -> (String, String)
tidy (a,b) = (show a, show b)

