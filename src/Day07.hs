module Day07
    ( day07
    ) where

import Lazy
import Data.Ord

type Hand = ([Int], Int)

day07 :: String -> (String, String)
day07 = (lines >>> (map (parse False) >>> part1) &&& (map (parse True) >>> part2) >>> tidy)


parse :: Bool -> String -> Hand
parse j s = (map (scoreCard j) handS, read numS)
    where
        (handS, numS) = break isSpace s

j2 = 0

scoreCard :: Bool -> Char -> Int
scoreCard j c
    | c == '2' = 1
    | c == '3' = 2
    | c == '4' = 3
    | c == '5' = 4
    | c == '6' = 5
    | c == '7' = 6
    | c == '8' = 7
    | c == '9' = 8
    | c == 'T' = 9
    | c == 'J' = if j then j2 else 10
    | c == 'Q' = 11
    | c == 'K' = 12
    | c == 'A' = 13
    | otherwise = error "What?"

handType :: Hand -> Int
handType h
    | hasGroupSize h 5 = 7
    | hasGroupSize h 4 = 6
    | hasGroupSize h 3 && hasGroupSize h 2 = 5
    | hasGroupSize h 3 = 4
    | hasTwoPair h = 3
    | hasGroupSize h 2 = 2
    | hasGroupSize h 1 = 1
    | otherwise = error "Unknown handType"

hasGroupSize :: Hand -> Int -> Bool
hasGroupSize (h,s) n = (>0) $ length $ filter (==n) $ map length $ group $ sort h

hasTwoPair :: Hand -> Bool
hasTwoPair (h,s) = (==2) $ length $ filter (==2) $ map length $ group $ sort h

compareHands :: Hand -> Hand -> Ordering
compareHands h1 h2 = if s1 == s2 then compareCards h1 h2 else compare s1 s2
    where
        s1 = handType h1
        s2 = handType h2

compareHands2 :: Hand -> Hand -> Ordering
compareHands2 h1 h2 = if s1 == s2 then compareCards h1 h2 else compare s1 s2
    where
        s1 = handType $ jokers h1
        s2 = handType $ jokers h2

compareCards :: Hand -> Hand -> Ordering
compareCards ((h1:xs1),s1) ((h2:xs2),s2) = if h1 /= h2 then compare h1 h2 else compareCards (xs1,s1) (xs2,s2)
compareCards ([],_) ([],_) = compare 0 0


part1 :: [Hand] -> Int
part1 s = sum $ zipWith (\n (_,score) -> n*score) [1..1000]  (sortBy compareHands s)

part2 :: [Hand] -> Int
part2 s = sum $ zipWith (\n (_,score) -> n*score) [1..1000]  (sortBy compareHands2 s)

tidy :: (Int, Int) -> (String, String)
tidy (a,b) = (show a, show b)

-- replace the joker with the most common element in the hand, unless it's jjjjj then it's already a 5
jokers :: Hand -> Hand
jokers hand@(cards,bid)
    | jCount == 5 = hand
    | jCount > 0 = (replaceAll j2 nextBest cards , bid)
    | otherwise = hand
    where
        jCount = length $ filter (==j2) cards
        nextBest = snd $ minimumBy (comparing Down) $ map (\g -> (length g, head g)) $ group $ sort $ filter (/=j2) cards 
