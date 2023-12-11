module Lib
    ( advent
    ) where

import Lazy
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11

getDay :: Int -> (String -> (String,String))
getDay  1 = day01
getDay  2 = day02
getDay  3 = day03
getDay  4 = day04
getDay  5 = day05
getDay  6 = day06
getDay  7 = day07
getDay  8 = day08
getDay  9 = day09
getDay 10 = day10
getDay 11 = day11
getDay _ = day01

advent :: IO ()
advent = do
           args <- getArgs
           input <- readFile ("Inputs/input_" ++ head args ++ ".txt")
           mapM_ (print . run input) [read (head args)]


run :: String -> Int -> (String, String)
run s day = (pt1, pt2)
    where
        (pt1,pt2) = (getDay day) s
