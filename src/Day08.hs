module Day08
    ( day08
    ) where

import Lazy

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.State
import Data.Either
import qualified Data.Map as M

import Data.Void

data LR =  L | R deriving (Eq, Show)

type Node = String
type Choice = (Node, Node)
type Row = (Node, Choice)

type Parser = Parsec Void String

type NMap = M.Map Node Choice

pLR :: Parser LR
pLR = choice
  [ L <$ string "L"
  , R <$ string "R"]

pLRRow :: Parser [LR]
pLRRow = do
    res <- many pLR
    void space1
    return res

pNode :: Parser Node
pNode = do
     many upperChar


pColourScore :: Parser Row
pColourScore = do
    k <- pNode
    void (string " = (")
    l <- pNode
    void (string ", ")
    r <- pNode
    void (string ")\n")
    return (k,(l,r))

pColourScores :: Parser [Row]
pColourScores = do
    many pColourScore

pInput :: Parser ([LR], [Row])
pInput = do
    l <- pLRRow
    r <- pColourScores
    return (l,r)

day08 :: String -> (String, String)
day08 s = tidy (part1 l nMap, part2 l nMap)
    where
        (l,r) = fromRight ([],[]) $ runParser pInput "" s
        nMap = M.fromList r


part1 :: [LR] -> NMap -> Int
part1 d m = countDistance 0 (cycle d) m "AAA" 

countDistance :: Int -> [LR] -> NMap -> Node -> Int
countDistance steps (h:xs) nMap cur
    | cur == "ZZZ" = steps
    | h == L = countDistance (steps+1)  xs nMap (getNext nMap L cur)
    | h == R = countDistance (steps+1)  xs nMap (getNext nMap R cur)
    | otherwise = error "count distance error"

countDistance2 :: Int -> [LR] -> NMap -> Node -> Int
countDistance2 steps (h:xs) nMap cur
    | "Z" `isSuffixOf` cur = steps
    | h == L = countDistance2 (steps+1)  xs nMap (getNext nMap L cur)
    | h == R = countDistance2 (steps+1)  xs nMap (getNext nMap R cur)
    | otherwise = error "count distance error"

part2 :: [LR] -> NMap -> Int
part2 d m = lcmm $ map (countDistance2 0 (cycle d) m) startNodes
    where
        startNodes = getStartNodes m

getStartNodes :: NMap -> [Node]
getStartNodes = M.toList >>> map fst >>> filter ("A" `isSuffixOf`)

lcmm :: Integral a => [a] -> a
lcmm [] = 1
lcmm (x:xs) = lcm x (lcmm xs)

getNext :: NMap -> LR -> Node -> Node
getNext nMap d n
    | d == L = fst (nMap M.! n)
    | d == R = snd (nMap M.! n)

tidy :: (Int, Int) -> (String, String)
tidy (a,b) = (show a, show b)

