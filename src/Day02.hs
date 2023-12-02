module Day02
    ( day02
    ) where

import Lazy

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.State
import Data.Either

import Data.Void

data Colour =  Red | Green | Blue deriving (Eq, Show)

data Draw = Draw {
    red :: Int,
    green :: Int,
    blue :: Int
} deriving (Eq, Show)

type Game = (Int, [Draw])

type Parser = Parsec Void String

pColour :: Parser Colour
pColour = choice
  [ Red   <$ string "red"
  , Green <$ string "green"
  , Blue  <$ string "blue" ]

pColourScore :: Parser (Colour, Int)
pColourScore = do
    optional (char ' ')
    i <- L.decimal
    optional (char ' ')
    c <- pColour
    return (c,i)

asDraw :: Draw -> (Colour, Int) -> Draw
asDraw d (c,i)
            | c == Red = Draw i (green d) (blue d)
            | c == Green = Draw (red d) i (blue d)
            | c == Blue = Draw (red d) (green d) i

pDraw :: Parser Draw
pDraw = do
    p <- pColourScore `Text.Megaparsec.sepBy` (char ',')
    return $ foldl asDraw (Draw 0 0 0) p

pGame :: Parser Game
pGame = do
    void (string "Game ")
    gameNum <- L.decimal
    void (char ':')
    draws <- pDraw `Text.Megaparsec.sepBy` (char ';')
    void (newline)
    return (gameNum, draws)

parseGames :: Parser [Game]
parseGames = do
    many pGame

day02 :: String -> (String, String)
day02 s = tidy (part1 games, part2 games)
    where
        games = fromRight [(0,[])] $ runParser parseGames "" s
    
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

