{-# LANGUAGE OverloadedStrings #-}


module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as B
import Data.Text.Read as R
import Data.Either


data RPS =
    Rock | Paper | Scissors 
    deriving Show


data Resolution = Win RPS | Tied RPS | Lost RPS
    deriving Show

isWon :: RPS -> RPS -> (Resolution, Resolution)
isWon  Rock Paper          = (Lost Rock, Win Paper )
isWon  Paper Rock          = (Win Paper, Lost Rock)
isWon  Rock Scissors       = (Win Rock, Lost Scissors)
isWon  Scissors Rock       = (Lost Scissors, Win Rock)
isWon  Paper Scissors      = (Lost Paper, Win Scissors)
isWon  Scissors Paper      = (Win Scissors, Lost Paper)
isWon  Rock Rock           = (Tied Rock, Tied Rock)
isWon  Scissors Scissors   = (Tied Scissors, Tied Scissors)
isWon  Paper Paper         = (Tied Paper, Tied Paper)

score :: (Resolution,Resolution) -> Int
--perdidas
score (Win _,Lost Rock)              = 1
score (Win _,Lost Paper)             = 2
score (Win _,Lost Scissors)          = 3
--empates
score (Tied Rock, Tied Rock)         = 4
score (Tied Paper, Tied Paper)       = 5
score (Tied Scissors, Tied Scissors) = 6
--ganes
score (Lost _,Win Rock)              = 7
score (Lost _,Win Paper)             = 8
score (Lost _,Win Scissors)          = 9


parseRPS:: T.Text -> RPS
parseRPS value  
    | value == "A" || value == "X" = Rock
    | value == "B" || value == "Y" = Paper
    | value == "C" || value == "Z" = Scissors

parsePlay:: T.Text -> (Resolution,Resolution)
parsePlay value  
    | value == "A X" = (Win Rock, Lost Scissors)
    | value == "B X" = (Win Paper, Lost Rock)
    | value == "C X" = (Win Scissors, Lost Paper)

    | value == "A Y" = (Tied Rock,Tied Rock)
    | value == "B Y" = (Tied Paper,Tied Paper)
    | value == "C Y" = (Tied Scissors,Tied Scissors)

    | value == "A Z" = (Lost Rock, Win Paper)
    | value == "B Z" = (Lost Paper, Win Scissors)
    | value == "C Z" = (Lost Scissors, Win Rock)

sumInputList :: T.Text -> [Int]
sumInputList text = 
    map sum
    $ map (\ls -> map sum ls)
    $ filter (\ri -> length ri > 0 )
    $ map (\li -> map (fst <$>) li ) 
    $ map (\c -> filter isRight $ map R.decimal c)
    $ map (\n -> T.splitOn "\n" n) (T.splitOn "\n\n" text ) 


-- C X win Sccicors , lost paper
-- C Z lost scisors, win rock
-- A X
-- B X
-- B Y

main :: IO ()
main =
    -- day 1
    --B.readFile "input/day1.txt" >>= \text -> 
       --print $ reverse $ sort $ sumInputList text 
    -- day 2
--part 1
    -- B.readFile "input/day2.txt" >>= \text -> 
        -- print 
       -- $ foldr (+) 0
       -- $ map (\pares -> score pares)
       -- $ map (\pares -> isWon (head pares) (head $ tail pares))
       -- $ map (\pares -> map (\par -> parseRPS par) pares)
       -- $ filter (\ri -> length ri > 0 )
       -- $ map (\pares -> filter (\par -> not $ T.null par ) pares)
       -- $ map (\p -> T.splitOn " " p)  (T.splitOn "\n" text )
-- part 2 
    B.readFile "input/day2.txt" >>= \text -> 
        print 
         $ foldr (+) 0
         $ map (\pares -> score pares)
       $ map (\par -> parsePlay par)
       -- $ filter (\ri -> length ri > 0 )
       $ filter (\par -> not $ T.null par )
       $ T.splitOn "\n" text 
