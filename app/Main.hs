{-# LANGUAGE OverloadedStrings #-}


module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as B
import Data.Text.Read as R
import Data.Either
import Control.Monad.IO.Class
import Data.List (sort)


sumInputList :: T.Text -> [Int]
sumInputList text = 
    map sum
    $ map (\ls -> map sum ls)
    $ filter (\ri -> length ri > 0 )
    $ map (\li -> map (fst <$>) li ) 
    $ map (\c -> filter isRight $ map R.decimal c)
    $ map (\n -> T.splitOn "\n" n) (T.splitOn "\n\n" text ) 


say :: MonadIO m => T.Text -> m ()
say = liftIO . print

main :: IO ()
main =
    B.readFile "input/day1.txt" >>= \text -> 
       print $ reverse $ sort $ sumInputList text 

        
