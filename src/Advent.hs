{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Advent where

import BasePrelude
import Data.Map.Strict (Map)
import Data.Text (Text)
import System.Directory
import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Text.IO    as T

import Utils
import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)
import Day05 (day05)
import Day06 (day06)
import Day07 (day07)
import Day08 (day08)

type Day = Text -> IO ()

days :: Map String Day
days = M.fromList
    [ mk "01" $ day01 . map decimal . T.lines
    , mk "02" $ day02 19690720 . parseCode
    , mk "03" $ day03 . T.lines
    , mk "04" $ day04 134564 . const 585159
    , mk "05" $ day05 . parseCode
    , mk "06" $ day06 . map (pair . T.splitOn ")") . T.lines
    , mk "07" $ day07 . parseCode
    , (  "08" , out08 . day08 . T.chunksOf (25*6) )
    ]
  where
    mk k f = (k, print . f)
    pair [a,b] = (a,b)
    out08 o@(_,t) = print o >> traverse_ print (T.chunksOf 25 $ T.map f t) where
        f '1' = '#'
        f  q  = ' '

run :: String -> IO ()
run day = case M.lookup day days of
    Nothing -> printf "Invalid day: %s\n" day
    Just f ->
        let path = printf "i/%s" day in
        doesPathExist path >>= \case
            False -> printf "No input file: %s\n" path
            True -> do
                printf "Day %s: " day
                input <- T.readFile path
                f input
