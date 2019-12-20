{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Advent
    ( run
    , day01
    , day02
    , day03
    , day04
    ) where

-- imports
import BasePrelude
import Data.Map.Strict (Map)
import Data.Text (Text)
import System.Directory
import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
-- import qualified Data.Text.Read as T

import Utils
import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)
import Day05 (day05)

type Day = Text -> IO ()

days :: Map String Day
days = M.fromList
    [ mk "01" $ day01 . map decimal . T.lines
    , mk "02" $ day02 19690720 . map decimal . T.splitOn ","
    , mk "03" $ day03 . T.lines
    , mk "04" $ day04 134564 . const 585159
    , mk "05" $ day05 . map signed . T.splitOn ","
    ]
  where
    mk k f = (k, print . f)

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
