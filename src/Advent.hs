-- vim: foldmethod=marker
-- pragmas {{{1
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Advent where -- {{{1

-- imports {{{1
import BasePrelude
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Linear.V2 (V2(..))
import System.Directory
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import qualified Data.Text.Read  as T

import Day03 (day03)

type Day = Text -> IO ()

days :: Map String Day -- {{{1
days = M.fromList
    [ mk "01" $ day01 . map decimal . T.lines
    , mk "02" $ day02 . map decimal . T.splitOn ","
    , mk "03" $ day03 . T.lines
    , mk "04" $ day04 134564 . const 585159
    ]
  where
    mk k f = (k, print . f)

run :: String -> IO () -- {{{1
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


-- parsing helpers {{{1
decimal :: Text -> Int
decimal = either error fst . T.decimal

-- https://adventofcode.com/2019/day/1/ {{{1
day01 :: [Int] -> (Int,Int)
day01 xx = (part1,part2) where
    part1 = foldl' (+) 0 (f <$> xx)
    part2 = foldl' (+) 0 (g <$> xx)
    f x = max 0 (div x 3 - 2)
    g x = foldl' (+) 0 (tail $ takeWhile (>0) $ iterate f x)

-- https://adventofcode.com/2019/day/2/ {{{1
day02 :: [Int] -> (Int,Int)
day02 xx = 
    (part1,part2)
  where
    part1 = fromJust $ trial 12 2
    part2 = head
        [ 100 * a + b
        | a <- [0..99]
        , b <- [0..99]
        , trial a b == Just 19690720 ] -- MAGIC
    trial a b = run 0 $ M.insert 1 a $ M.insert 2 b m0
    m0 = M.fromList $ zip [0..] xx
    run :: Int -> Map Int Int -> Maybe Int
    run i m = M.lookup i m >>= \case
        99 -> M.lookup 0 m
        1 -> opcode (+)
        2 -> opcode (*)
        q -> error $ printf "invalid opcode at %d: %d" i q
      where
        get :: [Int] -> Maybe [Int]
        get = mapM (flip M.lookup m)
        opcode :: (Int -> Int -> Int) -> Maybe Int
        opcode op = do
            [x,y,z] <- get [i+1..i+3]
            [a,b]   <- get [x,y]
            run (i+4) $ M.insert z (op a b) m

-- day 04 {{{1
day04 :: Int -> Int -> (Int,Int)
day04 lo hi = (length cands, length $ filter id cands)
  where
    cands =
        [ any (==2) runs
        | a <- [1..9]
        , b <- [a..9]
        , c <- [b..9]
        , d <- [c..9]
        , e <- [d..9]
        , f <- [e..9]
        , let n = f + 10 * (e + 10 * (d + 10 * (c + 10 * (b + 10 * a))))
        , n >= lo && n <= hi
        , let runs = length <$> group [a,b,c,d,e,f]
        , any (>=2) runs
        ]
