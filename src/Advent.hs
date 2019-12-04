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
import Data.Text (Text)
import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import qualified Data.Text.Read  as T

type Day = Text -> IO ()

run :: Int -> Day -> IO () -- {{{1
run i f = do
    printf "Day %02d:\n" i
    t <- T.readFile (printf "i/%02d" i)
    f t

days :: [ ( Int, Day ) ] -- {{{1
days =
    [ ( 1, print . day01 . map decimal . T.lines )
    , ( 2, print . day02 . map decimal . T.splitOn "," )
    ]

-- parsing helpers {{{1
decimal :: Text -> Int
decimal = either error fst . T.decimal

day01 :: [Int] -> (Int,Int) -- {{{1
day01 xx = (part1,part2) where
    part1 = foldl' (+) 0 (f <$> xx)
    part2 = foldl' (+) 0 (g <$> xx)
    f x = max 0 (div x 3 - 2)
    g x = foldl' (+) 0 (tail $ takeWhile (>0) $ iterate f x)

day02 :: [Int] -> (Int,Int) -- {{{1
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
