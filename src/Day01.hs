{-# LANGUAGE NoImplicitPrelude #-}

module Day01 where

import BasePrelude

day01 :: [Int] -> (Int,Int)
day01 xx = (part1,part2) where
    part1 = foldl' (+) 0 (f <$> xx)
    part2 = foldl' (+) 0 (g <$> xx)
    f x = max 0 (div x 3 - 2)
    g x = foldl' (+) 0 (tail $ takeWhile (>0) $ iterate f x)

