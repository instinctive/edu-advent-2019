{-# LANGUAGE NoImplicitPrelude #-}

module Day09 where

import BasePrelude

import IntCode

day09 :: [Int] -> (Int,Int)
day09 code = (part1,part2) where
    part1 = head . rights $ intCode code [1]
    part2 = 0
    -- part2 = head . rights $ intCode code [5]

