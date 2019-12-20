{-# LANGUAGE NoImplicitPrelude #-}

module Day05 where

import BasePrelude

import IntCode

day05 :: [Int] -> (Int,Int)
day05 code = (part1,part2) where
    part1 = out $ intCode code [1]
    part2 = out $ intCode code [5]
    out = head . snd . fromJust
