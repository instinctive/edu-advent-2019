{-# LANGUAGE NoImplicitPrelude #-}

module Day07 where

import BasePrelude

import IntCode

day07 :: [Int] -> (Int,Int)
day07 code = (part1,part2) where
    part1 = maximum $ foldl' f 0 <$> permutations [0..4] where 
        f i p = o where
            Just (_,(o:_)) = intCode code [p,i]
    part2 = 0
