{-# LANGUAGE NoImplicitPrelude #-}

module Day07 where

import BasePrelude

import IntCode

day07 :: [Int] -> (Int,Int)
day07 code = (part1,part2) where
    part1 = maximum $ foldl' f 0 <$> permutations [0..4] where 
        f i p = head . rights $ intCode code [p,i]
    part2 = maximum
        [ last f
        | [p0,p1,p2,p3,p4] <- permutations [5..9]
        , let a = (0:f)
              b = rights $ intCode code (p0:a)
              c = rights $ intCode code (p1:b)
              d = rights $ intCode code (p2:c)
              e = rights $ intCode code (p3:d)
              f = rights $ intCode code (p4:e)
        ]
