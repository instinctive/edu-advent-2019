{-# LANGUAGE NoImplicitPrelude #-}

module Day02 where

import BasePrelude

import IntCode

day02 :: Int -> [Int] -> (Int,Int)
day02 magic xx = (part1,part2) where
    part1 = fromJust $ trial 12 2
    part2 = head
        [ 100 * a + b
        | a <- [0..99]
        , b <- [0..99]
        , trial a b == Just magic ]
    z:_:_:zz = xx
    trial a b = fst <$> intCode (z:a:b:zz) []
