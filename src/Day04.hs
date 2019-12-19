{-# LANGUAGE NoImplicitPrelude #-}

module Day04 where

import BasePrelude

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
