{-# LANGUAGE NoImplicitPrelude #-}

module Day08 where

import BasePrelude
import Data.Text (Text)
import qualified Data.Text as T

day08 :: [Text] -> (Int,Text)
day08 layers =
    (part1,part2)
  where
    part1 = count '1' zeros * count '2' zeros
    zeros = minimumBy (comparing $ count '0') layers
    count c = T.foldl' f 0 where f z d = if c == d then succ z else z
    part2 = foldl1' comp (reverse layers)
    comp lo hi = T.zipWith pixel lo hi
    pixel u '2' = u
    pixel _  v  = v
