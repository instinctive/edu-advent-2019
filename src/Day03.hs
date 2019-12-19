{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day03 where

import BasePrelude
import Data.Map.Strict (Map)
import Data.Set (Set)
import Linear.V2 (V2(..))
import Data.Text (Text)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Text       as T
import qualified Data.Text.Read  as T

import Utils

type Loc = V2 Int
data Dir = U | D | L | R deriving (Eq,Ord,Read,Show)
type Move = (Dir,Int)

parse :: Text -> Move
parse t = (read $ [T.head t], decimal $ T.tail t)

step :: Dir -> Loc
step U = V2 (-1) ( 0)
step D = V2 ( 1) ( 0)
step L = V2 ( 0) (-1)
step R = V2 ( 0) ( 1)

mkLocs :: Loc -> [Move] -> [Loc]
mkLocs v [] = [v]
mkLocs v ((d,0):pp) = mkLocs v pp
mkLocs v ((d,n):pp) = 
    let v' = v + step d in
    v : mkLocs v' ((d,n-1):pp)

day03 :: [Text] -> (Int,Int)
day03 raw = (part1,part2) where
    zero = V2 0 0
    [aa,bb] = tail . mkLocs zero . map parse . T.splitOn "," <$> raw
    part1 = 
        minimum . map (sum.abs) . S.elems $
        S.intersection (S.fromList aa) (S.fromList bb)
    part2 =
        minimum . M.elems $
        M.intersectionWith (+) (mk aa) (mk bb)
      where mk = M.fromListWith min . flip zip [1..]
