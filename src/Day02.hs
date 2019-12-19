{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day02 where

import BasePrelude
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

day02 :: Int -> [Int] -> (Int,Int)
day02 magic xx = (part1,part2) where
    part1 = fromJust $ trial 12 2
    part2 = head
        [ 100 * a + b
        | a <- [0..99]
        , b <- [0..99]
        , trial a b == Just magic ]
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
