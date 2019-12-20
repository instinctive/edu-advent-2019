{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module IntCode where

import BasePrelude
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Output = Maybe (Int,[Int])

intCode :: [Int] -> [Int] -> Output
intCode code input = go 0 codemap input [] where
    codemap = M.fromList $ zip [0..] code
    go c m i o = M.lookup c m >>= \case
        99 -> (,o) <$> M.lookup 0 m
        1 -> binop (+)
        2 -> binop (*)
        q -> Nothing
      where
        get :: [Int] -> Maybe [Int]
        get = mapM (flip M.lookup m)
        binop :: (Int -> Int -> Int) -> Output
        binop op = do
            [x,y,r] <- get [c+1..c+3]
            [a,b]   <- get [x,y]
            let m' = M.insert r (op a b) m
            go (c+4) m' i o
