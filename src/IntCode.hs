{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module IntCode where

import BasePrelude
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

intCode :: [Int] -> Maybe Int
intCode code = go 0 codemap where
    codemap = M.fromList $ zip [0..] code
    go i m = M.lookup i m >>= \case
        99 -> M.lookup 0 m
        1 -> binop (+)
        2 -> binop (*)
        q -> Nothing
      where
        get :: [Int] -> Maybe [Int]
        get = mapM (flip M.lookup m)
        binop :: (Int -> Int -> Int) -> Maybe Int
        binop op = do
            [x,y,r] <- get [i+1..i+3]
            [a,b]   <- get [x,y]
            go (i+4) $ M.insert r (op a b) m
