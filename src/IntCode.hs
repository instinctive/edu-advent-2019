{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module IntCode where

import BasePrelude
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Output = Maybe (Int,[Int])
data Addr = Pos | Imm deriving (Eq,Ord,Show)

decode :: Int -> (Int,[Addr])
decode x = (op, addr raw) where
    (raw,op) = divMod x 100
    addr r = bool Pos Imm (odd r) : addr (div r 10)

intCode :: [Int] -> [Int] -> Output
intCode code input = go 0 codemap input [] where
    codemap = M.fromList $ zip [0..] code
    go c m i o = M.lookup c m >>= \opcode -> 
        let (op,addr) = decode opcode in case op of
            99 -> halt
            1 -> binop (+) addr
            2 -> binop (*) addr
            q -> Nothing
      where
        lookup = flip M.lookup m
        address Imm = Just
        address Pos = lookup
        get :: [Int] -> Maybe [Int]
        get = mapM lookup
        halt :: Output
        halt = (,o) <$> lookup 0
        binop :: (Int -> Int -> Int) -> [Addr] -> Output
        binop op (x:y:_) = do
            a <- lookup (c+1) >>= address x
            b <- lookup (c+2) >>= address y
            r <- lookup (c+3)
            let m' = M.insert r (op a b) m
            go (c+4) m' i o
