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
            3 -> read
            4 -> write addr
            5 -> jumpif id  addr
            6 -> jumpif not addr
            7 -> cmp ( <) addr
            8 -> cmp (==) addr
            q -> Nothing
      where
        lookup = flip M.lookup m
        address Imm = Just
        address Pos = lookup
        cmp op = binop (\a b -> bool 0 1 $ op a b)
        get :: [Int] -> Maybe [Int]
        get = mapM lookup
        halt :: Output
        halt =
            (,o) <$> lookup 0
        binop :: (Int -> Int -> Int) -> [Addr] -> Output
        binop op (x:y:_) = do
            a <- lookup (c+1) >>= address x
            b <- lookup (c+2) >>= address y
            r <- lookup (c+3)
            let m' = M.insert r (op a b) m
            go (c+4) m' i o
        read :: Output
        read = do
            r <- lookup (c+1)
            let (x:xx) = i
            let m' = M.insert r x m
            go (c+2) m' xx o
        write :: [Addr] -> Output
        write (x:_) = do
            v <- lookup (c+1) >>= address x
            go (c+2) m i (v:o)
        jumpif :: (Bool -> Bool) -> [Addr] -> Output
        jumpif f (x:y:_) = do
            b <- lookup (c+1) >>= fmap (f . (/=0)) . address x
            d <- lookup (c+2) >>= address y
            go (if b then d else c+3) m i o
