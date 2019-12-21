{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module IntCode where

import BasePrelude
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Output = [Either (Maybe Int) Int]
type Mode = Int -> Int
type Op = [Int] -> Output

intCode :: [Int] -> [Int] -> Output
intCode code input = go 0 0 codemap input where
    codemap = M.fromList $ zip [0..] code
    go base c m i = case op of
        99 -> halt
        0 -> error "zero opcode"
        1 -> get' 2 addr $ binop (+)
        2 -> get' 2 addr $ binop (*)
        3 -> get' 0 addr $ read
        4 -> get  1 addr $ write
        5 -> get  2 addr $ jumpif id
        6 -> get  2 addr $ jumpif not
        7 -> get' 2 addr $ cmp (<)
        8 -> get' 2 addr $ cmp (==)
        9 -> get  1 addr $ offset
        q -> error $ printf "invalid opcode: %d" q
      where
        op :: Int
        addr :: [Mode]
        (op,addr) = second f (swap $ divMod (lookup c) 100) where
            f modes = let (modes',mode) = divMod modes 10 in mk mode : f modes'
            mk 0 = pos
            mk 1 = imm
            mk 2 = rel
            mk q = error $ printf "invalid address mode: %d" q
        lookup :: Int -> Int
        lookup i | i < 0 = error $ printf "negative address: %d" i
        lookup i = M.findWithDefault 0 i m
        imm,pos,rel :: Mode
        imm = lookup
        pos = lookup . lookup
        rel = lookup . (+base) . lookup
        get',get :: Int -> [Mode] -> Op -> Output
        get' n addr = get (n+1) (take n addr ++ [imm])
        get  n addr f = f $ zipWith ($) addr [c+1..c+n]
        halt :: Output
        halt = [Left . Just $ lookup 0] -- for Day02
        binop :: (Int -> Int -> Int) -> Op
        binop op [a,b,r] = go base (c+4) m' i where
            m' = M.insert r (op a b) m
        cmp :: (Int -> Int -> Bool) -> Op
        cmp op = binop (\a b -> bool 0 1 $ op a b)
        jumpif :: (Bool -> Bool) -> Op
        jumpif f [b,d] = go base (if f (b /= 0) then d else c+3) m i
        read,write,offset :: Op
        read [r] = go base (c+2) m' i' where
            m' = M.insert r x m
            (x:i') = i
        write [o] = Right o : go base (c+2) m i
        offset [delta] = go (base + delta) (c+2) m i
