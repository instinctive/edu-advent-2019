{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module IntCode where

import BasePrelude
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Output = [Either (Maybe Int) Int]
type Op = [Int] -> Output
data Param = I | O deriving (Eq,Ord,Show)
data Mode = Pos | Imm | Rel deriving (Eq,Ord,Show)

intCode :: [Int] -> [Int] -> Output
intCode code input = go 0 0 codemap input where
    codemap = M.fromList $ zip [0..] code
    go base c m i = case op of
        99 -> halt
        0 -> error "zero opcode"
        1 -> {- traceShow "binop+" $ -} get [I,I,O] $ binop (+)
        2 -> {- traceShow "binop*" $ -} get [I,I,O] $ binop (*)
        3 -> {- traceShow "read  " $ -} get [O]     $ read
        4 -> {- traceShow "write " $ -} get [I]     $ write
        5 -> {- traceShow "jump  " $ -} get [I,I]   $ jumpif id
        6 -> {- traceShow "jump -" $ -} get [I,I]   $ jumpif not
        7 -> {- traceShow "cmp lt" $ -} get [I,I,O] $ cmp (<)
        8 -> {- traceShow "cmp eq" $ -} get [I,I,O] $ cmp (==)
        9 -> {- traceShow "offset" $ -} get [I]     $ offset
        q -> error $ printf "invalid opcode: %d" q
      where
        op :: Int
        addr :: [Mode]
        (op,addr) = second f (swap $ divMod (lookup c) 100) where
            f modes = uncurry (:) . bimap mk f . swap $ divMod modes 10
            mk 0 = Pos; mk 1 = Imm; mk 2 = Rel
            mk q = error $ printf "invalid address mode: %d" q
        lookup :: Int -> Int
        lookup i | i < 0 = error $ printf "negative address: %d" i
        lookup i = M.findWithDefault 0 i m
        get :: [Param] -> Op -> Output
        get pp f = 
            -- traceShow (zip pp addr) $
            f $ zipWith3 mk pp addr [c+1..]
          where
            mk I Imm = lookup
            mk I Pos = lookup . lookup
            mk I Rel = lookup . (+base) . lookup
            mk O Imm = error "immediate mode for output parameter"
            mk O Pos = lookup
            mk O Rel = (+base) . lookup
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
