{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module IntCode where

import BasePrelude
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Output = [Either (Maybe Int) Int]

intCode :: [Int] -> [Int] -> Output
intCode code input = go 0 codemap input where
    codemap = M.fromList $ zip [0..] code
    go c m i = case lookup c of
        Nothing -> [Left Nothing]
        Just x ->
            let (op,addr) = decode x in case op of
                99 -> halt
                1 -> get' 2 addr $ binop (+)
                2 -> get' 2 addr $ binop (*)
                3 -> get' 0 addr $ read
                4 -> get  1 addr $ write
                5 -> get  2 addr $ jumpif id
                6 -> get  2 addr $ jumpif not
                7 -> get' 2 addr $ cmp (<)
                8 -> get' 2 addr $ cmp (==)
                q -> [Left Nothing]
      where
        lookup = flip M.lookup m
        imm = lookup
        pos = lookup >=> lookup
        decode x = (op, addr modes) where
            (modes,op) = divMod x 100
            addr r = bool pos imm (odd r) : addr (div r 10)
        get' n addr = get (n+1) (take n addr ++ [imm])
        get n addr f = case zipWithM ($) addr [c+1..c+n] of
            Nothing -> [Left Nothing]
            Just pp -> f pp
        binop op [a,b,r] = go (c+4) m' i where
            m' = M.insert r (op a b) m
        cmp op = binop (\a b -> bool 0 1 $ op a b)
        halt = [Left . Just $ m M.! 0] -- for Day02
        read [r] = go (c+2) m' i' where
            m' = M.insert r x m
            (x:i') = i
        write [o] = Right o : go (c+2) m i
        jumpif f [b,d] = go (if f (b /= 0) then d else c+3) m i
