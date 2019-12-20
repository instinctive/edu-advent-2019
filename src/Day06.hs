{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day06 where

import BasePrelude
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as M

day06 :: [(Text,Text)] -> (Int,Int)
day06 planets = (part1,part2) where
    inner = fst <$> planets
    outer = snd <$> planets
    dmap = M.fromList $ ("COM",(Nothing,0)) : zip outer (mk <$> inner)
        where mk i = (Just i, succ . snd $ dmap M.! i)
    path k = reverse . tail . map fromJust . takeWhile isJust $ iterate f (Just k)
        where f = maybe Nothing (fst . (M.!) dmap)
    part1 = sum . map snd $ M.elems dmap
    part2 = go (path "YOU") (path "SAN") where
        go (a:aa) (b:bb) | a == b = go aa bb
        go aa bb = length aa + length bb
