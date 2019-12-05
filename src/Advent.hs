-- vim: foldmethod=marker
-- pragmas {{{1
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Advent where -- {{{1

-- imports {{{1
import BasePrelude
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Linear.V2 (V2(..))
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import qualified Data.Text.Read  as T

type Day = Text -> IO ()

run :: Int -> Day -> IO () -- {{{1
run i f = do
    printf "Day %02d:\n" i
    t <- T.readFile (printf "i/%02d" i)
    f t

days :: [ ( Int, Day ) ] -- {{{1
days =
    [ ( 1, print . day01 . map decimal . T.lines )
    , ( 2, print . day02 . map decimal . T.splitOn "," )
    , ( 3, print . day03 . T.lines )
    ]

-- parsing helpers {{{1
decimal :: Text -> Int
decimal = either error fst . T.decimal

-- https://adventofcode.com/2019/day/1/ {{{1
day01 :: [Int] -> (Int,Int)
day01 xx = (part1,part2) where
    part1 = foldl' (+) 0 (f <$> xx)
    part2 = foldl' (+) 0 (g <$> xx)
    f x = max 0 (div x 3 - 2)
    g x = foldl' (+) 0 (tail $ takeWhile (>0) $ iterate f x)

-- https://adventofcode.com/2019/day/2/ {{{1
day02 :: [Int] -> (Int,Int)
day02 xx = 
    (part1,part2)
  where
    part1 = fromJust $ trial 12 2
    part2 = head
        [ 100 * a + b
        | a <- [0..99]
        , b <- [0..99]
        , trial a b == Just 19690720 ] -- MAGIC
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

-- https://adventofcode.com/2019/day/3/ {{{1
type Int2 = V2 Int
data UDLR = U | D | L | R deriving (Eq,Ord,Read,Show)
type Dir = (UDLR,Int)

rev03 :: UDLR -> UDLR
rev03 U = D
rev03 D = U
rev03 L = R
rev03 R = L

parse03 :: Text -> Dir
parse03 t = (read $ [T.head t], decimal $ T.tail t)

step03 :: UDLR -> Int2
step03 U = V2 (-1) ( 0)
step03 D = V2 ( 1) ( 0)
step03 L = V2 ( 0) (-1)
step03 R = V2 ( 0) ( 1)

move03 :: Int2 -> Dir -> Int2
move03 v (d,n) = v + fromIntegral n * step03 d

outbound03 :: Int2 -> Dir -> [ (Int2,Dir) ]
outbound03 v@(V2 r c) (d,n)
    | s * v  >= 0 = [ (v , (      d, n)) ]
    | s * v' <= 0 = [ (v', (rev03 d, n)) ]
    | otherwise = case d of
        U -> [ (V2 1 c, (D, r-1))
             , (V2 0 c, (U, n-r)) ]
        D -> [ (V2 1 c, (D, n+r-1))
             , (V2 0 c, (U, -r)) ]
        L -> [ (V2 r 1, (R, c-1))
             , (V2 r 0, (L, n-c)) ]
        R -> [ (V2 r 1, (R, n+c-1))
             , (V2 r 0, (L, -c)) ]
  where
    s = step03 d
    v' = v + s * fromIntegral n

prep03 :: Text -> [(Int2,Dir)]
prep03 t = jump (head ss) : tail ss where
    dd = parse03 <$> T.splitOn "," t
    vv = scanl' move03 (V2 0 0) dd
    oo = concat $ zipWith outbound03 vv dd
    ss = sortBy (comparing $ sum.abs.fst) oo
    -- gg = groupBy (on (==) $ sum.abs.fst) ss

jump (v,(d,n))
    | n > 0 = (v + step03 d, (d,n-1))
    | otherwise = error (show (v,(d,n)))

check03 :: [(Int2,Dir)] -> (Set Int2,[(Int2,Dir)])
check03 qq = (s, yy' ++ nn) where
    (yy,nn) = break ((>d).sum.abs.fst) qq
    d = sum.abs.fst $ head qq
    s = S.fromList $ fst <$> yy
    yy' = f <$> filter ((>0).snd.snd) yy
    f (v,(d,n)) = (v + step03 d, (d,n-1))

day03 :: [Text] -> (Int,Int)
day03 raw = (part1,part2) where
    part1 = go uu vv where [uu,vv] = prep03 <$> raw
    part2 = 0
    go uu vv
        | S.null (S.intersection us vs) = go uu' vv'
        | otherwise = sum.abs.fst $ head uu
      where
        (us,uu') = check03 uu
        (vs,vv') = check03 vv
