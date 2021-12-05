module Day5 (main) where

import Misc (check)
import Par4 (Par,parse,separated,nl,int,key)
import qualified Data.Map as Map

main :: IO ()
main = do
  sam <- load "input/day5.input.sam"
  inp <- load "input/day5.input"
  print ("day5, part1(sam)", check 5 $ part1 sam)
  print ("day5, part1", check 6687 $ part1 inp)
  print ("day5, part2(sam)", check 12 $ part2 sam)
  print ("day5, part2", check 19851 $ part2 inp)

load :: FilePath -> IO Setup
load path = parse gram <$> readFile path

gram :: Par Setup
gram = separated nl line
  where
    line = do a <- pos; key " -> "; b <- pos; pure (a,b)
    pos = do x <- int; key ","; y <- int; pure (x,y)

type Setup = [Line]
type Line = (Pos,Pos)
type Pos = (Int,Int)

part1 :: [Line] -> Int
part1 lines = do
  let m = Map.fromListWith (+) [ (pos,1::Int) | line <- lines, pos <- expand1 line ]
  length [ n | (_,n) <- Map.toList m, n >=2 ]

part2 :: [Line] -> Int
part2 lines = do
  let m = Map.fromListWith (+) [ (pos,1::Int) | line <- lines, pos <- expand2 line ]
  length [ (p,n) | (p,n) <- Map.toList m, n >=2 ]

expand1 :: Line -> [Pos]
expand1 ((a,b),(c,d)) = do
  let xs = if a < c then [a..c] else [c..a]
  let ys = if b < d then [b..d] else [d..b]
  if a == c then zip (repeat a) ys
  else if b == d then zip xs (repeat b)
  else []

expand2 :: Line -> [Pos]
expand2 ((a,b),(c,d)) = do
  let xs = if a < c then [a..c] else [c..a]
  let ys = if b < d then [b..d] else [d..b]
  if a == c then zip (repeat a) ys
  else if b == d then zip xs (repeat b)
  else if (a < c) == (b < d) then zip xs ys else zip xs (reverse ys)
