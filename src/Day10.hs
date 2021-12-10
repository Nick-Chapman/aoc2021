module Day10 (main) where

import Data.List (sort)
import Misc (check)
import Par4 (Par,parse,separated,nl,lit,alts,some)

main :: IO ()
main = do
  sam <- load "input/day10.input.sam"
  inp <- load "input/day10.input"
  print ("day10, part1(sam)", check 26397 $ part1 sam)
  print ("day10, part1", check 394647 $ part1 inp)
  print ("day10, part2(sam)", check 288957 $ part2 sam)
  print ("day10, part2", check 2380061249 $ part2 inp)

load :: FilePath -> IO Setup
load path = parse gram <$> readFile path

gram :: Par [Line]
gram = separated nl line
  where
    line = some tok
    tok = alts [ t | k <- [R,S,C,A], t <- [open k, close k]]
    open k = do lit (o k); pure $ Open k
    close k = do lit (c k); pure $ Close k
    o = \case R -> '('; S -> '['; C -> '{'; A -> '<'
    c = \case R -> ')'; S -> ']'; C -> '}'; A -> '>'

type Setup = [Line]
type Line = [Tok]
data Tok = Open Kind | Close Kind deriving (Eq,Show)
data Kind = R | S | C | A deriving (Eq,Show) -- Round,Square,Curly,Angle

part1 :: Setup -> Int
part1 lines = sum scores
  where
    scores =
      [ case k of R -> 3; S -> 57; C -> 1197; A -> 25137
      | line <- lines
      , k <- case checkLine line of Incomplete _ -> []; Corrupt k -> [k]
      ]

part2 :: Setup -> Int
part2 lines = middle scores
  where
    scores =
      [ foldl (\acc k -> 5*acc + case k of R -> 1; S -> 2; C -> 3; A -> 4) 0 ks
      | line <- lines
      , ks <- case checkLine line of Incomplete xs -> [xs]; Corrupt _ -> []
      ]

data Res = Corrupt Kind | Incomplete [Kind]

checkLine :: [Tok] -> Res
checkLine = loop []
  where
    loop :: [Kind] -> [Tok] -> Res
    loop stack = \case
      [] -> Incomplete stack
      Open k : rest -> loop (k:stack) rest
      Close k : rest -> case stack of
        [] -> error "unexpected close"
        k':stack -> if k==k' then loop stack rest else Corrupt k

middle :: Ord a => [a] -> a
middle xs = head (drop n (sort xs)) where n = length xs `div` 2
