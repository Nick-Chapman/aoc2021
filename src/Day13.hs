module Day13 (main) where

import Data.Set (Set)
import Misc (check)
import Par4 (Par,parse,separated,terminated,nl,int,lit,key,alts)
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- load "input/day13.input.sam"
  inp <- load "input/day13.input"
  print ("day13, part1(sam)", check 17 $ part1 sam)
  print ("day13, part1", check 775 $ part1 inp)
  part2 sam
  part2 inp

load :: FilePath -> IO Setup
load path = parse gram <$> readFile path

gram :: Par Setup
gram = do
  xs <- Set.fromList <$> terminated nl pos
  nl; is <- separated nl instr; pure (xs,is)
  where
    pos = do x <- int; lit ','; y <- int; pure (x,y)
    instr = do key "fold along "; a <- axis; lit '='; n <- int; pure (a,n)
    axis = alts [ do lit 'x'; pure X, do lit 'y'; pure Y]

type Setup = (Set Pos, [Instr])
type Pos = (Int,Int)
type Instr = (Axis,Int)
data Axis = X | Y deriving Show

part1 :: Setup -> Int
part1 (ps,is) = Set.size $ step ps (head is)

part2 :: Setup -> IO ()
part2 (ps,is) = draw $ foldl step ps is

step :: Set Pos -> Instr -> Set Pos
step ps instr = Set.fromList [ applyInstruction p | p <- Set.toList ps ]
  where
    applyInstruction (x,y) = case instr of
      (Y,n) -> (x, if y < n then y else 2*n - y )
      (X,n) -> (   if x < n then x else 2*n - x, y )

draw :: Set Pos -> IO ()
draw ps = putStrLn res
  where
    mx = maximum [ x | (x,_) <- Set.toList ps ]
    my = maximum [ y | (_,y) <- Set.toList ps ]
    res = unlines (map mkLine [0..my])
    mkLine y = [ pos x y | x <- [0..mx] ]
    pos x y = if (x,y) `Set.member` ps then '#' else '.'
