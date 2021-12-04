module Day4 (main) where

import Data.List (sortBy)
import Data.Map (Map)
import Data.Ord (comparing)
import Data.Set (Set)
import Misc (check)
import ParE (Par,parse,separated,nl,lit,int,ws0,ws1)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- load "input/day4.input.sam"
  inp <- load "input/day4.input"
  print ("day4, part1(sam)", check 4512 $ part1 sam)
  print ("day4, part1", check 16716 $ part1 inp)
  print ("day4, part2(sam)", check 1924 $ part2 sam)
  print ("day4, part2", check 4880 $ part2 inp)

load :: FilePath -> IO Setup
load path = parse gram <$> readFile path

gram :: Par Setup
gram = setup
  where
    setup = do
      draw <- Draw <$> separated (lit ',') int
      nl2
      boards <- separated nl2 board
      pure $ Setup { draw, boards }
    nl2 = do nl; nl
    board = (Board . concat) <$> separated nl line
    line = do ws0; separated ws1 int

data Setup = Setup { draw :: Draw, boards :: [Board] } deriving Show
data Board = Board [Int] deriving Show
data Draw = Draw [Int] deriving Show

data Pos = Pos Int Int deriving (Eq,Ord)
data Marked = Marked (Set Pos)
data Line = Line (Set Pos)

part1 :: Setup -> Int
part1 Setup{draw,boards} = d*u
  where
    (_,d,u) = head $ sortBy (comparing fst3) [ play draw b | b <- boards ]
    fst3 (x,_,_) = x

part2 :: Setup -> Int
part2 Setup{draw,boards} = d*u
  where
    (_,d,u) = head $ reverse $ sortBy (comparing fst3) [ play draw b | b <- boards ]
    fst3 (x,_,_) = x

induce :: Board -> Map Int Pos
induce (Board nums) =
  Map.fromList (zip nums [ Pos x y | x <- [0..4], y <- [0..4] ])

play :: Draw -> Board -> (Int,Int,Int)
play draw board = res
  where
    boardMapping = induce board
    res = loop 0 marked0 [] draw
    loop :: Int -> Marked -> [Int] -> Draw -> (Int,Int,Int)
    loop i m drawn = \case
      Draw [] -> error "play.loop"
      Draw (d:draw) -> do
        let
          m1 = case Map.lookup d boardMapping of
            Nothing -> m
            Just pos -> mark pos m

        let drawn1 = d:drawn
        if hasLine m1 then (i, d, scoreUnmarked board drawn1) else
          loop (i+1) m1 drawn1 (Draw draw)

marked0 :: Marked
marked0 = Marked Set.empty

scoreUnmarked :: Board -> [Int] -> Int
scoreUnmarked (Board nums) drawn = do
  let d = Set.fromList drawn
  sum [ n | n <- nums, not (n `elem` d) ]

mark :: Pos -> Marked -> Marked
mark p (Marked ps) = Marked (Set.insert p ps)

hasLine :: Marked -> Bool
hasLine (Marked m) = any (m `contains`) allLines

contains :: Set Pos -> Line -> Bool
contains ps (Line xs) = xs `Set.isSubsetOf` ps

allLines :: [Line]
allLines = h ++ v
  where
    h = [ Line $ Set.fromList [ Pos x y | x <- [0..4]] | y <- [0..4] ]
    v = [ Line $ Set.fromList [ Pos x y | y <- [0..4]] | x <- [0..4] ]
