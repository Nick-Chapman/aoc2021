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
      called <- Called <$> separated (lit ',') int
      nl2
      boards <- separated nl2 board
      pure $ Setup { called, boards }
    nl2 = do nl; nl
    board = (Board . concat) <$> separated nl line
    line = do ws0; separated ws1 int

data Setup = Setup { called :: Called, boards :: [Board] } deriving Show
data Board = Board [Int] deriving Show
data Called = Called [Int] deriving Show

data Drawn = Drawn (Set Int)
data Pos = Pos Int Int deriving (Eq,Ord)
data Marked = Marked (Set Pos)
data Line = Line (Set Pos)
data Outcome = Outcome { gameLength :: Int, score :: Int }

part1 :: Setup -> Int
part1 Setup{called,boards} = score
  where
    Outcome{score} = head $ sortBy (comparing gameLength) [ play called b | b <- boards ]

part2 :: Setup -> Int
part2 Setup{called,boards} = score
  where
    Outcome{score} = head $ reverse $ sortBy (comparing gameLength) [ play called b | b <- boards ]

play :: Called -> Board -> Outcome
play (Called xs) board = res
  where
    mapping = induce board
    res = loop 0 (Marked Set.empty) (Drawn Set.empty) xs
    loop :: Int -> Marked -> Drawn -> [Int] -> Outcome
    loop i m drawn = \case
      [] -> error "play.loop"
      x:xs -> do
        let m1 = case Map.lookup x mapping of Nothing -> m; Just pos -> mark pos m
        let drawn1 = draw x drawn
        if wins m1
          then Outcome { gameLength = i, score = x * scoreUnmarked board drawn1}
          else loop (i+1) m1 drawn1 xs

induce :: Board -> Map Int Pos
induce (Board nums) = Map.fromList (zip nums [ Pos x y | x <- [0..4], y <- [0..4] ])

mark :: Pos -> Marked -> Marked
mark p (Marked ps) = Marked (Set.insert p ps)

draw :: Int -> Drawn -> Drawn
draw x (Drawn xs) = Drawn (Set.insert x xs)

wins :: Marked -> Bool
wins (Marked m) = any (m `contains`) allLines
  where
    contains :: Set Pos -> Line -> Bool
    contains ps (Line xs) = xs `Set.isSubsetOf` ps

    allLines :: [Line]
    allLines = h ++ v
      where
        h = [ Line $ Set.fromList [ Pos x y | x <- [0..4]] | y <- [0..4] ]
        v = [ Line $ Set.fromList [ Pos x y | y <- [0..4]] | x <- [0..4] ]

scoreUnmarked :: Board -> Drawn -> Int
scoreUnmarked (Board nums) (Drawn drawn) = sum [ n | n <- nums, not (n `elem` drawn) ]
