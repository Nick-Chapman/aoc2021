module Day15 (main) where

import Data.Map (Map)
import Data.Maybe (maybeToList)
import Data.Set (Set)
import Misc (check,look)
import Par4 (Par,parse,separated,nl,some,digit)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- load "input/day15.input.sam"
  inp <- load "input/day15.input"
  print ("day15, part1(sam)", check 40 $ part1 sam)
  print ("day15, part1", check 462 $ part1 inp)
  print ("day15, part2(sam)", check 315 $ part2 sam)
  print ("day15, part2", check 2846 $ part2 inp)

load :: FilePath -> IO Raw
load path = parse gram <$> readFile path

gram :: Par Raw
gram = separated nl (some digit)

type Raw = [[Int]]
type Maze = Map Pos Int
type Pos = (Int,Int)

part1 :: Raw -> Int
part1 vss = solve maze
  where
    maze = Map.fromList [ ((x,y),v) | (y,vs) <- zip [0..] vss, (x,v) <- zip [0..] vs ]

part2 :: Raw -> Int
part2 vss = solve maze
  where
    my = length vss
    mx = length (head vss)
    maze =
      Map.fromList
      [ ((rx*mx+x,ry*my+y), (v+rx+ry-1) `mod` 9 + 1)
      | rx <- [0..4]
      , ry <- [0..4]
      , (y,vs) <- zip [0..] vss
      , (x,v) <- zip [0..] vs
      ]

solve :: Maze -> Int
solve maze = do
  let mx = maximum [ x | (x,_) <- Map.keys maze ]
  let my = maximum [ y | (_,y) <- Map.keys maze ]
  let goalPos = (mx,my)
  let ys = dropWhile (not . (reached goalPos)) (explore maze)
  pickAnswer (head ys)

data State = State { acc :: Set Pos, frontier :: Map Int (Set Pos) } deriving Show

explore :: Maze -> [State]
explore m = iterate (step m) (start m)

start :: Maze -> State
start _m = State
  { acc = Set.empty
  , frontier = Map.fromList [ (0, Set.fromList [ (0,0) ]) ]
  }

reached :: Pos -> State -> Bool
reached goalPos State{frontier} = goalPos `elem` points
  where
    minK = minimum (Map.keys frontier)
    points = look minK frontier

pickAnswer :: State -> Int
pickAnswer State{frontier} = minimum (Map.keys frontier)

step :: Maze -> State -> State
step maze State{acc=acc0,frontier=frontier0} = State{acc=acc1,frontier=frontier1}
  where
    acc1 = Set.union acc0 pointsToExpand
    frontier1 = Map.unionWith Set.union newFrontier (Map.delete minK frontier0)

    minK = minimum (Map.keys frontier0)
    pointsToExpand = look minK frontier0

    newFrontier = Map.fromListWith Set.union
      [ (v + minK, Set.fromList [q])
      | p <- Set.toList pointsToExpand
      , q <- adjacent p
      , q `Set.notMember` acc0
      , v <- maybeToList $ Map.lookup q maze
      ]

adjacent :: Pos -> [Pos]
adjacent (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
