module Day15 (main) where

import Data.Map (Map)
import Data.Maybe (maybeToList,fromJust)
import Data.Set (Set)
import Misc (check)
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

bottomRightCorner :: Maze -> Pos
bottomRightCorner maze = do
  let mx = maximum [ x | (x,_) <- Map.keys maze ]
  let my = maximum [ y | (_,y) <- Map.keys maze ]
  (mx,my)

solve :: Maze -> Int
solve maze = do
  let waves = explore maze
  let goal = bottomRightCorner maze
  let reachGoal State{frontier} = goal `Set.member` snd (Map.findMin frontier)
  let findGoal State{frontier} = fst $ Map.findMin frontier
  findGoal $ head $ dropWhile (not . reachGoal) waves

data State = State { visited :: Set Pos
                   , frontier :: Map Int (Set Pos) } deriving Show

explore :: Maze -> [State]
explore m = iterate (step m) (start m)

start :: Maze -> State
start _m = State
  { visited = Set.fromList [start]
  , frontier = Map.fromList [ (0, Set.fromList [start]) ]
  }
  where start = (0,0)

step :: Maze -> State -> State
step maze State{visited=visited0,frontier=frontier0} = State{visited=visited1,frontier=frontier1}
  where
    ((minK,pointsToExpand),frontier0') = fromJust $ Map.minViewWithKey frontier0

    newPoints = [ q | p <- Set.toList pointsToExpand , q <- adjacent p , q `Set.notMember` visited0 ]

    newFrontier =
      Map.fromListWith Set.union
      [ (v + minK, Set.fromList [q])
      | q <- newPoints
      , v <- maybeToList $ Map.lookup q maze
      ]

    visited1 = Set.union visited0 (Set.fromList newPoints)
    frontier1 = Map.unionWith Set.union newFrontier frontier0'

adjacent :: Pos -> [Pos]
adjacent (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
