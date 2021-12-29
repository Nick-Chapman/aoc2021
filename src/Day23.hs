
module Day23 (main) where

import Data.Map (Map)
import Data.Maybe (fromJust,listToMaybe)
import Data.Set (Set)
import Misc (check)
import Text.Printf (printf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  let
    _sam =
      [(A,[B,A])
      ,(B,[C,D])
      ,(C,[B,C])
      ,(D,[D,A])
      ]
    _inp =
      [(A,[D,B])
      ,(B,[D,A])
      ,(C,[C,A])
      ,(D,[B,C])
      ]
    _sam2 =
      [(A,[B,D,D,A])
      ,(B,[C,C,B,D])
      ,(C,[B,B,A,C])
      ,(D,[D,A,C,A])
      ]
    _inp2 =
      [(A,[D,D,D,B])
      ,(B,[D,C,B,A])
      ,(C,[C,B,A,A])
      ,(D,[B,A,C,C])
      ]

  let
    _part1 = search 2
    _part2 = search 4

  s1 <- _part1 _sam
  print ("day23, part1(sam)", check 12521 $ s1) -- 0.3

  i1 <- _part1 _inp
  print ("day23, part1", check 16157 $ i1) -- 0.9

--  s2 <- _part2 _sam2
--  print ("day23, part2(sam)", check 44169 $ s2) -- 10

  i2 <- _part2 _inp2
  print ("day23, part2", check 43481 $ i2) -- 13


-- A* search

search :: Int -> Setup -> IO Cost
search d iState = do
  let
    state0 = initState iState
  let
    reachGoal :: SS -> Maybe (Path,Cost)
    reachGoal SS{frontier} = do
      let (_c,edges) :: (Cost,Set (Path,Cost,State)) = Map.findMin frontier
      listToMaybe [ (p,c) | _e@(p,c,q) <- Set.toList edges, isComplete q ]

  let ss0 = SS
        { expanded = Set.empty
        , frontier = Map.fromList [ ( lowerBoundCostToComplete state0
                                    , Set.fromList [([],0,state0)]) ]
        }
  let
    loop :: Int -> SS -> IO Cost
    loop i ss = do
      --putStrLn (show i ++ "..." ++ show ss)
      case reachGoal ss of
        Nothing -> loop (i+1) (step d ss)
        Just (_path,cost) -> do
          --mapM_ print (reverse _path)
          pure cost

  loop 0 ss0


step :: Int -> SS -> SS
step d SS{expanded=expanded0,frontier=frontier0} = SS {expanded, frontier}

  where
    edges :: Set (Path, Cost, State)
    frontier0' :: Map Cost (Set (Path, Cost, State))
    (edges,frontier0') = fromJust $ Map.minView frontier0

    newStateEdges =
      [ e
      | e@(_, _, p) <- Set.toList edges
      , p `Set.notMember` expanded0
      ]

    newFrontier :: Map Cost (Set (Path,Cost,State))
    newFrontier =
      Map.fromListWith Set.union
      [ (a + b , Set.fromList [(ms++path,a, q)])
      | (path, a1, p) <- newStateEdges
      , (ms,a2,q) <- legalMoves d p
      , let a = a1+a2
      , let b = lowerBoundCostToComplete q
      ]

    newS = map (\(_,_,q) -> q) newStateEdges
    expanded = Set.union expanded0 (Set.fromList newS)

    frontier = Map.unionWith Set.union newFrontier frontier0'


data SS = SS { expanded :: Set State , frontier :: Map Cost (Set Edge) }

instance Show SS where
  show SS{expanded,frontier} =
    show (Set.size expanded, length (Map.keys frontier)
         , (fst (Map.findMin frontier), fst (Map.findMax frontier)))

type Setup = [(Type,[Type])]

type Cost = Int
type Edge = (Path,Cost,State)
type Path = [Move]

data Type = A | B | C | D deriving (Eq,Ord,Show)
data Corr = LL | L | X | Y | Z | R | RR deriving (Eq,Ord,Show)
data Burrow = Wrong (Int,[Type]) | Correct Int deriving (Eq,Ord,Show)
data State = State { burrows :: Map Type Burrow
                   , corridor :: Map Corr Type
                   } deriving (Eq,Ord,Show)

initState :: Setup -> State
isComplete :: State -> Bool
lowerBoundCostToComplete :: State -> Cost
legalMoves :: Int -> State -> [Edge]

initState xs = State{burrows,corridor = Map.empty}
  where burrows = Map.fromList [ (k,Wrong (0,ts)) | (k,ts) <- xs ]

isComplete State{burrows,corridor} =
  Map.null corridor && all (\(_,b) -> isCorrect b) (Map.toList burrows)
  where isCorrect = \case Correct{} -> True; Wrong{} -> False

lowerBoundCostToComplete _s@State{burrows,corridor} =
  sum [ cost
      | (w,Wrong (n,ts)) <- Map.toList burrows
      , (i,t) <- zip [n+1..] ts
      , t /= w
      , let cost = mulT t * (i + abs (posT w - posT t))
      ]
  +
  sum [ cost
      | (c,t) <- Map.toList corridor
      , let cost = mulT t * (abs (posC c - posT t))
      ]
  + (if _blocked _s then 1000000 else 0)

_blocked :: State -> Bool
_blocked State{corridor} = do
  let x = Map.lookup X corridor
  let y = Map.lookup Y corridor
  let z = Map.lookup Z corridor
  blM X Y (x,y) || blM Y Z (y,z) || blM X Z (x,z)
    where
      blM :: Corr -> Corr -> (Maybe Type,Maybe Type) -> Bool
      blM a b = \case
        (Just ta,Just tb) -> (posT ta) > (posC b) && (posT tb) < (posC a)
        _ ->
          False

legalMoves d s = [ forcedMove d e | e <- moveOut s ]

forcedMove :: Int -> Edge -> Edge
forcedMove d (p,c,s) =
  case listToMaybe (moveIn d s ++ nullMoves s) of
    Just (p2,c2,s2) -> forcedMove d (p2++p,c2+c,s2)
    Nothing -> (p,c,s)

data Move
  = MoveNull {kind:: Type, num:: Int}
  | MoveOut {fromT :: Type, kind:: Type, dest:: Corr, cost:: Cost}
  | MoveIn {fromC :: Corr, kind :: Type, cost:: Cost}
  deriving (Eq,Ord)

instance Show Move where
  show = \case
    MoveNull{kind,num} -> printf "n: (%s/%d)" (show kind) num
    MoveOut{fromT,kind,dest,cost} ->
      printf "o: %s-->(%s)-->%s [%d]"
      (show fromT) (show kind) (show dest) cost
    MoveIn{fromC,kind,cost} ->
      printf "i: %s-->(%s)-->%s [%d]"
      (show fromC) (show kind) (show kind) cost

nullMoves :: State -> [Edge]
nullMoves state@State{burrows} =
  [ ( [MoveNull {kind = w, num = length ts}]
    , 0, state { burrows = Map.insert w (Correct (length ts)) burrows })
  | (w,Wrong (_,ts)) <- Map.toList burrows
  , all (==w) ts
  ]

moveOut :: State -> [Edge]
moveOut state@State{burrows=burrows0,corridor=corridor0} =
  [ ( [MoveOut {fromT=w,kind=t1,dest=c,cost}]
    , cost, state { burrows , corridor}
    )
  | (w,Wrong (n,t1:trest)) <- Map.toList burrows0
  , c <- allC
  , okToMove w c (Map.keys corridor0)
  , let burrows = Map.insert w (Wrong (n+1,trest)) burrows0
  , let corridor = Map.insert c t1 corridor0
  , let cost = mulT t1 * (n + 1 + costCT w c)
  ]

moveIn :: Int -> State -> [Edge]
moveIn d state@State{burrows=burrows0,corridor=corridor0} =
  [ ( [MoveIn {fromC=c,kind=t,cost}]
    , cost, state { burrows, corridor}
    )
  | (c,t) <- Map.toList corridor0
  , (t',Correct n) <- Map.toList burrows0
  , t==t'
  , let corridor = Map.delete c corridor0
  , okToMove t c (Map.keys corridor)
  , let burrows = Map.insert t (Correct (n+1)) burrows0
  , let cost = mulT t * ((d-n) + costCT t c)
  ]

okToMove :: Type -> Corr -> [Corr] -> Bool
okToMove t c taken =
  all (\p -> p < x || p > y) (map posC taken)
  where
    (x,y) = ordered (posT t, posC c)
    ordered (a,b) = if a < b then (a,b) else (b,a)

posC :: Corr -> Int
posC = \case LL -> 0; L -> 1; X -> 3; Y -> 5; Z -> 7; R -> 9; RR -> 10

posT :: Type -> Int
posT = \case A -> 2; B -> 4; C -> 6; D -> 8

costCT :: Type -> Corr -> Cost
costCT t c = abs (posT t - posC c)

mulT :: Type -> Int
mulT = \case A -> 1; B -> 10; C -> 100; D -> 1000

allC :: [Corr]
allC = [LL,L,X,Y,Z,R,RR]
