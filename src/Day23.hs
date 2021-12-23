module Day23 (main) where

import Data.Maybe --(fromJust,maybeToList)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Misc (check)

main :: IO ()
main = do
  s1 <- part1 initSam1
  print ("day23, part1(sam)", check 12521 $ s1)

  let _ = do
        i1 <- part1 initInp1
        print ("day23, part1", check 0 $ i1)

        s2 <- part2 initSam2
        print ("day23, part2(sam)", check 0 $ s2)

        i2 <- part2 initInp2
        print ("day23, part2", check 0 $ i2)

  pure ()

part1 :: State -> IO Cost
part1 = search part1pieces

part2 :: State -> IO Cost
part2 = search part2pieces


-- A* search

search :: [Piece] -> State -> IO Cost
search pieces state0 = do

  let
    reachGoal :: SS -> Maybe Cost
    reachGoal SS{frontier} = do
      let (cost,states) :: (Cost,Set (Cost,State)) = Map.findMin frontier
      if any (\(_,s) -> isComplete s) states then Just cost else Nothing

  let ss0 = SS
        { visited = Set.empty -- fromList [state0] -- TODO: correct
        , frontier = Map.fromList [ (lowerBoundCostToComplete state0
                                    , Set.fromList [(0,state0)]) ]
        }
  let
    loop :: Int -> SS -> IO Cost
    loop i ss = do
      putStrLn (show i ++ "..." ++ show ss)
      --if i == 2 then error "stop!" else
      case reachGoal ss of
          Just cost -> pure cost
          Nothing -> loop (i+1) (step pieces ss)

  loop 0 ss0

data SS = SS --search state
  { visited :: Set State
  , frontier :: Map Cost (Set (Cost,State))
  }

{-instance Show SS where
  show SS{frontier} =
    concat [ "(" ++ show c ++ ")\n" ++
             concat (map (\(_,s) -> show s) (Set.toList ss))
             ++ "\n" | (c,ss) <- Map.toList frontier ]
-}

instance Show SS where
  show SS{visited,frontier} =
    show (Set.size visited, (fst (Map.findMin frontier), fst (Map.findMax frontier)))



step :: [Piece] -> SS -> SS
step pieces SS{visited=visited0,frontier=frontier0} = SS{visited=visited1,frontier=frontier1}
  where
    xx :: ((Cost, Set (Cost, State)), Map Cost (Set (Cost, State)))
    xx = fromJust $ Map.minViewWithKey frontier0

    --minK :: Cost
    statesToExpand :: Set (Cost, State)
    frontier0' :: Map Cost (Set (Cost, State))
    ((_,statesToExpand),frontier0') = xx

    transitions :: [(Cost,State,Move)]
    transitions = [ (c,q,m)
                  | (c,p) <- Set.toList statesToExpand
                  , m <- legalMoves pieces p
                  , let q = applyMove m p
                  , q `Set.notMember` visited0
                  ]

    newFrontier :: Map Cost (Set (Cost,State))
    newFrontier =
      Map.fromListWith Set.union
      [ (lowerBoundCostToComplete q + c' , Set.fromList [(c',q)])
      | (c,q,m) <- transitions
      , let c' = c + costOfMove m
      ]

    newStates :: [State]
    newStates = map (\(_,s,_) -> s) transitions

    visited1 = Set.union visited0 (Set.fromList newStates)
    frontier1 = Map.unionWith Set.union newFrontier frontier0'



data Type = A | B | C | D deriving (Eq,Ord,Show)
data Pos
  = Ht Type | Hb Type -- top/bottom home slots -- MAN, this wll need to be 1/2/3/4 for real!
  | LL | L | X | Y | Z | R | RR
  deriving (Eq,Ord,Show)

data Piece = Piece Type Int deriving (Eq,Ord,Show)-- 1/2 (part1) 1/2/3/4 (part2)

data State = State (Map Piece Pos) deriving (Eq,Ord)
data Move = Move Piece Pos Pos
type Cost = Int


data Cell = W | G | P Pos

instance Show State where
  show (State m) = concat [ seeLine line ++ "\n" | line <- world ]
    where
      world :: [[Cell]]
      world =
        [ [P LL,P L,G,        P X,G,       P Y,G,        P Z,G,       P R,P RR]
        , [W,   W,  P (Ht A), W  ,P (Ht B),W,  P (Ht C), W,  P (Ht D),W,  W]
        , [W,   W,  P (Hb A), W  ,P (Hb B),W,  P (Hb C), W,  P (Hb D),W,  W]
        ]

      seeLine :: [Cell] -> String
      seeLine line = [ seeCell cell | cell <- line ]

      seeCell :: Cell -> Char
      seeCell = \case
        W -> ' '
        G -> ' '
        P pos -> case located pos of Just t -> seeType t; Nothing -> '.'

      seeType :: Type -> Char
      seeType = \case A -> 'A'; B -> 'B'; C -> 'C'; D -> 'D'

      located :: Pos -> Maybe Type -- reverse mapping (Nothing = empty)
      located pos' =
        mostOne [ typeOf piece | (piece,pos) <- Map.toList m, pos == pos' ]

      mostOne :: [a] -> Maybe a
      mostOne = \case [a] -> Just a; [] -> Nothing; _ -> error "the>=2"


part1pieces,part2pieces :: [Piece]
initSam1,initInp1,initSam2,initInp2 :: State
isComplete :: State -> Bool
costOfMove :: Move -> Cost
lowerBoundCostToComplete :: State -> Cost
legalMoves :: [Piece] -> State -> [Move]
applyMove :: Move -> State -> State

part1pieces = [ Piece t n | t <- [A,B,C,D], n <- [1,2] ]
part2pieces = undefined

initSam1 = State $ Map.fromList
  [ (Piece A 1, Hb A)
  , (Piece A 2, Hb D)
  , (Piece B 1, Ht A)
  , (Piece B 2, Ht C)
  , (Piece C 1, Ht B)
  , (Piece C 2, Hb C)
  , (Piece D 1, Hb B)
  , (Piece D 2, Ht D)
  ]
initSam2 = undefined
initInp1 = undefined
initInp2 = undefined


shortMoveEndpoint :: Pos -> Bool
shortMoveEndpoint = \case
  LL -> True
  RR -> True
  Hb{} -> True
  _ -> False

typeMultiplier :: Type -> Int
typeMultiplier = \case
  A -> 1
  B -> 10
  C -> 100
  D -> 1000

costOfMove (Move piece from to) =
  typeMultiplier (typeOf piece) *
  (if (shortMoveEndpoint from || shortMoveEndpoint to) then 1 else 2)

lowerBoundCostToComplete _ = 0 -- TODO

applyMove (Move piece _ dest) (State m) = State (Map.insert piece dest m)

isComplete (State m) = all atHome (Map.toList m)

legalMoves pieces (State m) =
  [ Move piece pos1 pos2
  | piece <- pieces
  , let pos1 = locate piece
--  , not (atHome (piece, pos1))
  , pos2 <- adjacent pos1
  , not (filled pos2)
  , not (corridor pos1 && wrongHome piece pos2)
--  , not (corridor pos1 && corridor pos2)
--  , not (correctHome piece pos2 && notCleared (typeOf piece))
  ]
  where

--    notCleared :: Type -> Bool
--    notCleared _ = False -- TODO: WRONG!

    filled :: Pos -> Bool
    filled pos = any (== pos) (map snd (Map.toList m))

    locate :: Piece -> Pos
    locate piece = maybe (error "locate") id $ Map.lookup piece m


corridor :: Pos -> Bool
corridor = \case
  Ht{} -> False
  Hb{} -> False
  _ -> True

wrongHome :: Piece -> Pos -> Bool
wrongHome piece = \case
  Ht t -> typeOf piece /= t
  _ -> False

{-correctHome :: Piece -> Pos -> Bool
correctHome piece = \case
  Ht t -> typeOf piece == t
  _ -> False-}

typeOf :: Piece -> Type
typeOf (Piece t _) = t


atHome :: (Piece,Pos) -> Bool
atHome (Piece t _, pos) =
  case pos of
    Ht t' -> t==t'
    Hb t' -> t==t'
    _ -> False

adjacent :: Pos -> [Pos]
adjacent = \case
  LL ->[L]
  L -> [LL, Ht A,       X]
  X -> [L,  Ht A, Ht B, Y]
  Y -> [X,  Ht B, Ht C, Z]
  Z -> [Y,  Ht C, Ht D, R]
  R -> [Z,        Ht D, RR]
  RR ->[R]
  Ht A -> [Hb A, L,X]
  Ht B -> [Hb B, X,Y]
  Ht C -> [Hb C, Y,Z]
  Ht D -> [Hb D, Z,R]
  Hb t -> [Ht t]
