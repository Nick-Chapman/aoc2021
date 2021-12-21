module Day21 (main) where

import Data.Map (Map)
import Misc (check)
import qualified Data.Map as Map

main :: IO ()
main = do
  let sam = (4,8)
  let inp = (8,3)
  print ("day21, part1(sam)", check 739785 $ part1 sam)
  print ("day21, part1", check 412344 $ part1 inp)
  print ("day21, part2(sam)", check 444356092776315 $ part2 sam)
  print ("day21, part2", check 214924284932572 $ part2 inp)

data Param = Param
  { win :: Int
  , b_size :: Int
  , d_size :: Int
  , p1_start :: Int
  , p2_start :: Int
  }

data State = State
  { turn :: Player
  , p1_loc :: Int
  , p2_loc :: Int
  , p1_score :: Int
  , p2_score :: Int
  }
  deriving (Eq,Ord,Show)

data Player = P1 | P2 deriving (Eq,Ord,Show)

part1 :: (Int,Int) -> Int
part1 (p1_start,p2_start) = do
  let p = Param { win = 1000, b_size = 10, d_size = 100, p1_start, p2_start }
  let
    play :: [Int] -> State -> [State]
    play = \case
      a:b:c:dice -> \s -> s : play dice (step p (a+b+c) s)
      _ -> undefined
  let (ys,zs) = span (not . stop p) $ play (cycle [1..100]) (start p)
  score (head zs) * 3 * length ys

part2 :: (Int,Int) -> Int
part2 (p1_start,p2_start) = do
  let win = 21
  let param = Param { win, b_size = 10, d_size = 100, p1_start, p2_start }
  let throws = [ a+b+c | a <- [1,2,3], b <- [1,2,3], c <- [1,2,3] ]
  let
    winner :: State -> Player
    winner State {turn} = case turn of P1 -> P2; P2 -> P1
  let
    nd_step :: State -> [State]
    nd_step s = [ step param abc s | abc <- throws ]
  let
    combine :: [Res] -> Res
    combine xs = (sum ys, sum zs) where (ys,zs) = unzip xs
  let
    rolls :: Memo -> [Res] -> [State] -> (Memo, [Res])
    rolls m rs = \case
      x:xs -> let (m2,r) = rolloutM m x in rolls m2 (r:rs) xs
      [] -> (m,rs)
    rolloutM :: Memo -> State -> (Memo, Res)
    rolloutM m s = do
      case Map.lookup s m of
        Just res -> (m,res)
        Nothing -> do
          if stop param s then
            (m, case winner s of P1 -> (1,0); P2 -> (0,1))
            else do
            let (m2,rs) = rolls m [] (nd_step s)
            let r = combine rs
            let m3 = Map.insert s r m2
            (m3,r)
  let memo0 = Map.empty
  let (_,(p1,p2)) = rolloutM memo0 (start param)
  if p1 > p2 then p1 else p2

type Memo = Map State Res
type Res = (Int,Int)

start :: Param -> State
step :: Param -> Int -> State -> State
stop :: Param -> State -> Bool
score :: State -> Int

start Param{p1_start,p2_start} = State
  { turn = P1
  , p1_loc = p1_start
  , p2_loc = p2_start
  , p1_score = 0
  , p2_score = 0
  }

step Param{b_size} ddd State{turn,p1_loc,p2_loc,p1_score,p2_score} = do
  case turn of
    P1 -> do
      State
        { turn = P2
        , p1_loc = ((p1_loc + ddd - 1) `mod` b_size) + 1
        , p2_loc = p2_loc
        , p1_score = p1_score + ((p1_loc + ddd - 1) `mod` b_size) + 1
        , p2_score = p2_score
        }
    P2 ->
      State
        { turn = P1
        , p1_loc = p1_loc
        , p2_loc = ((p2_loc + ddd - 1) `mod` b_size) + 1
        , p1_score = p1_score
        , p2_score = p2_score + ((p2_loc + ddd - 1) `mod` b_size) + 1
        }

stop Param{win} State {turn,p1_score,p2_score} =
  case turn of
    P1 -> p2_score >= win
    P2 -> p1_score >= win

score State {turn,p1_score,p2_score} =
  case turn of
    P1 -> p1_score
    P2 -> p2_score
