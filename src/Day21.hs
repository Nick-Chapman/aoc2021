module Day21 (main) where

import Misc (check)

main :: IO ()
main = do
  let sam = Param { win = 1000, b_size = 10, d_size = 100, p1_start = 4, p2_start = 8 }
  let inp = Param { win = 1000, b_size = 10, d_size = 100, p1_start = 8, p2_start = 3 }
  print ("day21, part1(sam)", check 739785 $ part1 sam)
  print ("day21, part1", check 412344 $ part1 inp)

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
  deriving Show

data Player = P1 | P2 deriving Show

part1 :: Param -> Int
part1 p = do
  let
    play :: [Int] -> State -> [State]
    play = \case
      a:b:c:dice -> \s -> s : play dice (step p (a+b+c) s)
      _ -> undefined
  let (ys,zs) = span (not . stop p) $ play (cycle [1..100]) (start p)
  score (head zs) * 3 * length ys

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


