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
  { n :: Int
  , next :: Int
  , turn :: Player
  , p1_loc :: Int
  , p2_loc :: Int
  , p1_score :: Int
  , p2_score :: Int
  }
  deriving Show

data Player = P1 | P2 deriving Show

part1 :: Param -> Int
part1 p = score $ head $ dropWhile (not . stop p) $ iterate (step p) (start p)

start :: Param -> State
step :: Param -> State -> State
stop :: Param -> State -> Bool
score :: State -> Int

start Param{p1_start,p2_start} = State
  { n = 0
  , next = 1
  , turn = P1
  , p1_loc = p1_start
  , p2_loc = p2_start
  , p1_score = 0
  , p2_score = 0
  }

step Param{d_size,b_size} State{n,next,turn,p1_loc,p2_loc,p1_score,p2_score} = do
  let d1 = next
  let d2 = next `mod` d_size + 1
  let d3 = (next + 1) `mod` d_size + 1
  let ddd = d1 + d2 + d3
  case turn of
    P1 -> do
      State
        { n = n + 3
        , next = (next + 2) `mod` d_size + 1
        , turn = P2
        , p1_loc = ((p1_loc + ddd - 1) `mod` b_size) + 1
        , p2_loc = p2_loc
        , p1_score = p1_score + ((p1_loc + ddd - 1) `mod` b_size) + 1
        , p2_score = p2_score
        }
    P2 ->
      State
        { n = n + 3
        , next = (next + 2) `mod` d_size + 1
        , turn = P1
        , p1_loc = p1_loc
        , p2_loc = ((p2_loc + ddd - 1) `mod` b_size) + 1
        , p1_score = p1_score
        , p2_score = p2_score + ((p2_loc + ddd - 1) `mod` b_size) + 1
        }

stop Param{win} State {turn,p1_score,p2_score} =
  case turn of
    P1 -> p2_score >= win
    P2 -> p1_score >= win

score State {n,turn,p1_score,p2_score} =
  case turn of
    P1 -> p1_score * n
    P2 -> p2_score * n


