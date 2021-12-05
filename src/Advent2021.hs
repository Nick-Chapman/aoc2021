module Advent2021 where

import System.Environment (getArgs)

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5

mains :: [(Int,IO ())]
mains = zip [1..]
  [ Day1.main
  , Day2.main
  , Day3.main
  , Day4.main
  , Day5.main
  ]

main :: IO ()
main = do
  args <- getArgs
  let selected = if args == [] then  [1..] else map read args
  let picked = [ x | x@(i,_) <- mains, i `elem` selected ]
  sequence_ [ io | (_day,io) <- picked ]
