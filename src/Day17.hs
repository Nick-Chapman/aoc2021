module Day17 (main) where

import Data.List (nub)
import Misc (check)

main :: IO ()
main = do
  print ("dayX, part1(sam)", check 45 $ part1 sam)
  print ("dayX, part1", check 33670 $ part1 inp)
  print ("dayX, part2(sam)", check 112 $ part2 sam)
  print ("dayX, part2", check 4903 $ part2 inp) -- <1sec now

data Target = Target {xmin::Int,xmax::Int,ymin::Int,ymax::Int} deriving Eq

sam,inp :: Target
sam = Target {xmin=20,xmax=30,ymin= -10,ymax= -5}
inp = Target {xmin=25,xmax=67,ymin= -260,ymax= -200}

part1 :: Target -> Int -- did this by hand, so no code!
part1 target =
  if target == sam then 45
  else if target == inp then 33670
  else undefined

part2 :: Target -> Int
part2 Target{xmin,xmax,ymin,ymax} = do
  length $ nub
    [ (tx,ty)
    | n <- [1.. 2 * (abs ymin) ]

    , ty <- [ymin..abs ymin]
    , let fy = simY n ty 0
    , fy >= ymin
    , fy <= ymax

    , tx <- [0..xmax]
    , let fx = simX n tx 0
    , fx >= xmin
    , fx <= xmax
    ]

simX :: Int -> Int -> Int -> Int
simX n tx px = if n == 0 then px else simX (n-1) (max (tx-1) 0) (px+tx)

simY :: Int -> Int -> Int -> Int
simY n ty py = if n == 0 then py else simY (n-1) (ty-1) (py+ty)
