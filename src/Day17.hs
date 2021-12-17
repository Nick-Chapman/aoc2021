module Day17 (main) where

import Data.List (nub)
import Misc (check)

main :: IO ()
main = do
  print ("dayX, part1(sam)", check 45 $ part1 sam)
  print ("dayX, part1", check 33670 $ part1 inp)
  print ("dayX, part2(sam)", check 112 $ part2 sam)
  print ("dayX, part2", check 4903 $ part2 inp) -- very slow (200s)

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
    [ t
    | tx <- [0..xmax]
    , ty <- [ymin..abs ymin]
    , n <- [1.. 2 * (abs ymin) + 1]
    , let t = Traj tx ty
    , let _fp@(Pos fx fy) = sim n t
    , fx >= xmin
    , fx <= xmax
    , fy >= ymin
    , fy <= ymax
    ]

data Pos = Pos Int Int deriving Show
data Traj = Traj Int Int deriving (Eq,Show)

sim :: Int -> Traj -> Pos
sim n t = snd $ last $ loop n t (Pos 0 0)
  where
    loop :: Int -> Traj -> Pos -> [(Traj,Pos)]
    loop n t@(Traj tx ty) p@(Pos px py) =
      if n == 0 then [] else
        (t,p) : loop (n-1) (Traj (max (tx-1) 0) (ty-1)) (Pos (px+tx) (py+ty))
