module Day9 (main) where

import Data.List (sort)
import Data.Maybe (listToMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Misc (check,collate)
import Par4 (Par,parse,separated,nl,digit,some)

main :: IO ()
main = do
  sam <- load "input/day9.input.sam"
  inp <- load "input/day9.input"
  print ("day9, part1(sam)", check 15 $ part1 sam)
  print ("day9, part1", check 491 $ part1 inp)
  print ("day9, part2(sam)", check 1134 $ part2 sam)
  print ("day9, part2", check 1075536 $ part2 inp)

load :: FilePath -> IO Setup
load path = parse gram <$> readFile path

gram :: Par [[Height]]
gram = separated nl (some digit)

type Setup = [[Height]]
type Height = Int

part1 :: Setup -> Int
part1 hss = res
  where
    coordinated =
      concat [ [ ((x,y),h) | (x,h) <- zip [0..] hs ] | (y,hs) <- zip [0..] hss ]

    m :: Map Pos Height = Map.fromList coordinated

    nextDoorHeights :: Pos -> [Height]
    nextDoorHeights pos =
      [ h | p <- nextDoor pos, h <- maybe [] single $ Map.lookup p m ]
      where single x = [x]

    lowPointRisks =
      [ h+1
      | (pos,h) <- coordinated
      ,  all (h <) (nextDoorHeights pos)
      ]

    res = sum lowPointRisks

type Pos = (Int,Int)

nextDoor :: Pos -> [Pos]
nextDoor (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

part2 :: Setup -> Int
part2 hss =
  product $ take 3 $ reverse $ sort [ length b | (_,b) <- basinsByLowPoint ]

  where
    coordinated =
      concat [ [ ((x,y),h) | (x,h) <- zip [0..] hs ] | (y,hs) <- zip [0..] hss ]

    m :: Map Pos Height = Map.fromList coordinated

    nextDoorHeights :: Pos -> [(Pos,Height)]
    nextDoorHeights pos =
      [ (p,h) | p <- nextDoor pos, h <- maybe [] pure $ Map.lookup p m ]

    flow1 :: Pos -> Maybe Pos -- single step flow, or else we are low point
    flow1 p = do
      let myH = maybe (error "myH") id $ Map.lookup p m
      listToMaybe [ p' | (p',h') <- nextDoorHeights p, h' < myH ]

    flow :: Pos -> Pos -- final flow point. maybe self
    flow p =
      case flow1 p of
        Nothing -> p
        Just p' -> flow p'

    basinsByLowPoint = collate [ (flow p, p) | (p,h) <- coordinated, h < 9 ]
