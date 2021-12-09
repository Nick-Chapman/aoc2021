module Day9 (main) where

import Data.List (sort)
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
type Pos = (Int,Int)

part1 :: Setup -> Int
part1 hss = sum [ h+1 | (pos,h) <- Map.toList m,  all (h <) (adjacentHeights pos)]
  where
    m = Map.fromList [ ((x,y),h) | (y,hs) <- zip [0..] hss, (x,h) <- zip [0..] hs ]
    adjacentHeights pos = [ h | p <- adjacent pos, h <- maybe [] pure $ Map.lookup p m ]

part2 :: Setup -> Int
part2 hss = product $ take 3 $ reverse $ sort [ length b | (_,b) <- basinsByLowPoint ]
  where
    basinsByLowPoint = collate [ (flow p h, p) | (p,h) <- Map.toList m, h < 9 ]
    m = Map.fromList [ ((x,y),h) | (y,hs) <- zip [0..] hss, (x,h) <- zip [0..] hs ]
    adjacentPH pos = [ (p,h) | p <- adjacent pos, h <- maybe [] pure $ Map.lookup p m ]
    flow p h = case [ ph | ph@(_,h') <- adjacentPH p, h' < h ] of [] -> p; (p,h):_ -> flow p h

adjacent :: Pos -> [Pos]
adjacent (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
