module Day11 (main) where

import Data.Map (Map)
import Data.Set (Set)
import Misc (check)
import Par4 (Par,parse,separated,nl,some,digit)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- load "input/day11.input.sam"
  inp <- load "input/day11.input"
  print ("day11, part1(sam)", check 1656 $ part1 sam)
  print ("day11, part1", check 1649 $ part1 inp)
  print ("day11, part2(sam)", check 195 $ part2 sam)
  print ("day11, part2", check 256 $ part2 inp)

type Setup = [[Int]]

load :: FilePath -> IO Setup
load path = parse gram <$> readFile path

gram :: Par Setup
gram = separated nl (some digit)

part1 :: Setup -> Int
part1 vss = do
  let s = initState vss
  let (100,res,_) = head $ reverse $ stepN 100 s
  res

part2 :: Setup -> Int
part2 vss = do
  let s = initState vss
  let xs = [ x | (_,x,_) <- stepN 2000 s ]
  head [ n | (n,(a,b)) <- zip [1..] (zip (tail xs) xs), let d = a-b, d == 100 ]

newtype State = State (Map K V)
type K = Pos
type V = Energy
type Pos = (Int,Int)
type Energy = Int
type CountFlashes = Int

instance Show State where
  show s@(State m) = res where
    (mx,my) = maxPos s
    res = unlines (map mkLine [0..my])
    mkLine y = [ c | x <- [0..mx], let [c] = pos x y ]
    pos x y = maybe "?" char $ Map.lookup (x,y) m
    char v = if v > 9 then "*" else show v

initState :: [[Int]] -> State
initState vss =
  State $ Map.fromList [ ((x,y),v) | (y,vs) <- zip [0..] vss, (x,v) <- zip [0..] vs ]

maxPos :: State -> Pos
maxPos (State m) = (mx,my)
  where
    mx = maximum [ x | (x,_) <- Map.keys m ]
    my = maximum [ y | (_,y) <- Map.keys m ]

stepN :: Int -> State -> [(Int,CountFlashes,State)] -- stop after N, accum couns
stepN n s = loop 0 s 0
  where
    loop :: Int -> State -> Int -> [(Int,CountFlashes,State)]
    loop accCount s i =
      if i > n then [] else  do
        let (s',c) = step s
        (i,accCount,s) : loop (accCount+c) s' (i+1)


step :: State -> (State,CountFlashes)
step s0 = loop Set.empty (incAll s0)
  where
    (mx,my) = maxPos s0

    loop :: Set Pos -> State -> (State,CountFlashes)
    loop acc s = do
      let ps = [ p | p <- flash s, p `notElem` acc ]
      case ps of
        [] -> do
          let sFinal = resetZeros (Set.toList acc) s
          let countFlashes = Set.size acc
          (sFinal, countFlashes)
        _ -> do
          let adjPs = [ a | p <- ps, a <- adjacent p ]
          let s' = incPositions adjPs s
          let acc' = acc `Set.union` Set.fromList ps
          loop acc' s'

    adjacent :: Pos -> [Pos] -- given max, including diaganols
    adjacent (x,y) =
      [ (x',y') | x' <- [x-1,x,x+1]
                , x' >= 0 && x' <= mx
                , y' <- [y-1,y,y+1]
                , y' >= 0 && y' <= my
                , (x/=x') || (y/=y')
                ]

incAll :: State -> State
incAll s = incPositions (allPos s) s

incPositions :: [Pos] -> State -> State
incPositions ps s = foldr incPosition s ps

incPosition :: Pos -> State -> State
incPosition p (State m) = State $ Map.adjust (+1) p m

flash :: State -> [Pos]
flash (State m) = [ p | (p,v) <- Map.toList m, v >= 10 ]

allPos :: State -> [Pos]
allPos (State m) = Map.keys m

resetZeros :: [Pos] -> State -> State
resetZeros ps s = foldr resetZero s ps

resetZero :: Pos -> State -> State
resetZero p (State m) = State $ Map.insert p 0 m
