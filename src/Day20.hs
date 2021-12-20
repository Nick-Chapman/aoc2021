module Day20 (main) where

import Data.Map (Map)
import Data.Set (Set)
import Misc (check)
import Par4 (Par,parse,separated,nl,lit,some,alts)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- load "input/day20.input.sam"
  inp <- load "input/day20.input"
  let _ = play sam
  print ("day20, part1(sam)", check 35 $ part1 sam)
  print ("day20, part1", check 5597 $ part1 inp)
  print ("day20, part2(sam)", check 3351 $ part2 sam)
  print ("day20, part2", check 18723 $ part2 inp)

load :: FilePath -> IO Setup
load path = parse gram <$> readFile path

gram :: Par Setup
gram = do
  i <- iea; nl; nl; g <- separated nl (some pix); pure (i,g)
  where
    iea = makeIEA <$> sequence (take 512 (repeat pix))
    pix = alts [l,d]
    l = do lit '#'; pure Light
    d = do lit '.'; pure Dark

type Setup = (IEA, Grid)
type IEA = Set Nine
type Nine = Int -- 9 bit Int
type Grid = [Line]
type Line = [Pix]
data Pix = Light | Dark deriving (Eq,Show)

makeIEA :: [Pix] -> IEA
makeIEA pixs =
  Set.fromList [ n | (n,p) <- zip [0..] pixs, p == Light ]

play :: Setup -> IO ()
play (iea,grid) =
  mapM_ print $ take 50 (iterate (step iea) (initGen grid))

part1 :: Setup -> Int
part1 setup = countLight (run 2 setup)

part2 :: Setup -> Int
part2 setup = countLight (run 50 setup)

run :: Int -> Setup -> Gen
run n (iea,grid) = do
  head (drop n (iterate (step iea) (initGen grid)))

data Gen = Gen { m :: Map Pos Pix, bg :: Pix }

instance Show Gen where
  show Gen{m,bg} = res where
    nx = minimum [ x | (x,_) <- Map.keys m ] - 1
    ny = minimum [ y | (_,y) <- Map.keys m ] - 1
    mx = maximum [ x | (x,_) <- Map.keys m ] + 1
    my = maximum [ y | (_,y) <- Map.keys m ] + 1
    res = unlines (map mkLine [ny..my])
    mkLine y = [ c | x <- [nx..mx], let c = pos x y ]
    pos x y = char (maybe bg id $ Map.lookup (x,y) m)
    char = \case Light -> '#'; Dark -> '.'

type Pos = (Int,Int)

initGen :: Grid -> Gen
initGen pss =
  Gen { bg = Dark
      , m = Map.fromList
            [ ((x,y),p)
            | (y,ps) <- zip [0..] pss, (x,p) <- zip [0..] ps
            ] }

step :: IEA -> Gen -> Gen
step iea Gen{bg=bg0,m=m0} = do
  let
    look :: Pos -> Pix
    look pos = maybe bg0 id $ Map.lookup pos m0
  let
    look9 :: Pos -> Nine
    look9 pos = do
      sum [ 2 ^ i
          | (i,pos) <- zip [0::Int ..] (nineBy pos)
          , look pos == Light
          ]
  let
    do9 :: Nine -> Pix
    do9 i = if i `Set.member` iea then Light else Dark
  let
    pointsToConsider = do
      let nub = Set.toList . Set.fromList
      nub [ b | a <- Map.keys m0, b <- nineBy a ]
  Gen
    { bg = do9 (case bg0 of Light -> 511; Dark -> 0),
      m = Map.fromList
          [ (p, do9 (look9 p))
          | p <- pointsToConsider
          ]
    }

nineBy :: Pos -> [Pos]
nineBy (x,y) = reverse
  [(x-1,y-1), (x,y-1), (x+1,y-1)
  ,(x-1,y  ), (x,y  ), (x+1,y  )
  ,(x-1,y+1), (x,y+1), (x+1,y+1)
  ]

countLight :: Gen -> Int
countLight Gen{m,bg} =
  if bg == Light then error "countLight" else
    length [ () | (_,pix) <- Map.toList m, pix == Light ]
