module Day19 (main) where

import Data.List (nubBy)
import Data.Set (Set)
import Misc (check)
import ParE (Par,parse,separated,nl,int,lit,key,some,sat,alts)
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- load "input/day19.input.sam"
  inp <- load "input/day19.input"
  play sam
  let _ = play inp
  print ("day19, part1(sam)", check 79 $ part1 sam)
  print ("day19, part2(sam)", check 3621 $ part2 sam)
  let _ = print ("day19, part1", check 372 $ part1 inp) -- slow!
  let _ = print ("day19, part2", check 12241 $ part2 inp) -- slow!
  pure ()

load :: FilePath -> IO Setup
load path = parse gram <$> readFile path

gram :: Par Setup
gram = separated blank scanner
  where
    scanner = do header; xs <- separated nl point; pure $ Scanner (Set.fromList xs)
    header = do key "---"; _ <- some notNL; nl; pure ()
    notNL = do _ <- sat (/= '\n'); pure ()
    blank = do nl; nl
    point = do x <- num; lit ','; y <- num; lit ','; z <- num; pure (x,y,z)
    num = alts [ int , do lit '-'; negate <$> int]

type Setup = [Scanner]
data Scanner = Scanner (Set Point) deriving Show
type Point = (Int,Int,Int)

type K = Char
type E = (Orientation,Point)

labels :: [K]
labels = ['a'..]

part1 :: [Scanner] -> Int
part1 scanners = do
  let zs = zip labels scanners
  let ks = map fst zs
  let overlaps = findOverlaps scanners
  let overlapsT = transitiveClosure ks overlaps
  let
    pointsOfKey c = head [ Set.toList ps | (c',Scanner ps) <- zs, c==c' ]
  let
    lookPath :: K -> K -> [E]
    lookPath k1 k2 = head [ path | ((k1',k2'),path) <- overlapsT, k1==k1', k2==k2' ]
  let k0 = head ks
  let ps = [ p
           | k <- ks
           , let path = lookPath k k0
           , let ps' = map (applyPath path) (pointsOfKey k)
           , p <- ps' ]
  Set.size (Set.fromList ps)

part2 :: Setup -> Int
part2 scanners = do
  let zs = zip labels scanners
  let ks = map fst zs
  let overlaps = findOverlaps scanners
  let overlapsT = transitiveClosure ks overlaps
  let
    lookPath :: K -> K -> [E]
    lookPath k1 k2 = head [ path | ((k1',k2'),path) <- overlapsT, k1==k1', k2==k2' ]
  let ws = [ (abs x + abs y + abs z)
           | k1 <- ks, k2 <- ks
           , let path = lookPath k1 k2
           , let (x,y,z) = applyPath path (0,0,0)
           ]
  maximum ws

play :: [Scanner] -> IO ()
play scanners = do
  let zs = zip labels scanners
  let ks = map fst zs
  let overlaps = findOverlaps scanners
  mapM_ print overlaps
  let overlapsT = transitiveClosure ks overlaps
  let
    pointsOfKey c = head [ Set.toList ps | (c',Scanner ps) <- zs, c==c' ]
  let
    lookPath :: K -> K -> [E]
    lookPath k1 k2 = head [ path | ((k1',k2'),path) <- overlapsT, k1==k1', k2==k2' ]
  let k0 = head ks
  --print "paths..."
  --mapM_ print [ (k,path) | k <- ks, let path = lookPath k0 k ]
  let ps = [ p
           | k <- ks
           , let path = lookPath k k0
           , let ps' = map (applyPath path) (pointsOfKey k)
           , p <- ps'
           ]
  print (Set.size (Set.fromList ps))
  --print "pairwise..."
  let ws = [ (abs x + abs y + abs z)
           | k1 <- ks, k2 <- ks
           , let path = lookPath k1 k2
           , let (x,y,z) = applyPath path (0,0,0)
           ]
  print (maximum ws)
  pure ()

findOverlaps :: [Scanner] -> [((K,K),E)]
findOverlaps scanners =
  [ ((c2,c1),(o,shift))
  | (c1,s1) <- zip labels scanners
  , (c2,s2) <- zip labels scanners
  , c2 /= c1
  , (o,shift) <- maybeOverlaps s1 s2
  ]

applyPath :: [E] -> Point -> Point
applyPath = \case
  [] -> \p -> p
  (o,s):path -> \p -> applyPath path (appOrientation o p `add` s)

transitiveClosure :: [K] -> [((K,K),E)] -> [((K,K),[E])]
transitiveClosure ks edges =
  loop ([ (p,[e]) | (p,e) <- edges ] ++ [ ((k,k),[]) | k <- ks ])
  where
    loop :: [((K,K),[E])] -> [((K,K),[E])]
    loop es = do
      let es2 = doubleEdges es
      case es2 of [] -> es; _ -> loop (es++es2)

doubleEdges :: [((K,K),[E])] -> [((K,K),[E])]
doubleEdges xs =
  nubBy (\(a,_) (b,_) -> a==b)
  [ ((p,r), e1++e2)
  | ((p,q),e1) <- xs, ((q',r),e2) <- xs, q==q', (p,r) `notElem` (map fst xs)
  ]

maybeOverlaps :: Scanner -> Scanner -> [E] --- Maybe
maybeOverlaps (Scanner xs) (Scanner ys) =
  take 1 [ (o,shift)
  | o <- allO
  , x <- Set.toList xs
  , y <- Set.toList ys
  , let shift = x `sub` (appOrientation o y)
  , let ys' = Set.map (add shift . appOrientation o) ys
  , hasTwelveInCommon xs ys'
  ]

sub :: Point -> Point -> Point
sub (a,b,c) (x,y,z) = (a-x,b-y,c-z)

add :: Point -> Point -> Point
add (p,q,r) (x,y,z) = (p+x,q+y,r+z)

hasTwelveInCommon :: Set Point -> Set Point -> Bool
hasTwelveInCommon xs ys = do
  let nX = Set.size xs
  let nY = Set.size ys
  let nXY = Set.size (xs `Set.union` ys)
  nX + nY - nXY >= 12

appOrientation :: Orientation -> Point -> Point
appOrientation (Orientation f) p = f p

data Orientation = Orientation (Point -> Point)
instance Show Orientation where show _ = "<O>"

allO :: [Orientation]
allO = [ Orientation (o1 . o2 . o3 . o4)
       | o1 <- [id,quarterXY]
       , o2 <- [id,halfXY]
       , o3 <- [id,quarterXZ]
       , o4 <- [id,clock,anti]
       ]
  where
    quarterXY   = \(a,b,c) -> (b,-a,c)
    halfXY      = \(a,b,c) -> (-a,-b,c)
    quarterXZ   = \(a,b,c) -> (c,b,-a)
    clock       = \(a,b,c) -> (b,c,a)
    anti        = \(a,b,c) -> (c,a,b)
