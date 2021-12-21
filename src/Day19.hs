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

  -- play runs parts 1 & 2 together with prints
  play sam
  --play inp -- 100s -- TODO: optimize so this can be enabled!

  let _ = print ("day19, part1(sam)", check 79 $ part1 sam)
  let _ = print ("day19, part2(sam)", check 3621 $ part2 sam)
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
  print "**overlaps computed**"
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
  print ("day19, part1",Set.size (Set.fromList ps))
  --print "pairwise..."
  let ws = [ (abs x + abs y + abs z)
           | k1 <- ks, k2 <- ks
           , let path = lookPath k1 k2
           , let (x,y,z) = applyPath path (0,0,0)
           ]
  print ("day19, part2",maximum ws)
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
  (o,s):path -> \p -> applyPath path (appOrientation p o `add` s)

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
  | o <- allOrientation
  , x <- Set.toList xs
  , y <- Set.toList ys
  , let shift = x `sub` (appOrientation y o)
  , let ys' = Set.map (add shift . flip appOrientation o) ys
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


data Orientation
  = O_X_Y_Z
  | O_Y_Z_X
  | O_Z_X_Y
  | O_Z_Y_x
  | O_X_Z_y
  | O_Y_X_z
  | O_x_y_Z
  | O_y_z_X
  | O_z_x_Y
  | O_z_y_x
  | O_x_z_y
  | O_y_x_z
  | O_Y_x_Z
  | O_Z_y_X
  | O_X_z_Y
  | O_Y_z_x
  | O_Z_x_y
  | O_X_y_z
  | O_y_X_Z
  | O_z_Y_X
  | O_x_Z_Y
  | O_y_Z_x
  | O_z_X_y
  | O_x_Y_z
  deriving Show

allOrientation :: [Orientation]
allOrientation =
  [ O_X_Y_Z
  , O_Y_Z_X
  , O_Z_X_Y
  , O_Z_Y_x
  , O_X_Z_y
  , O_Y_X_z
  , O_x_y_Z
  , O_y_z_X
  , O_z_x_Y
  , O_z_y_x
  , O_x_z_y
  , O_y_x_z
  , O_Y_x_Z
  , O_Z_y_X
  , O_X_z_Y
  , O_Y_z_x
  , O_Z_x_y
  , O_X_y_z
  , O_y_X_Z
  , O_z_Y_X
  , O_x_Z_Y
  , O_y_Z_x
  , O_z_X_y
  , O_x_Y_z
  ]


appOrientation :: Point -> Orientation -> Point
appOrientation (x,y,z) = \case
  O_X_Y_Z -> (x,y,z)
  O_Y_Z_X -> (y,z,x)
  O_Z_X_Y -> (z,x,y)
  O_Z_Y_x -> (z,y,-x)
  O_X_Z_y -> (x,z,-y)
  O_Y_X_z -> (y,x,-z)
  O_x_y_Z -> (-x,-y,z)
  O_y_z_X -> (-y,-z,x)
  O_z_x_Y -> (-z,-x,y)
  O_z_y_x -> (-z,-y,-x)
  O_x_z_y -> (-x,-z,-y)
  O_y_x_z -> (-y,-x,-z)
  O_Y_x_Z -> (y,-x,z)
  O_Z_y_X -> (z,-y,x)
  O_X_z_Y -> (x,-z,y)
  O_Y_z_x -> (y,-z,-x)
  O_Z_x_y -> (z,-x,-y)
  O_X_y_z -> (x,-y,-z)
  O_y_X_Z -> (-y,x,z)
  O_z_Y_X -> (-z,y,x)
  O_x_Z_Y -> (-x,z,y)
  O_y_Z_x -> (-y,z,-x)
  O_z_X_y -> (-z,x,-y)
  O_x_Y_z -> (-x,y,-z)
