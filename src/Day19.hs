module Day19 (main) where

import Data.List (nub,nubBy,sort)
import Data.Maybe (listToMaybe)
import Misc (check)
import ParE (Par,parse,separated,nl,int,lit,key,some,sat,alts)

main :: IO ()
main = do

  sam <- load "input/day19.input.sam"
  (s1,s2) <- part1and2 sam
  print ("day19, part1(sam)", check 79 $ s1)
  print ("day19, part2(sam)", check 3621 $ s2)

  inp <- load "input/day19.input"
  (r1,r2) <- part1and2 inp
  print ("day19, part1", check 372 $ r1)
  print ("day19, part2", check 12241 $ r2)

  pure ()

load :: FilePath -> IO Setup
load path = parse gram <$> readFile path

gram :: Par Setup
gram = separated blank scanner
  where
    scanner = do header; xs <- separated nl point; pure $ Scanner xs
    header = do key "---"; _ <- some notNL; nl; pure ()
    notNL = do _ <- sat (/= '\n'); pure ()
    blank = do nl; nl
    point = do x <- num; lit ','; y <- num; lit ','; z <- num; pure (x,y,z)
    num = alts [ int , do lit '-'; negate <$> int]

type Setup = [Scanner]
data Scanner = Scanner [Point] deriving Show
type Point = (Int,Int,Int)

type K = Char
type E = (Orientation,Point)

labels :: [K]
labels = ['a'..]

part1and2 :: [Scanner] -> IO (Int,Int)
part1and2 scanners = do
  let zs = zip labels scanners
  let ks = map fst zs
  overlapsT <- findTransitiveOverlaps ks scanners
  print "**transitive overlaps computed**"
  let
    pointsOfKey c = head [ ps | (c',Scanner ps) <- zs, c==c' ]
  let
    lookPath :: K -> K -> [E]
    lookPath k1 k2 = head [ path | ((k1',k2'),path) <- overlapsT, k1==k1', k2==k2' ]
  let k0 = head ks
  let ps = nub [ p
           | k <- ks
           , let path = lookPath k k0
           , let ps' = map (applyPath path) (pointsOfKey k)
           , p <- ps'
           ]
  let part1 = length ps
  let ws = [ (abs x + abs y + abs z)
           | k1 <- ks, k2 <- ks
           , let path = lookPath k1 k2
           , let (x,y,z) = applyPath path (0,0,0)
           ]
  let part2 = maximum ws
  pure (part1,part2)

findTransitiveOverlaps :: [K] -> [Scanner] -> IO [((K,K),[E])]
findTransitiveOverlaps ks scanners = do
  let overlaps = findOverlaps scanners
  mapM_ print overlaps
  print "**overlaps computed**"
  let overlapsT = transitiveClosure ks overlaps
  pure overlapsT

findOverlaps :: [Scanner] -> [((K,K),E)]
findOverlaps scanners =
  [ (if b then (c2,c1) else (c1,c2), res)
  | (c1,s1) <- zip labels scanners
  , (c2,s2) <- zip labels scanners
  , c1 < c2
  , (b,res) <- maybeOverlapsEachWay s1 s2
  ]

maybeOverlapsEachWay :: Scanner -> Scanner -> [(Bool,E)]
maybeOverlapsEachWay s1 s2 = do
  case maybeOverlaps s1 s2 of
    Nothing -> []
    Just r12 -> [ (True, r12), (False, invertE r12) ]

invertE :: E -> E
invertE (o,p) = do
  let o' = invertO o
  let p' = invertP p
  (o', appOrientation p' o')

maybeOverlaps :: Scanner -> Scanner -> Maybe E
maybeOverlaps (Scanner xs) (Scanner ys) = do
  let xsSorted = sort xs
  listToMaybe
    [ (o,shift)
    | o <- allOrientation
    , y <- ys
    , let yO = appOrientation y o
    , let ysO = sort (map (flip appOrientation o) ys)
    , x <- xs
    , let shift = x `sub` yO
    , let ysOS = map (add shift) ysO
    , matchN 12 xsSorted ysOS
    ]

matchN :: Int -> [Point] -> [Point] -> Bool
matchN 0 = \_ _ -> True
matchN n = \case
  [] -> \_ -> False
  x:xs -> \case
    [] -> False
    y:ys ->
      if x == y then matchN (n-1) xs ys
      else
        if x < y
        then matchN n xs (y:ys)
        else matchN n (x:xs) ys

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

invertP :: Point -> Point
invertP (x,y,z) = (-x,-y,-z)

sub :: Point -> Point -> Point
sub (a,b,c) (x,y,z) = (a-x,b-y,c-z)

add :: Point -> Point -> Point
add (p,q,r) (x,y,z) = (p+x,q+y,r+z)

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
  deriving (Eq,Show)

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

invertO :: Orientation -> Orientation
invertO = \case
  O_X_Y_Z -> O_X_Y_Z
  O_Y_Z_X -> O_Z_X_Y
  O_Z_X_Y -> O_Y_Z_X
  O_Z_Y_x -> O_z_Y_X
  O_X_Z_y -> O_X_z_Y
  O_Y_X_z -> O_Y_X_z
  O_x_y_Z -> O_x_y_Z
  O_y_z_X -> O_Z_x_y
  O_z_x_Y -> O_y_Z_x
  O_z_y_x -> O_z_y_x
  O_x_z_y -> O_x_z_y
  O_y_x_z -> O_y_x_z
  O_Y_x_Z -> O_y_X_Z
  O_Z_y_X -> O_Z_y_X
  O_X_z_Y -> O_X_Z_y
  O_Y_z_x -> O_z_X_y
  O_Z_x_y -> O_y_z_X
  O_X_y_z -> O_X_y_z
  O_y_X_Z -> O_Y_x_Z
  O_z_Y_X -> O_Z_Y_x
  O_x_Z_Y -> O_x_Z_Y
  O_y_Z_x -> O_z_x_Y
  O_z_X_y -> O_Y_z_x
  O_x_Y_z -> O_x_Y_z
