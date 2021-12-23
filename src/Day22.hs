module Day22 (main) where

import Data.List (sort,nub)
import Data.Maybe (maybeToList)
import Misc (check)
import Par4 (Par,parse,separated,nl,int,lit,key,alts)

main :: IO ()
main = do
  sam <- load "input/day22.input.sam"
  sam2 <- load "input/day22.input.sam2"
  inp <- load "input/day22.input"

  -- The part1 sample
  s1 <- part1 sam
  print ("day22, part1(sam)", check 590784 $ s1)

  -- The real part1 run...
  i1 <- part1 inp
  print ("day22, part1", check 644257 $ i1)

  -- The part2 sample, used for part1
  t1 <- part1 sam2
  print ("day22, part1(sam2)", check 474140 $ t1)

  -- The part2 sample, used for part2
  t2 <- part2 sam2
  print ("day22, part2(sam2)", check 2758514936282235 $ t2)

  -- The real part2 run...
  --i2 <- part2 inp
  --print ("day22, part2", check 1235484513229032 $ i2) -- 40 minutes


  -- New approach for part2 WIP... fast; but wrong answer :(

  let dropLast2 = reverse . tail . tail . reverse
  part2new (dropLast2 sam) >>= print
  part2new sam2 >>= print
  --part2new inp >>= print

  --t2new <- part2new sam2
  --print ("day22, new-part2(sam2)", check 2758514936282235 $ t2new)

  pure ()

load :: FilePath -> IO Setup
load path = parse gram <$> readFile path

gram :: Par [Line]
gram = separated nl line where
  line = do sw <- switch; key " x="; x <- range; key ",y="; y <- range; key ",z="; z <- range; pure (sw,(x,y,z))
  range = do a <- num; key ".."; b <- num; pure (a,b)
  switch = do lit 'o'; alts [do key "n"; pure On, do key "ff"; pure Off]
  num = alts [ int , do lit '-'; negate <$> int]

type Setup = [Line]
type Line = (Switch,R3)
type R3 = (Range,Range,Range)
type Range = (Int,Int)
data Switch = On | Off deriving (Eq,Show)

type Point = (Int,Int,Int)

part2 :: Setup -> IO Int
part2 = run id

part1 :: Setup -> IO Int
part1 = run bound
  where
    -- TODO: what's the right word, not "bound"
    bound :: Int -> Int
    bound a = min (max a (-100)) 101

run :: (Int -> Int) -> Setup -> IO Int
run bound setup = pure $ sum
  [ sx*sy*sz
  | rx@(x,_) <- zip xs (tail xs)
  , let sx = sizeRange rx
  , ry@(y,_) <- zip ys (tail ys)
  , let sy = sizeRange ry
  , rz@(z,_) <- zip zs (tail zs)
  , let sz = sizeRange rz
  , final (x,y,z) == On
  ]
  where
    sizeRange :: Range -> Int
    sizeRange (a,b) = max 0 (b - a)

    reversedSetup = reverse setup
    final :: Point -> Switch
    final p = head ([ sw | (sw,r3) <- reversedSetup, p `withinR3` r3 ] ++ [Off])

    xs = nub $ sort [ bound x | (_,((a,b),_,_)) <- setup, x <- [a,b+1] ]
    ys = nub $ sort [ bound y | (_,(_,(a,b),_)) <- setup, y <- [a,b+1] ]
    zs = nub $ sort [ bound z | (_,(_,_,(a,b))) <- setup, z <- [a,b+1] ]

withinR3 :: Point -> R3 -> Bool
withinR3 (x,y,z) (r1,r2,r3) = within x r1 && within y r2 && within z r3

within :: Int -> Range -> Bool
within x (a,b) = x >= a && x <= b


part2new :: Setup -> IO Int
part2new lines = pure $ countOn $ foldl addLine state0 lines

data State = State { pos :: [R3], neg :: [R3] }

state0 :: State
state0 = State { pos = [], neg = [] }

countOn :: State -> Int
countOn State{pos,neg} =
  sum (map volumeR3 pos) - sum (map volumeR3 neg)

addLine :: State -> Line -> State
addLine State{pos,neg} (switch,cube) =
  case switch of
    On ->
       State
       { pos = cube : intersections cube neg ++ pos
       , neg =        intersections cube pos ++ neg
       }

    Off ->
       State
       { pos = intersections cube neg ++ pos
       , neg = intersections cube pos ++ neg
       }

intersections :: R3 -> [R3] -> [R3]
intersections x ys = [ a | y <- ys, a <- maybeToList (x `interR3` y) ]

volumeR3 :: R3 -> Int
volumeR3 (x,y,z) = sizeRange x * sizeRange y * sizeRange z

sizeRange :: Range -> Int
sizeRange (a,b) = b-a

interR3 :: R3 -> R3 -> Maybe R3
interR3 (p,q,r) (a,b,c) =
  case (interRange p a ,interRange q b ,interRange r c) of
    (Just x, Just y, Just z) -> Just (x,y,z)
    _ -> Nothing

interRange :: Range -> Range -> Maybe Range
interRange (a,b) (c,d) = do
  let j = max a c
  let k = min b d
  if j<k then Just (j,k) else Nothing
