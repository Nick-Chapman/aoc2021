module Day25 (main) where

import Misc (check)
import Par4 (Par,parse,separated,nl,lit,some,alts)

import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- load "input/day25.input.sam"
  inp <- load "input/day25.input"
  print ("day25, part1(sam)", check 58 $ part1 sam)
  print ("day25, part1", check 384 $ part1 inp)

load :: FilePath -> IO Grid
load path = parse gram <$> readFile path

gram :: Par Grid
gram = Grid <$> separated nl line
  where
    line = some cell
    cell = alts
      [ do lit '>'; pure East
      , do lit 'v'; pure South
      , do lit '.'; pure Empty
      ]

data Grid = Grid [[Cell]]
data Cell = East | South | Empty deriving Eq

part1 :: Grid -> Int
part1 g = length $ whileMoving (iterate step (toHerds g))

whileMoving :: [Herds] -> [Herds]
whileMoving = \case
  h:hs@(h2:_) -> if h==h2 then [h] else h : whileMoving hs
  _ -> error "whileMoving"

instance Show Grid where
  show (Grid xss) = unlines (map (concat . map show) xss)

instance Show Cell where
  show = \case East -> ">"; South -> "v"; Empty -> "."

data Herds = Herds { h :: Int, w :: Int, east :: Set Pos, south :: Set Pos }
  deriving (Eq,Show)
type Pos = (Int,Int)

toHerds :: Grid -> Herds
toHerds (Grid vss) = Herds {h,w,east,south}
  where
    h = length vss
    w = length (head vss)
    east = Set.fromList [ (x,y) | (y,vs) <- zip [0..] vss, (x,East) <- zip [0..] vs ]
    south = Set.fromList [ (x,y) | (y,vs) <- zip [0..] vss, (x,South) <- zip [0..] vs ]

step :: Herds -> Herds
step herds@Herds{w,h,east,south} = do
  let
    taken = east `Set.union` south
    moveE p@(x,y) = if q `Set.member` taken then p else q where q = ((x+1) `mod` w, y)
    east1 = Set.map moveE east
    taken1 = east1 `Set.union` south
    moveS p@(x,y) = if q `Set.member` taken1 then p else q where q = (x, (y+1) `mod` h)
    south1 = Set.map moveS south
  herds { east = east1 , south = south1 }
