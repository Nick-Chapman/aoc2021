module Day3 (main) where

import Data.List (transpose)
import Misc (check)
import Par (Par,parse,separated,nl,many,alts,key)

main :: IO ()
main = do
  sam <- load "input/day3.input.sam"
  inp <- load "input/day3.input"
  print ("day3, part1(sam)", check 198 $ part1 sam)
  print ("day3, part1", check 738234 $ part1 inp)
  print ("day3, part2(sam)", check 230 $ part2 sam)
  print ("day3, part2", check 3969126 $ part2 inp)

load :: FilePath -> IO [Bin]
load path = parse gram <$> readFile path

gram :: Par [Bin]
gram = separated nl line
  where
    line = many bit
    bit = alts [z,o]
    z = do key "0"; pure Z
    o = do key "1"; pure O

data Bit = Z | O deriving (Eq,Show)
type Bin = [Bit]

part1 :: [Bin] -> Integer
part1 bins = (b2d gam * b2d eps)
  where
    eps = map invert gam
    gam = map mostCommon (transpose bins)

part2 :: [Bin] -> Integer
part2 bins = (b2d ogr * b2d csr)
  where
    ogr = filterProcess mostCommon bins
    csr = filterProcess (invert . mostCommon) bins

invert :: Bit -> Bit
invert = \case Z -> O; O -> Z

filterProcess :: ([Bit] -> Bit) -> [Bin] -> Bin
filterProcess f = loop 0
  where
    loop :: Int -> [Bin] -> Bin
    loop pos = \case
      [] -> error "oops!"
      [bin] -> bin
      bins ->
        loop (pos+1) (filter equalAtPos bins)
        where
          col = map (!! pos) bins
          x = f col
          equalAtPos bin = bin !! pos == x

mostCommon :: [Bit] -> Bit
mostCommon bs =
  if length [ () | O <- bs ] >= length [ () | Z <- bs ] then O else Z

b2d :: Bin -> Integer
b2d bs = sum [ 2 ^ i | (i,O) <- zip [0::Integer ..] (reverse bs) ]
