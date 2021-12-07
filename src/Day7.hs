module Day7 (main) where

import Misc (check)
import Par4 (Par,parse,separated,int,lit)

main :: IO ()
main = do
  sam <- load "input/day7.input.sam"
  inp <- load "input/day7.input"
  print ("day7, part1(sam)", check 37 $ part1 sam)
  print ("day7, part1", check 333755 $ part1 inp)
  print ("day7, part2(sam)", check 168 $ part2 sam)
  print ("day7, part2", check 94017638 $ part2 inp)

load :: FilePath -> IO [Int]
load path = parse gram <$> readFile path

gram :: Par [Int]
gram = separated (lit ',') int

type Setup = [Int]

part1 :: Setup -> Int
part1 xs = res
  where
    res = minimum [ cost i | i <- [minimum xs .. maximum xs ] ]
    cost p = sum [ abs (p-x) | x <- xs ]

part2 :: Setup -> Int
part2 xs = res
  where
    res = minimum [ cost i | i <- [minimum xs .. maximum xs ] ]
    cost p = sum [ triangle $ abs (p-x) | x <- xs ]

triangle :: Int -> Int
triangle x = (x * (x+1)) `div` 2
