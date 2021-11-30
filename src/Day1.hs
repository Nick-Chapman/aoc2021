
module Day1 (main) where

import Misc (check,readInts)

main :: IO ()
main = do
  let sam = [1,2,3,4]
  inp <- readInts "input/day1.input"
  print ("day1, sample", check 10 $ solve sam)
  print ("day1, part1", check 9137 $ solve inp)

solve :: [Int] -> Int
solve xs = sum xs
