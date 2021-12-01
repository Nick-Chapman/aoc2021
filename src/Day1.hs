
module Day1 (main) where

import Misc (check,readInts)

main :: IO ()
main = do
  inp <- readInts "input/day1.input"
  print ("day1, part1", check 1316 $ countIncreases inp)
  print ("day1, part2)", check 1344 $ countIncreases (slide3 inp))

countIncreases :: [Int] -> Int
countIncreases xs = length [ () | (me,last) <- zip (tail xs) xs, me>last ]

slide3 :: [Int] -> [Int]
slide3 xs = [ a+b+c | (a,(b,c)) <- zip (tail (tail xs)) (zip (tail xs) xs) ]
