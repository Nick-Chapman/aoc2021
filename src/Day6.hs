module Day6 (main) where

import Misc (check)
import Par4 (Par,parse,separated,int,lit)

main :: IO ()
main = do
  sam <- load "input/day6.input.sam"
  inp <- load "input/day6.input"
  print ("day6, part1(sam)", check 5934 $ part1 sam)
  print ("day6, part1", check 360610 $ part1 inp)
  print ("day6, part2(sam)", check 26984457539 $ part2 sam)
  print ("day6, part2", check 1631629590423 $ part2 inp)

load :: FilePath -> IO [Int]
load path = parse gram <$> readFile path

gram :: Par [Int]
gram = separated (lit ',') int

part1 :: [Int] -> Int
part1 = run 80

part2 :: [Int] -> Int
part2 = run 256

run :: Int -> [Int] -> Int
run n = collapse . (!!n) . iterate step . initialize

type State = (Int,Int,Int,Int,Int,Int,Int,Int,Int)

initialize :: [Int] -> State
initialize nums = (0,0, x 6, x 5, x 4, x 3, x 2, x 1, x 0) where
  x q = fromIntegral $ length [ () | n <- nums, n == q ]

collapse :: State -> Int
collapse (x,y,a,b,c,d,e,f,g) = x+y+a+b+c+d+e+f+g

step :: State -> State
step = \(x,y, a,b,c,d,e,f,g) -> (g,x, y+g, a,b,c,d,e,f)
