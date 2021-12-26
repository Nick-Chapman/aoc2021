module DayX (main) where

import Misc (check)
import Par4 --(Par,parse,separated,nl,int,lit,key)

main :: IO ()
main = do
  sam <- load "input/dayX.input.sam"
  inp <- load "input/dayX.input"
  print ("dayX, part1(sam)", check 0 $ part1 sam)
  print ("dayX, part1", check 0 $ part1 inp)
  print ("dayX, part2(sam)", check 0 $ part2 sam)
  print ("dayX, part2", check 0 $ part2 inp)

load :: FilePath -> IO Setup
load path = parse gram <$> readFile path

gram :: Par [Line]
gram = separated nl undefined

type Setup = [Line]
data Line

part1 :: Setup -> Int
part1 = undefined

part2 :: Setup -> Int
part2 = undefined
