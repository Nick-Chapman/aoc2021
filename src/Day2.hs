module Day2 (main) where

import Misc (check)
import Par (Par,parse,separated,nl,alts,key,int)

main :: IO ()
main = do
  sam <- load "input/day2.input.sam"
  inp <- load "input/day2.input"
  print ("day2, part1(sam)", check 150 $ part1 sam)
  print ("day2, part1", check 1714950 $ part1 inp)
  print ("day2, part2(sam)", check 900 $ part2 sam)
  print ("day2, part2", check 1281977850 $ part2 inp)

load :: FilePath -> IO [Command]
load path = parse gram <$> readFile path

data Command = Forward Int | Down Int | Up Int

gram :: Par [Command]
gram = separated nl line
  where
    line = alts [f,d,u]
    f = do key "forward "; x <- int; pure (Forward x)
    d = do key "down "; x <- int; pure (Down x)
    u = do key "up "; x <- int; pure (Up x)

part1 :: [Command] -> Int
part1 coms = loop (0,0) coms
  where
    loop (h,v) = \case
      [] -> h*v
      com:coms -> case com of
        Forward n -> loop (h+n,v) coms
        Down n -> loop (h,v+n) coms
        Up n -> loop (h,v-n) coms

part2 :: [Command] -> Int
part2 coms = loop (0,0,0) coms
  where
    loop (h,v,a) = \case
      [] -> h*v
      com:coms -> case com of
        Forward n -> loop (h+n,v+a*n,a) coms
        Down n -> loop (h,v,a+n) coms
        Up n -> loop (h,v,a-n) coms
