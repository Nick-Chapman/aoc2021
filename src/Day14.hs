module Day14 (main) where

import Data.Map (Map)
import Misc (check)
import Par4 (Par,parse,separated,nl,key,word,char)
import qualified Data.Map as Map

main :: IO ()
main = do
  sam <- load "input/day14.input.sam"
  inp <- load "input/day14.input"
  print ("day14, part1(sam)", check 1588 $ part1 sam)
  print ("day14, part1", check 2447 $ part1 inp)
  print ("day14, part2(sam)", check 2188189693529 $ part2 sam)
  print ("day14, part2", check 3018019237563 $ part2 inp)

load :: FilePath -> IO Setup
load path = parse gram <$> readFile path

gram :: Par Setup
gram = do w <- word; nl; nl; rs <- separated nl rule; pure (w,rs)
  where rule = do a <- char; b <- char; key " -> "; c <- char; pure ((a,b),c)

type Setup = (String,[Rule])
type Rule = (Pair,Char)
type Pair = (Char,Char)

type Step = Map Pair (Pair,Pair)
type State = Map Pair Int

part1 :: Setup -> Int
part1 = run 10

part2 :: Setup -> Int
part2 = run 40

run :: Int -> Setup -> Int
run n (w,rs) = res
  where
    res = final lastChar (states !! n)
    lastChar = head (reverse w)
    states = iterate (runStep step) state0
    step = mkStep rs
    state0 = initState w

mkStep :: [Rule] -> Step
mkStep rs = Map.fromList [ ((a,b),((a,c),(c,b))) | ((a,b),c) <- rs ]

initState :: String -> State
initState s = Map.fromListWith (+) [ (p,1) | p <- zip s (tail s) ]

runStep :: Step -> State -> State
runStep step s =
  Map.fromListWith (+)
  [ (q,f) | (p,f) <- Map.toList s
          , let (p1,p2) = maybe undefined id $ Map.lookup p step
          , q <- [p1,p2]
          ]

final :: Char -> State -> Int
final lastChar s = maximum freqs - minimum freqs
  where
    freqs = [ f `div` 2 | (_,f) <- Map.toList fm ]
    fm = Map.fromListWith (+)
         ((lastChar,1) : [ (c,f) | ((a,b),f) <- Map.toList s, c <- [a,b] ])
