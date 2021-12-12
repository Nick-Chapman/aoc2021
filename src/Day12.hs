module Day12 (main) where

import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import Misc (check)
import ParE --(Par,parse,separated,nl,int,lit,key)

main :: IO ()
main = do
  sam1 <- load "input/day12.input.sam1"
  sam2 <- load "input/day12.input.sam2"
  sam3 <- load "input/day12.input.sam3"
  inp <- load "input/day12.input"

  print ("day12, part1(sam1)", check 10 $ part1 sam1)
  print ("day12, part1(sam2)", check 19 $ part1 sam2)
  print ("day12, part1(sam3)", check 226 $ part1 sam3)
  print ("day12, part1", check 4659 $ part1 inp)

  print ("day12, part2(sam1)", check 36 $ part2 sam1)
  print ("day12, part2(sam2)", check 103 $ part2 sam2)
  print ("day12, part2(sam3)", check 3509 $ part2 sam3)
  print ("day12, part2", check 148962 $ part2 inp)

load :: FilePath -> IO Setup
load path = parse gram <$> readFile path

gram :: Par Setup
gram = separated nl line
  where
    line = do
      a <- node
      lit '-'
      b <- node
      pure (a,b)
    node = do
      w <- word
      pure $
        if w=="start" then Start else
          if w=="end" then End else
            if Char.isUpper (head w) then Big w else Small w

type Setup = [Connection]
type Connection = (Node,Node)
data Node = Start | End | Small String | Big String deriving (Eq,Ord)

instance Show Node where
  show = \case
    Start -> "start"
    End -> "end"
    Big w -> w
    Small w -> w

type Path = [Node]


part1 :: Setup -> Int
part1 setup = do
  length (explore 0 [Start] Start)
  where
    m :: Map Node (Set Node)
    m = Map.fromListWith Set.union
      [ (k,Set.fromList [v]) | (a,b) <- setup, (k,v) <- [(a,b),(b,a)] ]

    step :: Node -> [Node]
    step n = maybe [] Set.toList $ Map.lookup n m

    explore :: Int -> [Node] -> Node -> [Path]
    explore i acc n1 =
       if n1==End then [reverse acc] else
         [ path
         | n2 <- step n1
         , okToContinue n2
         , path <- explore (i-1) (n2:acc) n2 ]
      where
        okToContinue :: Node -> Bool
        okToContinue n2 =
          case n2 of
            Start -> False
            End -> True
            Small{} -> n2 `notElem` acc
            Big{} -> True


part2 :: Setup -> Int
part2 setup = do
  length (explore 0 [Start] Start)
  where
    m :: Map Node (Set Node)
    m = Map.fromListWith Set.union
      [ (k,Set.fromList [v]) | (a,b) <- setup, (k,v) <- [(a,b),(b,a)] ]

    step :: Node -> [Node]
    step n = maybe [] Set.toList $ Map.lookup n m

    explore :: Int -> [Node] -> Node -> [Path]
    explore i acc n1 =
       if n1==End then [reverse acc] else
         [ path
         | n2 <- step n1
         , okToContinue n2
         , path <- explore (i-1) (n2:acc) n2 ]
      where
        okToContinue :: Node -> Bool
        okToContinue n2 =
          case n2 of
            Start -> False
            End -> True
            Small{} -> n2 `notElem` acc || noSmallDups acc
            Big{} -> True


noSmallDups :: [Node] -> Bool
noSmallDups ns = do
  let smalls = [ w | Small w <- ns ]
  Set.size (Set.fromList smalls) == length smalls
