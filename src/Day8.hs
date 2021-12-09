module Day8 (main) where

import Data.Set (Set)
import Data.List (permutations)
import qualified Data.Set as Set
import Misc (check)
import Par4 (Par,parse,separated,nl,key,some,alts,opt,sp)

main :: IO ()
main = do
  sam <- load "input/day8.input.sam"
  inp <- load "input/day8.input"
  print ("day8, part1(sam)", check 26 $ part1 sam)
  print ("day8, part1", check 416 $ part1 inp)
  print ("day8, part2(sam)", check 61229 $ part2 sam)
  print ("day8, part2", check 1043697 $ part2 inp)

load :: FilePath -> IO Setup
load path = parse gram <$> readFile path

gram :: Par [Line]
gram = separated nl line
  where
    line = do x <- obs; key "| "; y <- out; pure (x,y)
    obs = Obs <$> some ss
    out = Out <$> some ss
    ss = nibble (Set.fromList <$> some seg)
    seg = alts[a,b,c,d,e,f,g]
    a = do key "a"; pure A
    b = do key "b"; pure B
    c = do key "c"; pure C
    d = do key "d"; pure D
    e = do key "e"; pure E
    f = do key "f"; pure F
    g = do key "g"; pure G
    nibble p = do x <- p; _ <- opt sp; pure x

type Setup = [Line]
type Line = (Obs,Out)
data Obs = Obs [SS] deriving (Eq,Show) -- #10 observations
data Out = Out [SS] deriving (Eq,Show) -- #4 output digits
type SS = Set Seg -- Segment Set
data Seg = A | B | C | D | E | F | G deriving (Eq,Ord,Show)

part1 :: Setup -> Int
part1 lines = length
  [ ss
  | (_,Out out) <- lines
  , ss <- out
  , let n = Set.size ss
  , n == 2 || n == 3 || n == 4 || n == 7
  ]

part2 :: Setup -> Int
part2 lines = sum (map decode lines)

decode :: Line -> Int
decode (obs,Out out) = do
  let mix = deduce obs
  let [a,b,c,d] = map (reveal mix) out
  (a*1000 + b*100 + c*10 + d)

deduce :: Obs -> Mix
deduce (Obs sss) = the [ mix | mix <- allMix, all (checkMix mix) sss ]

reveal :: Mix -> SS -> Digit
reveal mix ss = the [ d | d <- [0..9], applyMix mix ss == ssOfDigit d ]

data Mix = Mix [Seg] deriving Show -- one of the (7!) 5760 perms

allMix :: [Mix]
allMix = [ Mix xs | xs <- permutations [A,B,C,D,E,F,G] ]

checkMix :: Mix -> SS -> Bool
checkMix mix ss = Set.member (applyMix mix ss) legalSS

legalSS :: Set SS
legalSS = Set.fromList (map ssOfDigit [0..9])

applyMix :: Mix -> SS -> SS
applyMix mix = Set.fromList . map (applyMix1 mix) . Set.toList

applyMix1 :: Mix -> Seg -> Seg
applyMix1 (Mix ys) x' = the [ y | (x,y) <- zip [A,B,C,D,E,F,G] ys, x==x' ]

-- 'the' is bit like 'head', but insists the input list has exactly one element
the :: [a] -> a
the = \case [a] -> a; [] -> error "the[]"; _ -> error "the>=2"

type Digit = Int -- 0..9

ssOfDigit :: Digit -> SS
ssOfDigit = Set.fromList .
  \case
    0 -> [A,B,C,  E,F,G]
    1 -> [    C,    F  ]
    2 -> [A,  C,D,E,  G]
    3 -> [A,  C,D,  F,G]
    4 -> [B,  C,D,  F  ]
    5 -> [A,B,  D,  F,G]
    6 -> [A,B,  D,E,F,G]
    7 -> [A,  C,    F  ]
    8 -> [A,B,C,D,E,F,G]
    9 -> [A,B,C,D  ,F,G]
    _ -> error "ssOfDigit"
