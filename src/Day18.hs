module Day18 (main) where

import Misc (check)
import Par4 (Par,parse,separated,nl,lit,alts,digit)

main :: IO ()
main = do
  sam <- load "input/day18.input.sam"
  inp <- load "input/day18.input"
  print ("day18, part1(sam)", check 4140 $ part1 sam)
  print ("day18, part1", check 3793 $ part1 inp)
  print ("day18, part2(sam)", check 3993 $ part2 sam)
  print ("day18, part2", check 4695 $ part2 inp)
  pure ()

load :: FilePath -> IO [N]
load path = parse gram <$> readFile path

gram :: Par [N]
gram = separated nl n
  where
    n = alts [p,l]
    l = L <$> digit
    p = do lit '['; a <- n; lit ','; b <- n; lit ']'; pure $ P a b

data N = P N N | L Int deriving Eq

instance Show N where
  show = \case P a b -> "[" ++ show a ++ "," ++ show b ++ "]"; L n -> show n

part1 :: [N] -> Int
part1 ns = loop (head ns) (tail ns) where
  loop x1 = \case
    [] -> magnitude x1
    x2:xs -> loop (add x1 x2) xs

part2 :: [N] -> Int
part2 ns = maximum [ magnitude (add a b) | a <- ns , b <- ns ]

add :: N -> N -> N
add a b = redstar (P a b)

redstar :: N -> N
redstar n = case red n of Nothing -> n; Just n -> redstar n

red :: N -> Maybe N
red n = case explode n of Nothing -> split n; j@Just{} -> j

magnitude :: N -> Int
magnitude = \case L x -> x; P a b -> 3 * magnitude a + 2 * magnitude b

explode :: N -> Maybe N
explode n = case focus 0 n of Nothing -> Nothing; Just (_,n,_) -> Just n
  where
    focus :: Int -> N -> Maybe (Maybe Int, N, Maybe Int)
    focus i = \case
      L{} -> Nothing
      P (L a) (L b) | i==4 -> Just (Just a, L 0, Just b)
      P a b -> do
        case focus (i+1) a of
          Just(x,a',Nothing) -> Just (x,P a' b,Nothing)
          Just(x,a',Just y) -> Just (x,P a' (pushL y b), Nothing)
          Nothing ->
            case focus (i+1) b of
              Just(Nothing,b',y) -> Just (Nothing, P a b', y)
              Just(Just x,b',y) -> Just (Nothing, P (pushR x a) b', y)
              Nothing -> Nothing

    pushL i = \case L j -> L (i+j); P a b -> P (pushL i a) b
    pushR i = \case L j -> L (i+j); P a b -> P a (pushR i b)

split :: N -> Maybe N
split = \case
  L i -> if i < 10 then Nothing else Just $ P (L (i `div` 2)) (L ((i+1) `div` 2))
  P a b ->
    case split a of
      Just a' -> Just $ P a' b
      Nothing ->
        case split b of
          Just b' -> Just $ P a b'
          Nothing -> Nothing
