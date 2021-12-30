module Day24 (main) where

import Data.List (sort)
import Misc (check)
import Par4 (Par,parse,separated,nl,int,lit,key,alts)

main :: IO ()
main = do
  inp <- load "input/day24.input"
  play inp
  print ("day24, part1", check 96929994293996 $ part1 inp)
  print ("day24, part2", check 41811761181141 $ part2 inp)
  --gary <- load "input/day24.input.gary"
  --print ("day24, part1(Gary)", check 98491959997994 $ part1 gary)
  --print ("day24, part2(Gary)", check 61191516111321 $ part2 gary)
    where
      part1 = solve Part1
      part2 = solve Part2

data Part = Part1 | Part2

load :: FilePath -> IO [Op]
load path = parse gram <$> readFile path

data Op = Inp Reg | Bin1 Bin Reg Int | Bin2 Bin Reg Reg deriving Show
data Bin = Add | Mul | Mod | Div | Eql | Shift deriving Show
data Reg = W | X | Y | Z deriving Show

gram :: Par [Op]
gram = separated nl op
  where
    op = alts [binop,inp]
    inp = do key "inp "; Inp <$> reg
    binop = do
      o <- bin
      lit ' '
      l <- reg
      lit ' '
      alts
        [ do n <- num; pure (Bin1 o l n)
        , do r <- reg; pure (Bin2 o l r) ]
    bin = alts
      [ do key "add"; pure Add
      , do lit 'm'; alts [ do key "ul"; pure Mul
                         , do key "od"; pure Mod ]
      , do key "div"; pure Div
      , do key "eql"; pure Eql
      ]
    num = alts [ int , do lit '-'; negate <$> int]
    reg = alts
      [ do lit 'w'; pure W
      , do lit 'x'; pure X
      , do lit 'y'; pure Y
      , do lit 'z'; pure Z ]


instance Show Prog where
  show = \case
    Final e -> "final: " ++ show e
    Assert e p -> "assert: " ++ show e ++ "\n" ++ show p

instance Show Exp where
  show = \case
    Const n -> show n
    Input c -> [c]
    Node o l r -> paren (show l ++ " " ++ opChar o ++ " " ++ show r)
    M26 e -> paren (show e ++ " *26")
    where
      paren s = "(" ++ s ++ ")"
      opChar = \case Add -> "+"; Mul -> "*"; Mod -> "%"; Div -> "/"; Eql -> "=="; Shift -> "@"


play :: [Op] -> IO ()
play ops = do
  print $ makeProg symbolicInputs ops

solve :: Part -> [Op] -> Int
solve part ops = do
  let prog = makeProg symbolicInputs ops
  let solve = case part of Part1 -> solveHigh; Part2 -> solveLow
  let xs = collectConstraints prog >>= solve
  sum [ v * 10 ^ n
      | (n,(_,v)) <- zip [0::Int ..] (reverse (sort xs))
      ]

symbolicInputs :: [Exp]
symbolicInputs = take 14 [ Input c | c <- ['A'..] ]

type InputChoice = (InputId,Int)
type InputId = Char

collectConstraints :: Prog -> [Con]
collectConstraints = \case
  Final{} -> []
  Assert (Node Eql (Node Add (Input a) (Const n)) (Input b)) p  ->
    Con a n b : collectConstraints p
  Assert (Node Eql (Node Add (Node Add (Input a) (Const m)) (Const n)) (Input b)) p  ->
    Con a (m+n) b : collectConstraints p
  Assert{} ->
    error "collectConstraints"

data Con = Con Char Int Char deriving Show

solveHigh :: Con -> [InputChoice]
solveHigh (Con a0 n0 b0) = do
  let (a,n,b) = if a0 < b0 then (a0,n0,b0) else (b0,-n0,a0)
  if n == 0 then [(a,9),(b,9)] else
    if n < 0 then [(a,9),(b,9+n)] else
      [(a,9-n),(b,9)]

solveLow :: Con -> [InputChoice]
solveLow (Con a0 n0 b0) = do
  let (a,n,b) = if a0 < b0 then (a0,n0,b0) else (b0,-n0,a0)
  if n == 0 then [(a,1),(b,1)] else
    if n < 0 then [(a,1-n),(b,1)] else
      [(a,1),(b,1+n)]

data Exp = Const Int | Input Char | Node Bin Exp Exp | M26 Exp
data Prog = Final Exp | Assert Exp Prog

data State a = State {w::a,x::a,y::a,z::a}

makeProg :: [Exp] -> [Op] -> Prog
makeProg inputs prog = loop state0 inputs prog $ \res -> Final res
  where
    set :: Reg -> a -> State a -> State a
    set reg v s = case reg of W -> s {w=v}; X -> s {x=v}; Y -> s {y=v}; Z -> s {z=v}
    get :: Reg -> State a -> a
    get reg State{w,x,y,z} = case reg of W -> w; X -> x; Y -> y; Z -> z
    zero = Const 0
    state0 = State {w=zero,x=zero,y=zero,z=zero}
    loop :: State Exp -> [Exp] -> [Op] -> (Exp -> Prog) -> Prog
    loop s xs prog k = case prog of
      [] -> k $ get Z s
      Inp reg:ops -> case xs of
        [] -> error "inp"
        x:xs -> loop (set reg x s) xs ops k
      Bin1 o reg1 lit2 : ops -> do
        let s' = set reg1 (simpBin o (get reg1 s) (Const lit2)) s
        loop s' xs ops k
      Bin2 o reg1 reg2 : ops -> do
        let lhs = get reg1 s
        let rhs = get reg2 s
        let e0 = simpBin o lhs rhs
        let
          tooBig :: Exp -> Bool
          tooBig = \case
            Const n -> n > 9
            Node Add _ (Const n) -> n > 9
            _ -> False
          (f,e) =
            case (o,tooBig lhs,rhs) of
              (Eql,False,Input{}) -> (Assert e0, Const 1)
              _ -> (id, e0)
        let s' = set reg1 e s
        f $ loop s' xs ops k

evalBin :: Bin -> Int -> Int -> Int
evalBin bin a b = case bin of
  Add -> a + b
  Mul -> a * b
  Mod -> a `mod` b
  Div -> a `div` b
  Eql -> if a == b then 1 else 0
  Shift -> a * 26 + b

simpBin :: Bin -> Exp -> Exp -> Exp
simpBin bin t1 t2 =
  case (bin,t1,t2) of
    (_, Const i1, Const i2) -> Const (evalBin bin i1 i2)
    (Mul, a, Const 26) -> M26 a
    (Add, M26 a, b) -> Node Shift a b
    (Mod, (Node Shift _ b), Const 26) -> b
    (Div, (Node Shift a _), Const 26) -> a
    (Mod, a, Const 26) -> a
    (Div, _, Const 26) -> Const 0
    (Mul, Const 0, _) -> Const 0
    (Mul, Const 1, _) -> t2
    (Add, Const 0, _) -> t2
    (Mul, _, Const 0) -> Const 0
    (Mul, _, Const 1) -> t1
    (Add, _, Const 0) -> t1
    (Div, _, Const 1) -> t1
    (Eql, e, Input{}) ->
      if defNotADigit e then Const 0 else def
      where
        defNotADigit :: Exp -> Bool
        defNotADigit = \case
          Node Add (Node Mod _ (Const 26)) (Const n) -> n > 9
          Node Add _ (Const n) -> n > 9
          Const n -> n > 9 || n < 1
          _ -> False
    _ ->
      def
  where def = Node bin t1 t2
