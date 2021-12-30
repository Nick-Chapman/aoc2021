module Day24 (main) where

import Data.Char (ord)
import Data.Map (Map)
import Misc (check,collate)
import ParE (Par,parse,separated,nl,int,lit,key,alts)
import qualified Data.Map as Map

main :: IO ()
main = do
  inp <- load "input/day24.input"
  play inp

load :: FilePath -> IO [Op]
load path = parse gram <$> readFile path

data Op = Inp Reg | Bin1 Bin Reg Int | Bin2 Bin Reg Reg deriving Show
data Bin = Add | Mul | Mod | Div | Eql deriving Show
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
      , do key "mul"; pure Mul
      , do key "mod"; pure Mod
      , do key "div"; pure Div
      , do key "eql"; pure Eql
      ]
    num = alts [ int , do lit '-'; negate <$> int]
    reg = alts
      [ do lit 'w'; pure W
      , do lit 'x'; pure X
      , do lit 'y'; pure Y
      , do lit 'z'; pure Z ]

play :: [Op] -> IO ()
play ops = do
  let choices = take 14 [ Input c | c <- ['A'..] ]
  let p1 = makeProg choices ops
  print $ inlineSingleRefVars p1
  print (check 0 $ test "96929994293996")
  print (check 0 $ test "41811761181141")
  where
    test :: String -> Int
    test ans = eval xs ops
      where xs = [ ord c - ord '0' | c <- ans ]

type Id = String
data Exp = Const Int | Input Char | Node Bin Exp Exp | Var Id
data Prog = Final Exp | Bind Id Exp Prog

instance Show Prog where
  show = \case
    Final e -> "final: " ++ show e
    Bind x e p -> "let " ++ x ++ " = " ++ show e ++ "\n" ++ show p

instance Show Exp where
  show = \case
    Const n -> show n
    Input c -> [c]
    Node o l r -> paren (show l ++ " " ++ opChar o ++ " " ++ show r)
    Var x -> x
    where
      paren s = "(" ++ s ++ ")"
      opChar = \case Add -> "+"; Mul -> "*"; Mod -> "%"; Div -> "/"; Eql -> "=="

eval :: [Int] -> [Op] -> Int
eval inputs ops = res where
  Final (Const res) = makeProg (map Const inputs) ops

data State a = State {w::a,x::a,y::a,z::a}

makeProg :: [Exp] -> [Op] -> Prog
makeProg inputs prog = loop ids0 state0 inputs prog $ \res -> Final res
  where
    set :: Reg -> a -> State a -> State a
    set reg v s = case reg of W -> s {w=v}; X -> s {x=v}; Y -> s {y=v}; Z -> s {z=v}
    get :: Reg -> State a -> a
    get reg State{w,x,y,z} = case reg of W -> w; X -> x; Y -> y; Z -> z
    ids0 :: [Id]
    ids0 = [ "v"++ show n | n <- [1::Int ..] ]
    zero = Const 0
    state0 = State {w=zero,x=zero,y=zero,z=zero}
    loop :: [Id] -> State Exp -> [Exp] -> [Op] -> (Exp -> Prog) -> Prog
    loop ids s xs prog k = case prog of
      [] -> k $ get Z s
      Inp reg:ops -> case xs of
        [] -> error "inp"
        x:xs -> loop ids (set reg x s) xs ops k
      Bin1 o reg1 lit2 : ops -> do
        let s' = set reg1 (simpBin o (get reg1 s) (Const lit2)) s
        loop ids s' xs ops k
      Bin2 o reg1 reg2 : ops -> do
        let e = simpBin o (get reg1 s) (get reg2 s)
        case isAtom e of
          True -> do
            let s' = set reg1 e s
            loop ids s' xs ops k
          False -> do
            let (fresh:ids') = ids
            let s' = set reg1 (Var fresh) s
            Bind fresh e (loop ids' s' xs ops k)

isAtom :: Exp -> Bool
isAtom = \case
  Node{} -> False
  _ -> True

evalBin :: Bin -> Int -> Int -> Int
evalBin bin a b = case bin of
  Add -> a + b
  Mul -> a * b
  Mod -> a `mod` b
  Div -> a `div` b
  Eql -> if a == b then 1 else 0

simpBin :: Bin -> Exp -> Exp -> Exp
simpBin bin t1 t2 =
  case (bin,t1,t2) of
    (_, Const i1, Const i2) ->
      Const (evalBin bin i1 i2)

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
          Const n -> n > 9 || n < 1
          _ -> False
    _ ->
      def
  where def = Node bin t1 t2

inlineSingleRefVars :: Prog -> Prog
inlineSingleRefVars p1 = simpP Map.empty p1
  where
    single = singleUseVars p1

    simpP :: Map Id Exp -> Prog -> Prog
    simpP m = \case
      Final e -> Final (simp m e)
      Bind x e0 p -> do
        let e = simp m e0
        if x `elem` single
        then simpP (Map.insert x e m) p
        else Bind x e (simpP (Map.insert x (Var x) m) p)

    simp :: Map Id Exp -> Exp -> Exp
    simp m = \case
      Var x -> maybe (error (show ("simp",x))) id $ Map.lookup x m
      Node b e1 e2 -> Node b (simp m e1) (simp m e2)
      e@Const{} -> e
      e@Input{} -> e

    singleUseVars :: Prog -> [Id]
    singleUseVars p = qs
      where
        qs = [ x | (x,ns) <- collate [ (x,1::Int) | x <- refsP [] p], sum ns <= 1 ]

        refsP :: [Id] -> Prog -> [Id]
        refsP acc = \case
          Final e -> refs acc e
          Bind _ e p -> refsP (refs acc e) p

        refs :: [Id] -> Exp -> [Id]
        refs acc = \case
          Var x -> x:acc
          Node _ e1 e2 -> refs (refs acc e1) e2
          Const{} -> acc
          Input{} -> acc
