module Day16 (main) where

import Control.Monad (ap,liftM)
import Misc (check)
import Par4 (Par,parse,lit,many,alts,digit)

main :: IO ()
main = do

  let
    play :: [Hex] -> IO ()
    play hs = print (runP theParser hs)
  play (parse gram "D2FE28")
  play (parse gram "38006F45291200")
  play (parse gram "EE00D40C823060")

  let
    checkPart1 :: String -> Int -> IO ()
    checkPart1 s n =
      print ("day16, part1("++s++")", check n $ part1 (parse gram s))

  checkPart1 "8A004A801A8002F478" 16
  checkPart1 "620080001611562C8802118E34" 12
  checkPart1 "C0015000016115A2E0802F182340" 23
  checkPart1 "A0016C880162017C3686B18A3D4780" 31

  inp <- load "input/day16.input"
  print ("day16, part1", check 925 $ part1 inp)

  let
    checkPart2 :: String -> Int -> IO ()
    checkPart2 s n =
      print ("day16, part1("++s++")", check n $ part2 (parse gram s))

  checkPart2 "C200B40A82" 3
  checkPart2 "04005AC33890" 54
  checkPart2 "880086C3E88112" 7
  checkPart2 "CE00C43D881120" 9
  checkPart2 "D8005AC2A8F0" 1
  checkPart2 "F600BC2D8F" 0
  checkPart2 "9C005AC2F8F0" 0
  checkPart2 "9C0141080250320F1802104A08" 1

  print ("day16, part2", check 342997120375 $ part2 inp)


load :: FilePath -> IO [Hex]
load path = parse gram <$> readFile path

gram :: Par [Hex]
gram = many hex
  where
    hex :: Par Hex
    hex = Hex <$> alts [digit,a,b,c,d,e,f]
    a = do lit 'A'; pure 10
    b = do lit 'B'; pure 11
    c = do lit 'C'; pure 12
    d = do lit 'D'; pure 13
    e = do lit 'E'; pure 14
    f = do lit 'F'; pure 15

part1 :: [Hex] -> Int
part1 hs = sumVersions (runP theParser hs)

part2 :: [Hex] -> Int
part2 hs = eval (runP theParser hs)


newtype Version = Version Int deriving Show
data Packet = Packet Version Body deriving Show
data Body = Lit Int | Operator Op [Packet] deriving Show
data Op = OpSum | OpProd | OpMin | OpMax | OpGT | OpLT | OpEQ deriving (Eq,Show)

decodeOp :: Int -> Op
decodeOp = \case
  0 -> OpSum
  1 -> OpProd
  2 -> OpMin
  3 -> OpMax
  5 -> OpGT
  6 -> OpLT
  7 -> OpEQ
  _ -> error "decodeOp"

sumVersions :: Packet -> Int
sumVersions (Packet (Version n) body) =
  case body of
    Lit{} -> n
    Operator _ ps -> n + sum (map sumVersions ps)

eval :: Packet -> Int
eval (Packet _ body) =
  case body of
    Lit i -> i
    Operator op ps ->
      case op of
        OpSum -> sum (map eval ps)
        OpProd -> product (map eval ps)
        OpMax -> maximum (map eval ps)
        OpMin -> minimum (map eval ps)
        OpGT -> if eval p1 > eval p2 then 1 else 0
        OpLT -> if eval p1 < eval p2 then 1 else 0
        OpEQ -> if eval p1 == eval p2 then 1 else 0
      where
        [p1,p2] = ps

theParser :: P Packet
theParser = packet

packet :: P Packet
packet = do
  v <- version
  k <- kind
  case k of
    KLit -> do
      body <- literal
      pure (Packet v body)
    KOper op -> do
      body <- oper op
      pure (Packet v body)

version :: P Version
version = do
  a <- bit
  b <- bit
  c <- bit
  pure $ Version (bits2int [a,b,c])

data Kind = KLit | KOper Op deriving Eq

kind :: P Kind
kind = do
  a <- bit
  b <- bit
  c <- bit
  let n = bits2int [a,b,c]
  pure (if n==4 then KLit else KOper (decodeOp n))

literal :: P Body
literal = Lit <$> loop 0
  where
    loop :: Int -> P Int
    loop acc = do
      continue <- bit
      v <- quad
      case continue of
        B1 -> loop (16*acc+v)
        B0 -> pure (16*acc+v)

quad :: P Int
quad = do
  a <- bit
  b <- bit
  c <- bit
  d <- bit
  let n = bits2int [a,b,c,d]
  pure n

oper :: Op -> P Body
oper op = do
  ot >>= \case
    T1_totalLength -> do
      n <- length15
      ps <- takeBits n manyPackets
      pure (Operator op ps)

    T2_numberSubPackets -> do
      n <- length11
      ps <- countedPackets n
      pure (Operator op ps)

manyPackets :: P [Packet]
manyPackets = loop []
  where
    loop acc = do
      atEof >>= \case
        True -> pure (reverse acc)
        False -> do
          p1 <- packet
          loop (p1:acc)

countedPackets :: Int -> P [Packet]
countedPackets n = sequence (take n (repeat packet))

length15 :: P Int
length15 = bits2int <$> sequence (take 15 (repeat bit))

length11 :: P Int
length11 = bits2int <$> sequence (take 11 (repeat bit))

bits2int :: [Bit] -> Int
bits2int = loop 0
  where
    loop :: Int -> [Bit] -> Int
    loop acc = \case
      [] -> acc
      B0:xs -> loop (2*acc) xs
      B1:xs -> loop (2*acc + 1) xs

data OperType = T1_totalLength | T2_numberSubPackets

ot :: P OperType
ot = do
  b <- bit
  pure $ case b of B0 -> T1_totalLength; B1 -> T2_numberSubPackets

instance Functor P where fmap = liftM
instance Applicative P where pure = return; (<*>) = ap
instance Monad P where return = retP; (>>=) = bindP

data P a = P ([Bit] -> (a,[Bit]))

takeBits :: Int -> P a -> P a
takeBits n (P p1) = P $ \xs -> do
  let (ys,zs) = splitAt n xs
  let (res,ys') = p1 ys
  case ys' of
    [] -> (res,zs)
    _ -> error "takeBits"

atEof :: P Bool
atEof = P $ \xs ->
  case xs of
    [] -> (True,xs)
    _ -> (False,xs)

bit :: P Bit
bit = P $ \xs ->
  case xs of
    [] -> error "bit[]"
    x:xs -> (x,xs)

retP :: a -> P a
retP x = P $ \xs -> (x,xs)

bindP :: P a -> (a -> P b) -> P b
bindP (P p1) f = P $ \xs -> do
  let (a,xs') = p1 xs
  let (P p2) = f a
  p2 xs'

runP :: P a -> [Hex] -> a
runP (P f) hs = fst (f (hexs2bits hs))

hexs2bits :: [Hex] -> [Bit]
hexs2bits hs = [ b | h <- hs, b <- hex2bits h ]

hex2bits :: Hex -> [Bit]
hex2bits (Hex n) = case n of
  0 -> [B0,B0,B0,B0]
  1 -> [B0,B0,B0,B1]
  2 -> [B0,B0,B1,B0]
  3 -> [B0,B0,B1,B1]
  4 -> [B0,B1,B0,B0]
  5 -> [B0,B1,B0,B1]
  6 -> [B0,B1,B1,B0]
  7 -> [B0,B1,B1,B1]
  8 -> [B1,B0,B0,B0]
  9 -> [B1,B0,B0,B1]
  10 -> [B1,B0,B1,B0]
  11 -> [B1,B0,B1,B1]
  12 -> [B1,B1,B0,B0]
  13 -> [B1,B1,B0,B1]
  14 -> [B1,B1,B1,B0]
  15 -> [B1,B1,B1,B1]
  _ -> error "hex2bits"

newtype Hex = Hex Int

data Bit = B0 | B1
