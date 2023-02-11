module Bit where

data Bit =
    B0
  | B1

bitToString :: Bit -> String
bitToString =
  case _ of
    B0 -> "0"
    B1 -> "1"

negate :: Bit -> Bit
negate =
  case _ of
    B0 -> B1
    B1 -> B0

eq :: Bit -> Bit -> Boolean
eq b0 b1 =
  case b0, b1 of
    B0, B0 -> true
    B1, B1 -> true
    _ , _  -> false

-- xor
sum :: Bit -> Bit -> Bit
sum b0 b1 =
  case b0, b1 of
    B0, B0 -> B0
    B1, B1 -> B0
    _ , _  -> B1

min :: Bit -> Bit -> Bit
min b0 b1 =
  case b0, b1 of
    B1, B1 -> B1
    _ , _  -> B0

max :: Bit -> Bit -> Bit
max b0 b1 =
  case b0, b1 of
    B0, B0 -> B0
    _ , _  -> B1
