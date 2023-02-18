module Base.Bit where

import Prelude
import Data.List ( List(..), (:) )
import Data.List as List
import Data.Maybe ( Maybe(..) )

data Bit =
    B0
  | B1

toString :: Bit -> String
toString =
  case _ of
    B0 -> "0"
    B1 -> "1"

instance showBit :: Show Bit where
  show = toString 

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


firstBit :: Int -> Maybe { bit :: Bit, rest :: Int }
firstBit n =
  if n == 0 then
    Nothing
  else
    Just { bit: if mod n 2 == 0 then B0 else B1, rest: n / 2 }

-- 0 ~> []
-- 1 ~> [1]
-- 2 ~> [0,1]
-- 3 ~> [1,1]
-- 4 ~> [0,0,1]
-- 5 ~> [1,0,1]
-- 6 ~> [0,1,1]
-- 7 ~> [1,1,1]
-- 8 ~> [0,1,1,1]
--  ...
--   b0 + b1*2 + b2*4 + b3*8 + b4*16 + b5*32 + ...
-- ~>
--   b0 : b1 : b2 : b3 : b4 : b5 : ...
-- TODO: little-endian or big-endian?
bitsFromInt :: Int -> List Bit
bitsFromInt n =
  case firstBit n of
    Just { bit, rest } -> bit : (bitsFromInt rest)
    Nothing -> Nil

encodeInt :: Bit -> Int
encodeInt =
  case _ of
      B0 -> 0
      B1 -> 1
