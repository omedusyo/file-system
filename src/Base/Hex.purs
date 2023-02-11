module Hex where

import Bit ( Bit(..) )
import Common ( Tuple4(..) )

data Hex =
    H0
  | H1
  | H2
  | H3
  | H4
  | H5
  | H6
  | H7
  | H8
  | H9
  | HA
  | HB
  | HC
  | HD
  | HE
  | HF

toString :: Hex -> String
toString =
  case _ of
    H0-> "0"
    H1-> "1"
    H2-> "2"
    H3-> "3"
    H4-> "4"
    H5-> "5"
    H6-> "6"
    H7-> "7"
    H8-> "8"
    H9-> "9"
    HA-> "A"
    HB-> "B"
    HC-> "C"
    HD-> "D"
    HE-> "E"
    HF-> "F"

fromBits :: Bit -> Bit -> Bit -> Bit -> Hex
fromBits b0 b1 b2 b3 =
  case b0, b1, b2, b3 of
    B0, B0, B0, B0 -> H0
    B0, B0, B0, B1 -> H1
    B0, B0, B1, B0 -> H2
    B0, B0, B1, B1 -> H3
    B0, B1, B0, B0 -> H4
    B0, B1, B0, B1 -> H5
    B0, B1, B1, B0 -> H6
    B0, B1, B1, B1 -> H7
    B1, B0, B0, B0 -> H8
    B1, B0, B0, B1 -> H9
    B1, B0, B1, B0 -> HA
    B1, B0, B1, B1 -> HB
    B1, B1, B0, B0 -> HC
    B1, B1, B0, B1 -> HD
    B1, B1, B1, B0 -> HE
    B1, B1, B1, B1 -> HF

toBits :: Hex -> Tuple4 Bit
toBits =
  case _ of
    H0 -> Tuple4 B0 B0 B0 B0
    H1 -> Tuple4 B0 B0 B0 B1
    H2 -> Tuple4 B0 B0 B1 B0
    H3 -> Tuple4 B0 B0 B1 B1
    H4 -> Tuple4 B0 B1 B0 B0
    H5 -> Tuple4 B0 B1 B0 B1
    H6 -> Tuple4 B0 B1 B1 B0
    H7 -> Tuple4 B0 B1 B1 B1
    H8 -> Tuple4 B1 B0 B0 B0
    H9 -> Tuple4 B1 B0 B0 B1
    HA -> Tuple4 B1 B0 B1 B0
    HB -> Tuple4 B1 B0 B1 B1
    HC -> Tuple4 B1 B1 B0 B0
    HD -> Tuple4 B1 B1 B0 B1
    HE -> Tuple4 B1 B1 B1 B0
    HF -> Tuple4 B1 B1 B1 B1
