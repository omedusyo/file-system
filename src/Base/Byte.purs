module Base.Byte where

import Prelude
import Data.List ( List(..), (:) )
import Data.String as String

import Base.Common ( Tuple8(..) )
import Base.Common as Common
import Base.Bit ( Bit(..) )
import Base.Bit as Bit
import Base.Hex ( Hex )
import Base.Hex as Hex

newtype Byte = Byte (Tuple8 Bit)

fromBits :: Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Byte
fromBits x0 x1 x2 x3 x4 x5 x6 x7 = Byte (Tuple8 x0 x1 x2 x3 x4 x5 x6 x7)

toString :: Byte -> String
toString (Byte (Tuple8 b0 b1 b2 b3 b4 b5 b6 b7)) =
  [b0, b1, b2, b3, b4, b5, b6, b7] # map Bit.toString # String.joinWith ""

instance showByte :: Show Byte where
  show = toString


byteFromHexes :: Hex -> Hex -> Byte
byteFromHexes hex0 hex1 =
  Byte (Common.concat4To4 (Hex.toBits hex0) (Hex.toBits hex1))

-- in hex
-- 00
zeros :: Byte
zeros = Byte (Common.repeat8 B0)

-- in hex
-- FF
ones :: Byte
ones = Byte (Common.repeat8 B1)

encodeInt1 :: Int -> { byte0 :: Byte }
encodeInt1 n =
  { byte0: 
      case Bit.bitsFromInt n of
        Nil -> fromBits B0 B0 B0 B0 B0 B0 B0 B0
        x0 : Nil -> fromBits B0 B0 B0 B0 B0 B0 B0 x0
        x0 : x1 : Nil -> fromBits B0 B0 B0 B0 B0 B0 x1 x0
        x0 : x1 : x2 : Nil -> fromBits B0 B0 B0 B0 B0 x2 x1 x0
        x0 : x1 : x2 : x3 : Nil -> fromBits B0 B0 B0 B0 x3 x2 x1 x0
        x0 : x1 : x2 : x3 : x4 : Nil -> fromBits B0 B0 B0 x4 x3 x2 x1 x0
        x0 : x1 : x2 : x3 : x4 : x5 : Nil -> fromBits B0 B0 x5 x4 x3 x2 x1 x0
        x0 : x1 : x2 : x3 : x4 : x5 : x6 : Nil -> fromBits B0 x6 x5 x4 x3 x2 x1 x0
        x0 : x1 : x2 : x3 : x4 : x5 : x6 : x7 : _ -> fromBits x7 x6 x5 x4 x3 x2 x1 x0
  }

encodeInt2 :: Int -> { byte0 :: Byte, byte1 :: Byte }
encodeInt2 n =
  { byte0: (encodeInt1 (n / 256)).byte0, byte1: (encodeInt1 n).byte0 }


decodeInt :: Byte -> Int
decodeInt (Byte (Tuple8 b7 b6 b5 b4 b3 b2 b1 b0)) =
  Bit.encodeInt b0 + 2*(Bit.encodeInt b1 + 2*(Bit.encodeInt b2 + 2*(Bit.encodeInt b3 + 2*(Bit.encodeInt b4 + 2*(Bit.encodeInt b5 + 2*(Bit.encodeInt b6 + 2*Bit.encodeInt b7))))))
