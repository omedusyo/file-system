module Base.Byte where

import Prelude
import Data.List ( List(..), (:) )
import Data.String as String

import Base.Common ( Tuple8(..), Tuple8Index(..) )
import Base.Common as Common
import Base.Bit ( Bit(..) )
import Base.Bit as Bit
import Base.Hex ( Hex )
import Base.Hex as Hex

newtype Byte = Byte (Tuple8 Bit)

derive instance byteEq :: Eq Byte

data Index = Index (Tuple8Index Unit)

i0 :: Index
i0 = Index (I0 unit)

i1 :: Index
i1 = Index (I1 unit)

i2 :: Index
i2 = Index (I2 unit)

i3 :: Index
i3 = Index (I3 unit)

i4 :: Index
i4 = Index (I4 unit)

i5 :: Index
i5 = Index (I5 unit)

i6 :: Index
i6 = Index (I6 unit)

i7 :: Index
i7 = Index (I7 unit)

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

encodeInt :: Int -> Byte
encodeInt n =
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


decodeInt :: Byte -> Int
decodeInt (Byte (Tuple8 b7 b6 b5 b4 b3 b2 b1 b0)) =
  Bit.encodeInt b0 + 2*(Bit.encodeInt b1 + 2*(Bit.encodeInt b2 + 2*(Bit.encodeInt b3 + 2*(Bit.encodeInt b4 + 2*(Bit.encodeInt b5 + 2*(Bit.encodeInt b6 + 2*Bit.encodeInt b7))))))


updateAt :: Index -> (Bit -> Bit) -> Byte -> Byte
updateAt (Index i) f (Byte byte) =
  Byte (Common.update8At i (\_ -> f) byte)

getAt :: Index -> Byte -> Bit
getAt (Index i) (Byte byte) =
  Common.get8At i (\_ b -> b) byte
