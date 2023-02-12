module Base.Byte where

import Base.Common ( Tuple8(..) )
import Base.Common as Common
import Base.Bit ( Bit(..) )
import Base.Hex ( Hex )
import Base.Hex as Hex

type Byte = Tuple8 Bit

byteFromBits :: Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Byte
byteFromBits = Tuple8

byteFromHexes :: Hex -> Hex -> Byte
byteFromHexes hex0 hex1 =
  Common.concat4To4 (Hex.toBits hex0) (Hex.toBits hex1)

-- in hex
-- 00
zeros :: Byte
zeros = Common.repeat8 B0

-- in hex
-- FF
ones :: Byte
ones = Common.repeat8 B1
