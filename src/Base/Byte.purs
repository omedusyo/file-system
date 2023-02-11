module Byte where

import Common ( Tuple8(..) )
import Common as Common
import Bit ( Bit(..) )
import Hex ( Hex )
import Hex as Hex

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
