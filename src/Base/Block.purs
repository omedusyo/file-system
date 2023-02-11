module Block where

import Prelude

import Common ( Tuple8(..), Tuple8Index(..) )
import Common as Common
import Byte ( Byte )
import Byte as Byte

-- Blocks are finite size (actually size is a power of 8 i.e. 1, 8, 64, 512, 4096) byte arrays.
-- You can access individual bytes, for not the bits.

-- ======1======
type Block1 = Byte

data Block1Index = B1I -- 1 possible value

update1At :: Block1Index -> (Byte -> Byte) -> Block1 -> Block1
update1At _ f = f

get1At :: Block1Index -> Block1 -> Byte
get1At _ b1 = b1

-- ======8======
type Block8 = Tuple8 Block1 -- 8 bytes

block8 :: Block1 -> Block1 -> Block1 -> Block1 -> Block1 -> Block1 -> Block1 -> Block1 -> Block8
block8 = Tuple8

data Block8Index = B8I (Tuple8Index Block1Index) -- 8 possible addresses

update8At :: Block8Index -> (Byte -> Byte) -> Block8 -> Block8
update8At (B8I i) f =
  Common.update8At i (\j -> update1At j f)

get8At :: Block8Index -> Block8 -> Byte
get8At (B8I i) =
  Common.get8At i get1At

-- ======64======
type Block64 = Tuple8 Block8 -- 64 bytes (64 == 8*8)

byte64 :: Block8 -> Block8 -> Block8 -> Block8 -> Block8 -> Block8 -> Block8 -> Block8 -> Block64
byte64 = Tuple8

data Block64Index = B64I (Tuple8Index Block8Index) -- 64 possible addresses

update64At :: Block64Index -> (Byte -> Byte) -> Block64 -> Block64
update64At (B64I i) f =
  Common.update8At i (\j b8 -> update8At j f b8)

get64At :: Block64Index -> Block64 -> Byte
get64At (B64I i) =
  Common.get8At i get8At

-- ======512======
type Block512 = Tuple8 Block64  -- 512 bytes (512 == 64*8)

block512 :: Block64 -> Block64 -> Block64 -> Block64 -> Block64 -> Block64 -> Block64 -> Block64 -> Block512
block512 = Tuple8

data Block512Index = B512I (Tuple8Index Block64Index) -- 512 possible addresses

update512At :: Block512Index -> (Byte -> Byte) -> Block512 -> Block512
update512At (B512I i) f =
  Common.update8At i (\j b64 -> update64At j f b64)

get512At :: Block512Index -> Block512 -> Byte
get512At (B512I i) =
  Common.get8At i get64At

-- in hex
-- 00
zeroBlock1 :: Block1
zeroBlock1 = Byte.zeros

-- in hex
-- 00 00 00 00
-- 00 00 00 00
zeroBlock8 :: Block8
zeroBlock8 = Common.repeat8 zeroBlock1
                               
-- in hex
-- 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00  
-- 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00  
--
-- 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00  
-- 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00  
zeroBlock64 :: Block64
zeroBlock64 = Common.repeat8 zeroBlock8

-- in hex
-- 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    
-- 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    
--
-- 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    
-- 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    
--
--
-- 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    
-- 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    
--
-- 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    
-- 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    
zeroBlock512 :: Block512
zeroBlock512 = Common.repeat8 zeroBlock64

-- in hex
-- 00 00 00 00  00 00 FF 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    
-- 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    
--
-- 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    
-- 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    
--
--
-- 00 00 00 00  00 00 FF 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    
-- 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    
--
-- 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    
-- 00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    00 00 00 00  00 00 00 00  00 00 00 00  00 00 00 00    
block512example0 :: Block512
block512example0 =
  zeroBlock512
    # update512At (B512I (I0 (B64I (I1 (B8I (I2 B1I)))))) (\_ -> Byte.ones)
    # update512At (B512I (I4 (B64I (I1 (B8I (I2 B1I)))))) (\_ -> Byte.ones)
