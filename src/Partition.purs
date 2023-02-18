module Partition where

import Prelude
import Data.Maybe ( Maybe(..))
import Data.Array as Array

import Base.Bit ( Bit(..) )
import Base.Byte ( Byte )
import Base.Byte as Byte
import Base.Block ( Block512 )
import Base.Block as Block
import Base.Common ( Tuple8(..), Tuple8Index(..) )
import Base.Common as Common


type PartitionIndex = Int

type Partition = 
  { state :: Array Block512
  , size :: Int
  }

initialPartition :: Partition
initialPartition =
  let n = 2048
   in
  { state: Array.replicate n Block.zeroBlock512
  -- , size: 128 -- 65 kB partition, 7 bits to encode address of a block
  -- , size: 256 -- 128 kB partition, 8 bits to encode address of a block
  -- , size: 1024 -- 512 kB (0.5 mB) partition, 10 bits to encode address of a block
  , size: n -- 2048 -- 1 mB partition, 11 bits to encode address of a block
  -- , size: 65536, -- 33 mB partition, 16 bits to encode address of a block
  -- note that if our partition has more than 128 kB, we must have atleast 2 byte addresses
  }

type Region =
  -- Let's try to fit a region into 32 bytes, so we can have 16 regions stored in a 512 byte block.
  --
  -- 2 bytes to store `Maybe PartitionIndex`
  -- So if we have 8 such addresses, that gives us 16 bytes for just where the blocks are at.
  -- This leaves 16 bytes for body.
  --
  -- Also note that the max number of blocks that a Blob can have is 7 + 256 == 263, i.e. max blob size is ~135 kB
  { block0 :: Maybe PartitionIndex -- First bit is to tell whether it is maybe or not
  , block1 :: Maybe PartitionIndex
  , block2 :: Maybe PartitionIndex
  , block3 :: Maybe PartitionIndex
  , block4 :: Maybe PartitionIndex
  , block5 :: Maybe PartitionIndex
  , block6 :: Maybe PartitionIndex
  , indirectIndex :: Maybe PartitionIndex -- This can be a pointer to a block that stores a sequence of addresses, so for 512 bytes blocks we can have 256 addresses
  -- We are left with 16 bytes for the body.
  -- Here we can encode stuff like the type of the blob... is it a file? is it a directory? something else maybe?
  -- Or: name, time, permissions, users, groups, capabilities, size, tag/type,
  -- but if 16 bytes is not enough, we can store pointers.
  , body0 :: Tuple8 Byte
  , body1 :: Tuple8 Byte
  }
-- block0:        flag ? ? ?   ? x10 x9 x8       x7 x6 x5 x4   x3 x2 x1 x0
-- block1:        flag ? ? ?   ? x10 x9 x8       x7 x6 x5 x4   x3 x2 x1 x0
-- block2:        flag ? ? ?   ? x10 x9 x8       x7 x6 x5 x4   x3 x2 x1 x0
-- block3:        flag ? ? ?   ? x10 x9 x8       x7 x6 x5 x4   x3 x2 x1 x0
-- block4:        flag ? ? ?   ? x10 x9 x8       x7 x6 x5 x4   x3 x2 x1 x0
-- block5:        flag ? ? ?   ? x10 x9 x8       x7 x6 x5 x4   x3 x2 x1 x0
-- block6:        flag ? ? ?   ? x10 x9 x8       x7 x6 x5 x4   x3 x2 x1 x0
-- indirectIndex: flag ? ? ?   ? x10 x9 x8       x7 x6 x5 x4   x3 x2 x1 x0

-- body: 16 bytes
--                ?    ? ? ?   ? ?   ?  ?        ?  ?  ?  ?    ?  ?  ?  ?
--                ?    ? ? ?   ? ?   ?  ?        ?  ?  ?  ?    ?  ?  ?  ?
--                ?    ? ? ?   ? ?   ?  ?        ?  ?  ?  ?    ?  ?  ?  ?
--                ?    ? ? ?   ? ?   ?  ?        ?  ?  ?  ?    ?  ?  ?  ?
--
--                ?    ? ? ?   ? ?   ?  ?        ?  ?  ?  ?    ?  ?  ?  ?
--                ?    ? ? ?   ? ?   ?  ?        ?  ?  ?  ?    ?  ?  ?  ?
--                ?    ? ? ?   ? ?   ?  ?        ?  ?  ?  ?    ?  ?  ?  ?
--                ?    ? ? ?   ? ?   ?  ?        ?  ?  ?  ?    ?  ?  ?  ?
--                ?    ? ? ?   ? ?   ?  ?        ?  ?  ?  ?    ?  ?  ?  ?

data EncodedMaybePartitionIndex = EncodedMaybePartitionIndex Byte Byte
instance showEncodedMaybePartitionIndex :: Show EncodedMaybePartitionIndex where
  show (EncodedMaybePartitionIndex byte0 byte1) = show byte0 <> " " <> show byte1


encodePartitionIndex :: Maybe PartitionIndex -> EncodedMaybePartitionIndex
encodePartitionIndex =
  case _ of
    Nothing ->
      -- 0000 000   0000 0000
      EncodedMaybePartitionIndex
        Byte.zeros
        Byte.zeros

    Just index ->
      EncodedMaybePartitionIndex
        (Byte.encodeInt (index / 256) # Byte.updateAt Byte.i0 (\_ -> B1))
        (Byte.encodeInt index)

data EncodedRegion = EncodedRegion (Tuple8 Byte) (Tuple8 Byte) (Tuple8 Byte) (Tuple8 Byte)

instance showEncodedRegion :: Show EncodedRegion where
  show (EncodedRegion bytes0 bytes1 bytes2 bytes3) =
    "(EncodedRegion " <> show bytes0 <> " " <> show bytes1 <> " " <> show bytes2 <> " " <> show bytes3 <> ")"


encodeRegion :: Region -> EncodedRegion
encodeRegion { block0, block1, block2, block3, block4, block5, block6, indirectIndex, body0, body1 } =
  let encodeFourIndexes :: Maybe PartitionIndex -> Maybe PartitionIndex -> Maybe PartitionIndex -> Maybe PartitionIndex -> Tuple8 Byte
      encodeFourIndexes i0 i1 i2 i3 =
        let 
          EncodedMaybePartitionIndex i0byte0 i0byte1 = encodePartitionIndex i0
          EncodedMaybePartitionIndex i1byte0 i1byte1 = encodePartitionIndex i1
          EncodedMaybePartitionIndex i2byte0 i2byte1 = encodePartitionIndex i2
          EncodedMaybePartitionIndex i3byte0 i3byte1 = encodePartitionIndex i3
         in
        Tuple8 i0byte0 i0byte1 i1byte0 i1byte1 i2byte0 i2byte1 i3byte0 i3byte1
   in
  EncodedRegion
    (encodeFourIndexes block0 block1 block2 block3)
    (encodeFourIndexes block4 block5 block6 indirectIndex)
    body0
    body1

decodeRegion :: EncodedRegion -> Region
decodeRegion (EncodedRegion indices0 indices1 body0 body1) =
  let decodeIndex :: Byte -> Byte -> Maybe PartitionIndex
      decodeIndex byte0 byte1 =
        if Byte.getAt Byte.i0 byte0 == B0 then
          Nothing
        else
          -- We need to decode the last 11 bits into an int
          -- Make sure to zero out the first 5 bits
          let byte0first5bitsZeroed =
                byte0
                  # Byte.updateAt Byte.i0 (\_ -> B0)
                  # Byte.updateAt Byte.i1 (\_ -> B0)
                  # Byte.updateAt Byte.i2 (\_ -> B0)
                  # Byte.updateAt Byte.i3 (\_ -> B0)
                  # Byte.updateAt Byte.i4 (\_ -> B0)
           in
          Just $ (256*Byte.decodeInt byte0first5bitsZeroed + Byte.decodeInt byte1)

      decodeIndices :: Tuple8 Byte -> { i0 :: Maybe PartitionIndex, i1 :: Maybe PartitionIndex, i2 :: Maybe PartitionIndex, i3 :: Maybe PartitionIndex }
      decodeIndices (Tuple8 byte0 byte1 byte2 byte3 byte4 byte5 byte6 byte7) =
        { i0: decodeIndex byte0 byte1
        , i1: decodeIndex byte2 byte3
        , i2: decodeIndex byte4 byte5
        , i3: decodeIndex byte6 byte7
        }

      blocks0 = decodeIndices indices0
      blocks1 = decodeIndices indices1
   in
  { block0: blocks0.i0
  , block1: blocks0.i1
  , block2: blocks0.i2
  , block3: blocks0.i3
  , block4: blocks1.i0
  , block5: blocks1.i1
  , block6: blocks1.i2
  , indirectIndex: blocks1.i3
  , body0: body0
  , body1: body1
  }

emptyRegion :: Region
emptyRegion =
  { block0: Nothing
  , block1: Nothing
  , block2: Nothing
  , block3: Nothing
  , block4: Nothing
  , block5: Nothing
  , block6: Nothing
  , indirectIndex: Nothing
  , body0: Common.repeat8 Byte.zeros
  , body1: Common.repeat8 Byte.zeros
  }
