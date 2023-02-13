module Partition where

import Prelude
import Data.Maybe ( Maybe(..))
import Data.Array as Array

import Base.Byte ( Byte )
import Base.Byte as Byte
import Base.Block ( Block512 )
import Base.Block as Block


type PartitionIndex = Int -- TODO: Better name

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
  -- , size: 258 -- 128 kB partition, 8 bits to encode address of a block
  -- , size: 1024 -- 512 kB (0.5 mB) partition, 10 bits to encode address of a block
  , size: n -- 2048 -- 1 mB partition, 11 bits to encode address of a block
  -- , size: 65536, -- 33 mB partition, 16 bits to encode address of a block
  -- note that if our partition has more than 128 kB, we must have atleast 2 byte addresses
  }

type Region =
  -- Let's try to fit a region into 32 bytes, so we can have 16 regions stored in a 512 byte block.
  --
  -- 2 bytes to store `Maybe PartitionIndex`
  -- So if we have 7 such addresses, that gives us 14 bytes for just where the blocks are at.
  -- This leaves 18 bytes for body.
  --
  -- Also note that the max number of blocks that a Blob can have is 6 + 256 == 262, i.e. max blob size is ~135 kB
  { block0 :: Maybe PartitionIndex -- First bit is to tell whether it is maybe or not
  , block1 :: Maybe PartitionIndex
  , block2 :: Maybe PartitionIndex
  , block3 :: Maybe PartitionIndex
  , block4 :: Maybe PartitionIndex
  , block5 :: Maybe PartitionIndex
  , indirectIndex :: Maybe PartitionIndex -- This can be a pointer to a block that stores a sequence of addresses, so for 512 bytes blocks we can have 256 addresses
  , bodySize :: Int -- number of bytes, in our case 18
  -- Here we can encode stuff like the type of the blob... is it a file? is it a directory? something else maybe?
  -- Or: name, time, permissions, users, groups, capabilities, size, tag/type,
  -- but if 18 bytes is not enough, we can store pointers
  , body :: Array Byte
  }

-- encodePartitionIndex :: Int -> PartitionIndex -> Array Byte
-- encodePartitionIndex sizeInBits index =
--   ?wat

-- encodeRegion :: Region -> Array Byte
-- encodeRegion region =
--   ?wat

-- TODO: How many regions do I need?
