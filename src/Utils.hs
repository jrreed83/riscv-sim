module Utils
    ( trim
    , U32) where

import qualified Data.Word as W

type U8  = W.Word8 
type U16 = W.Word16
type U32 = W.Word32 
type U64 = W.Word64 

trim1 :: U32 -> U32 
trim1 x = x .&. 0x00000001 

trim2 :: U32 -> U32 
trim2 x = x .&. 0x00000003

trim3 :: U32 -> U32
trim3 x = x .&. 0x00000007 

trim4 :: U32 -> U32
trim4 x = x .&. 0x0000000F 

trim5 :: U32 -> U32
trim5 x = x .&. 0x0000001F 

trim6 :: U32 -> U32
trim6 x = x .&. 0x0000003F 

trim7 :: U32 -> U32
trim7  x = x .&. 0x0000007F 

trim8 :: U32 -> U32
trim8  x = x .&. 0x000000FF 

trim9 :: U32 -> U32
trim9  x = x .&. 0x000001FF

trim11 :: U32 -> U32
trim11 x = x .&. 0x000007FF

trim19 :: U32 -> U32
trim19 x = x .&. 0x0007FFFF
