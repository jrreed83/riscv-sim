module Tokenizer 
where 

import qualified Data.Word as W 

data Tokens = COMMA 
            | LPAREN
            | RPAREN 
            | REG W.Word32 
            | IMM W.Word32
            | ADD 
            | SUB 
            | AND 
            | OR 
            | LB
            | LH 
            | LW 
            | LD 
            | SB 
            | SH
            | SW 
            | SD
            deriving (Show, Eq)