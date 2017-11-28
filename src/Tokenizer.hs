module Tokenizer 
where 

import qualified Data.Word as W 
import ParserLib

data Token  = COMMA 
            | LPAREN
            | RPAREN 
            | REG Int
            | IMM Int
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

comma :: Parser Token
comma = (char ',') >> return COMMA   

lparen :: Parser Token 
lparen = (char '(') >> return LPAREN

rparen :: Parser Token 
rparen = (char ')') >> return RPAREN

reg :: Parser Token 
reg = (char 'x') >>~ (integer >>= (\i -> return $ REG i))

--or :: Parser Token 
--or = (string "or") >> return OR

tokens = choice [comma, lparen, rparen]
