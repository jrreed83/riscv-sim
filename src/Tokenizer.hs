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

commaToken :: Parser Token
commaToken = (char ',') >> return COMMA   

lparenToken :: Parser Token 
lparenToken = (char '(') >> return LPAREN

rparenToken :: Parser Token 
rparenToken = (char ')') >> return RPAREN

regToken :: Parser Token 
regToken = (char 'x') >>~ (integer >>= (\i -> return $ REG i))

orToken :: Parser Token 
orToken = (string "or") >> return OR

tokens = choice [commaToken, lparenToken, rparenToken, orToken, regToken]

tokenize :: Parser Token -> Parser [Token]
tokenize p = Parser $ \s -> loop s []
             where loop [] accum = Success accum []
                   loop s  accum = case run p s of 
                                        Success tok rest -> loop rest     (accum ++ [tok]) 
                                        Failure _        -> loop (tail s) accum  
