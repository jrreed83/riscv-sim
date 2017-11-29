module Tokenizer 
where 

import qualified Data.Word as W 
import ParserLib

data Token  = COMMA 
            | LPAREN
            | RPAREN 
            | REG Int
            | IMM Int
            | LABEL String
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
            | JAL
            deriving (Show, Eq)

commaToken :: Parser Token
commaToken = (char ',') >> return COMMA   

lparenToken :: Parser Token 
lparenToken = (char '(') >> return LPAREN

rparenToken :: Parser Token 
rparenToken = (char ')') >> return RPAREN

regToken :: Parser Token 
regToken = (char 'x') *> (integer >>= (\i -> return $ REG i))

orToken :: Parser Token 
orToken = (string "or") >> return OR

addToken :: Parser Token 
addToken = (string "add") >> return ADD

subToken :: Parser Token 
subToken = (string "sub") >> return SUB 

lbToken :: Parser Token 
lbToken = (string "lb") >> return LB 

lhToken :: Parser Token 
lhToken = (string "lh") >> return LH 

lwToken :: Parser Token 
lwToken = (string "lw") >> return LW 

ldToken :: Parser Token 
ldToken = (string "ld") >> return LD 

sbToken :: Parser Token 
sbToken = (string "sb") >> return SB 

shToken :: Parser Token 
shToken = (string "sh") >> return SH 

swToken :: Parser Token 
swToken = (string "sw") >> return SW 

sdToken :: Parser Token 
sdToken = (string "sd") >> return SD 

labelToken :: Parser Token 
labelToken = ((many (anyOf ['a'..'z'])) <* (string ":")) >>= (\s -> return $ LABEL s) 

jalToken :: Parser Token 
jalToken = (string "jal") >> return JAL 

tokens = choice [ commaToken
                , lparenToken
                , rparenToken
                , orToken
                , addToken
                , regToken
                , labelToken
                , lbToken 
                , lhToken
                , lwToken 
                , ldToken
                , sbToken 
                , shToken
                , swToken 
                , sdToken
                , jalToken]

tokenize :: Parser Token -> Parser [Token]
tokenize p = Parser $ \s -> loop s []
             where loop [] toks = Success toks ""
                   loop s  toks = case run p s of 
                                        Success tok rest -> loop rest     (toks ++ [tok]) 
                                        Failure _        -> loop (tail s) toks  
