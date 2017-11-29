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

commaToken :: Parser String Token
commaToken = (char ',') >> return COMMA   

lparenToken :: Parser String Token 
lparenToken = (char '(') >> return LPAREN

rparenToken :: Parser String Token 
rparenToken = (char ')') >> return RPAREN

regToken :: Parser String Token 
regToken = (char 'x') *> (integer >>= (\i -> return $ REG i))

orToken :: Parser String Token 
orToken = (string "or") >> return OR

addToken :: Parser String Token 
addToken = (string "add") >> return ADD

subToken :: Parser String Token 
subToken = (string "sub") >> return SUB 

lbToken :: Parser String Token 
lbToken = (string "lb") >> return LB 

lhToken :: Parser String Token 
lhToken = (string "lh") >> return LH 

lwToken :: Parser String Token 
lwToken = (string "lw") >> return LW 

ldToken :: Parser String Token 
ldToken = (string "ld") >> return LD 

sbToken :: Parser String Token 
sbToken = (string "sb") >> return SB 

shToken :: Parser String Token 
shToken = (string "sh") >> return SH 

swToken :: Parser String Token 
swToken = (string "sw") >> return SW 

sdToken :: Parser String Token 
sdToken = (string "sd") >> return SD 

labelToken :: Parser String Token 
labelToken = ((many (anyOf ['a'..'z'])) <* (string ":")) >>= (\s -> return $ LABEL s) 

jalToken :: Parser String Token 
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

tokenize :: Parser String Token -> Parser String [Token]
tokenize p = Parser $ \s -> loop s []
             where loop [] toks = Success toks ""
                   loop s  toks = case run p s of 
                                        Success tok rest -> loop rest     (toks ++ [tok]) 
                                        Failure _        -> loop (tail s) toks  
