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

commaToken :: CharParser Token
commaToken = (char ',') >> return COMMA   

lparenToken :: CharParser Token 
lparenToken = (char '(') >> return LPAREN

rparenToken :: CharParser Token 
rparenToken = (char ')') >> return RPAREN

regToken :: CharParser Token 
regToken = (char 'x') *> (integer >>= (\i -> return $ REG i))

orToken :: CharParser Token 
orToken = (string "or") >> return OR

addToken :: CharParser Token 
addToken = (string "add") >> return ADD

subToken :: CharParser Token 
subToken = (string "sub") >> return SUB 

lbToken :: CharParser Token 
lbToken = (string "lb") >> return LB 

lhToken :: CharParser Token 
lhToken = (string "lh") >> return LH 

lwToken :: CharParser Token 
lwToken = (string "lw") >> return LW 

ldToken :: CharParser Token 
ldToken = (string "ld") >> return LD 

sbToken :: CharParser Token 
sbToken = (string "sb") >> return SB 

shToken :: CharParser Token 
shToken = (string "sh") >> return SH 

swToken :: CharParser Token 
swToken = (string "sw") >> return SW 

sdToken :: CharParser Token 
sdToken = (string "sd") >> return SD 

labelToken :: CharParser Token 
labelToken = ((many (anyOf ['a'..'z'])) <* (string ":")) >>= (\s -> return $ LABEL s) 

jalToken :: CharParser Token 
jalToken = (string "jal") >> return JAL 

comments :: CharParser String 
comments = (detect ';') *> 
           (many (anyOf (['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z']))) <* 
           (detect '\n')

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

tokenize :: CharParser Token -> CharParser [Token]
tokenize p = Parser $ \s -> loop s []
             where loop [] toks = Success toks ""
                   loop s  toks = case run p s of 
                                        Success tok rest -> loop rest     (toks ++ [tok]) 
                                        Failure _        -> loop (tail s) toks  
