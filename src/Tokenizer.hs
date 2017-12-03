module Tokenizer 
     ( Token(..)
     , tokenize) where 

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
            | COMMENT
            deriving (Show, Eq)

--token' :: Char -> CharParser Char 
--token' x = (detect (==x)) >> return map[x] 

commaToken :: CharParser Token
commaToken = (match ',') >> return COMMA   

lparenToken :: CharParser Token 
lparenToken = (match '(')  >> return LPAREN

rparenToken :: CharParser Token 
rparenToken = (match ')') >> return RPAREN

regToken :: CharParser Token 
regToken = (match 'x') *> (integer >>= (\i -> return $ REG i))

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


commentToken :: CharParser Token 
commentToken = (match ';') >>
               (many (anyOf (['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z'] ++ [' ']))) >> 
               (match '\n') >>
               return COMMENT

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
                , jalToken
                , commentToken]

tokenize :: CharParser [Token]
tokenize = scanAll tokens   
