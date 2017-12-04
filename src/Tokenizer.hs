module Tokenizer 
     ( Token(..)
     , tokenize 
     ) where 

import qualified Data.Word as W 
import ParserLib
import qualified Data.Map as Map 

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


commaToken :: Parser Char Token
commaToken = (match ',') >> return COMMA   

lparenToken :: Parser Char Token 
lparenToken = (match '(')  >> return LPAREN

rparenToken :: Parser Char Token 
rparenToken = (match ')') >> return RPAREN

regToken :: Parser Char Token 
regToken = (match 'x') *> (integer >>= (\i -> 
            if i > 0 && i < 32 
            then return $ REG i 
            else failure "Registers are labeled from x0 to x31"))

immToken :: Parser Char Token 
immToken = (integer >>= (\i -> return $ IMM i))

orToken :: Parser Char Token 
orToken = (string "or") >> return OR

addToken :: Parser Char Token 
addToken = (string "add") >> return ADD

subToken :: Parser Char Token 
subToken = (string "sub") >> return SUB 

lbToken :: Parser Char Token 
lbToken = (string "lb") >> return LB 

lhToken :: Parser Char Token 
lhToken = (string "lh") >> return LH 

lwToken :: Parser Char Token 
lwToken = (string "lw") >> return LW 

ldToken :: Parser Char Token 
ldToken = (string "ld") >> return LD 

sbToken :: Parser Char Token 
sbToken = (string "sb") >> return SB 

shToken :: Parser Char Token 
shToken = (string "sh") >> return SH 

swToken :: Parser Char Token 
swToken = (string "sw") >> return SW 

sdToken :: Parser Char Token 
sdToken = (string "sd") >> return SD 

labelToken :: Parser Char Token 
labelToken = (alphaNumeric <* (string ":")) >>= (\s -> return $ LABEL s) 

jalToken :: Parser Char Token 
jalToken = (string "jal") >> return JAL 

commentToken :: Parser Char Token 
commentToken = (match ';') >>
               (many (alphaNumeric <|> whiteSpace)) >> 
               (match '\n') >>
               return COMMENT

tokens = choice [ commaToken 
                , lparenToken
                , rparenToken
                , orToken 
                , addToken
                , subToken
                , regToken
                , labelToken
                , immToken
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
                
tokenize :: String -> Either String [Token]
tokenize s = case run (scanAll tokens) s of
    Success x _ -> Right x 
    Failure m   -> Left  m