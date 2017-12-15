module Tokenizer ( tokenize ) where 

import qualified Data.Word as W 
import ParserLib
import qualified Data.Map as Map 
import qualified Tokens as T 



commaToken :: Parser Char T.Token
commaToken = (match ',') >> return T.COMMA   

lparenToken :: Parser Char T.Token 
lparenToken = (match '(')  >> return T.LPAREN

rparenToken :: Parser Char T.Token 
rparenToken = (match ')') >> return T.RPAREN

regToken :: Parser Char T.Token 
regToken = (match 'x') *> (integer >>= (\i -> 
            if i > 0 && i < 32 
            then return $ T.REG i 
            else failure "Registers are labeled from x0 to x31"))

immToken :: Parser Char T.Token 
immToken = (integer >>= (\i -> return $ T.IMM i))

orToken :: Parser Char T.Token 
orToken = (string "or") >> return T.OR

addToken :: Parser Char T.Token 
addToken = (string "add") >> return T.ADD

subToken :: Parser Char T.Token 
subToken = (string "sub") >> return T.SUB 

lbToken :: Parser Char T.Token 
lbToken = (string "lb") >> return T.LB 

lhToken :: Parser Char T.Token 
lhToken = (string "lh") >> return T.LH 

lwToken :: Parser Char T.Token 
lwToken = (string "lw") >> return T.LW 

ldToken :: Parser Char T.Token 
ldToken = (string "ld") >> return T.LD 

sbToken :: Parser Char T.Token 
sbToken = (string "sb") >> return T.SB 

shToken :: Parser Char T.Token 
shToken = (string "sh") >> return T.SH 

swToken :: Parser Char T.Token 
swToken = (string "sw") >> return T.SW 

sdToken :: Parser Char T.Token 
sdToken = (string "sd") >> return T.SD 

labelToken :: Parser Char T.Token 
labelToken = (alphaNumeric <* (string ":")) >>= (\s -> return $ T.LABEL s) 

jalToken :: Parser Char T.Token 
jalToken = (string "jal") >> return T.JAL 

commentToken :: Parser Char T.Token 
commentToken = (match ';') >>
               (many (alphaNumeric <|> whiteSpace)) >> 
               (match '\n') >>
               return T.COMMENT

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
                
tokenize :: String -> Either String [T.Token]
tokenize s = case run (scanAll tokens) s of
    Success x _ -> Right x 
    Failure m   -> Left  m