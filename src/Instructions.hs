module Instructions
where

import Parser
import qualified Data.ByteString as B 
import qualified Data.Word as W
import Data.Bits 
import Data.IORef 
import qualified Data.Vector as V 

(|<<|) :: (Bits a) => a -> Int -> a 
(|<<|) x i = shiftL x i 

(|>>|) :: (Bits a) => a -> Int -> a 
(|>>|) x i = shiftR x i     
----------------------------------------------------------

type U32 = W.Word32 

asU32 :: (Integral a) => a -> U32 
asU32 x = fromIntegral x

asInt :: (Integral a) => a -> Int 
asInt x = fromIntegral x 

-----------------------------------------------------------
--  Some additional parsing function 
-----------------------------------------------------------
eol :: Parser String 
eol = (string "\n") <|> spaces

comma :: Parser Char 
comma = char ',' 

semicolon :: Parser Char 
semicolon = char ';' 


lparen :: Parser Char 
lparen = char '(' 

rparen :: Parser Char 
rparen = char ')'

comments :: Parser String 
comments = semicolon >> anyString


u32 :: Parser W.Word32 
u32 = integer >>= (\x -> return $ asU32 x)

-- Register Stuff
data Reg = X { ptr :: W.Word32 } 
         deriving (Show, Eq)

  
-- Assembler temporary
register :: Parser U32
register = do 
    _ <- string "x" 
    i <- u32 
    if i <= 31 then return i else failure "Not a valid register"

--------------------------------------------------------------------------------

-- data Rop = ADD | SUB | AND | OR | XOR 
--          deriving (Show,Eq)

-- data Iop = ADDI | LD | STO 
--          deriving (Show,Eq)

-- data Op  = Rop Rop 
--          | Iop Iop 
--          deriving (Show, Eq)

-- data Immed = Immed W.Word32 
--            deriving (Show, Eq)


--     -- Instruction Set 
-- data Instruction = RType Rop Reg Reg Reg 
--                  | IType Iop Reg W.Word32 
--                  | NOP 
--                  deriving (Show, Eq)

add :: Parser Encoding 
add = do
    _  <- string "add"
    _  <- spaces 
    rd <- register
    _  <- comma 
    rt <- register 
    _  <- comma 
    rs <- register 
    return $ REncoding 33 rd 0 rt rs 0 

sub :: Parser Encoding 
sub = do
    _  <- string "sub"
    _  <- spaces 
    rd <- register
    _  <- comma 
    rt <- register 
    _  <- comma 
    rs <- register 
    return $ REncoding 33 rd 0 rt rs 20                                     
----------------------------------------------------------------------------------

data Encoding = REncoding { op :: !U32, rd :: !U32, f3 :: !U32, rs :: !U32, rt  :: !U32, f7 :: !U32 } 
              | IEncoding { op :: !U32, rd :: !U32, f3 :: !U32, rs :: !U32, i11 :: !U32             }
              deriving Show
 
serialize :: Encoding -> U32 
serialize (REncoding op rd f3 rs rt f7)  = 
          (op |<<| 0  ) .|. 
          (rd |<<| 7  ) .|.
          (f3 |<<| 12 ) .|. 
          (rs |<<| 15 ) .|.
          (rt |<<| 20 ) .|.
          (f7 |<<| 25 ) 
serialize (IEncoding op rd f3 rs i11) = 
          (op  |<<| 0 ) .|. 
          (rd  |<<| 7 ) .|.
          (f3  |<<| 12) .|.
          (rs  |<<| 15) .|. 
          (i11 |<<| 20)

showBitPattern :: U32 -> String 
showBitPattern x =
    show ((x |>>| 31) .&. 1) ++  
    show ((x |>>| 30) .&. 1) ++  
    show ((x |>>| 29) .&. 1) ++  
    show ((x |>>| 28) .&. 1) ++    
    show ((x |>>| 27) .&. 1) ++  
    show ((x |>>| 26) .&. 1) ++  
    show ((x |>>| 25) .&. 1) ++  
    show ((x |>>| 24) .&. 1) ++    
    show ((x |>>| 23) .&. 1) ++  
    show ((x |>>| 22) .&. 1) ++  
    show ((x |>>| 21) .&. 1) ++  
    show ((x |>>| 20) .&. 1) ++    
    show ((x |>>| 19) .&. 1) ++  
    show ((x |>>| 18) .&. 1) ++  
    show ((x |>>| 17) .&. 1) ++  
    show ((x |>>| 16) .&. 1) ++  
    show ((x |>>| 15) .&. 1) ++  
    show ((x |>>| 14) .&. 1) ++  
    show ((x |>>| 13) .&. 1) ++  
    show ((x |>>| 12) .&. 1) ++    
    show ((x |>>| 11) .&. 1) ++  
    show ((x |>>| 10) .&. 1) ++  
    show ((x |>>| 9 ) .&. 1) ++  
    show ((x |>>| 8 ) .&. 1) ++    
    show ((x |>>| 7 ) .&. 1) ++  
    show ((x |>>| 6 ) .&. 1) ++  
    show ((x |>>| 5 ) .&. 1) ++  
    show ((x |>>| 4 ) .&. 1) ++    
    show ((x |>>| 3 ) .&. 1) ++  
    show ((x |>>| 2 ) .&. 1) ++  
    show ((x |>>| 1 ) .&. 1) ++  
    show ((x |>>| 0 ) .&. 1)             
 


    -- type Memory = V.Vector W.Word8 
    
    -- initMemory :: W.Word32 -> Memory 
    -- initMemory n = V.fromList [1,1,2,255]--V.replicate (asInt n) (0::W.Word8)

    -- memoryLookup :: (Integral a) => a -> Memory -> W.Word8 
    -- memoryLookup i mem = mem V.! (asInt i)

    -- data CPU = CPU { ip     :: IORef W.Word32
    --                , sp     :: IORef W.Word32 
    --                , memory :: Memory}

    -- initCPU :: IO CPU 
    -- initCPU =  (newIORef 0) >>= (\ip -> 
    --            (newIORef 0) >>= (\sp -> 
    --            (return $ CPU ip sp (initMemory 256))))

    -- nop :: CPU -> IO () 
    -- nop cpu = return ()

    -- stop :: CPU -> IO ()
    -- stop cpu = return ()

    -- nextInst :: CPU -> IO W.Word8
    -- nextInst cpu = let ip'  = ip cpu
    --                    mem' = memory cpu
    --                in  do { i <- readIORef ip' 
    --                       ; modifyIORef ip' (+ 1)
    --                       ; return $ memoryLookup i mem'}

    -- execute :: IO () 
    -- execute = initCPU >>= ( \cpu -> execute' cpu)

--    step :: CPU -> IO ()
--    step cpu = do
--        inst <- nextInst cpu
--        let fn = lookup inst
--        a <- fn cpu

    -- execute' :: CPU -> IO ()
    -- execute' cpu = do  
    --     x <- nextInst cpu 
    --     
    --     case x of 
    --         255 -> (stop cpu) >>= (\_ -> (putStrLn "STOP") >>= (\_ -> return ()))
    --         _   -> do 
    --             nop cpu
    --             putStrLn "NOP"
    --             execute' cpu     
    

               

--    foo :: IO () 
--    foo = do { cpu <- initCPU 
--             ; x   <- eval (Exp3 ADD (Reg 4) (Reg 5) (Reg 6)) cpu 
--             ; print x }

--    loop :: CPU -> IO () 
--    loop cpu = do   
--                 putStr ">>"
--                 l <- getLine -- read 
                 -- Want to check whether it's an expression
                 -- or some sort of debug command
--                 case eval l cpu of 
--                    Right cpu' -> loop cpu'
--                    Left  msg  -> loop cpu  
    
       

--    main :: IO () 
--    main = do loop initCPU 
