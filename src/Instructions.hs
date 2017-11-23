module Instructions
where

import Parser
import qualified Data.ByteString as B 
import qualified Data.Word as W
import Data.Bits 
import Data.IORef 
import qualified Data.Vector as V 

(.<<.) :: (Bits a) => a -> Int -> a 
(.<<.) x i = shiftL x i 

(.>>.) :: (Bits a) => a -> Int -> a 
(.>>.) x i = shiftR x i     
----------------------------------------------------------

asWord16 :: (Integral a) => a -> W.Word16 
asWord16 x = fromIntegral x

asWord8 :: (Integral a) => a -> W.Word8 
asWord8 x = fromIntegral x

asWord32 :: (Integral a) => a -> W.Word32 
asWord32 x = fromIntegral x

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

u8 :: Parser W.Word8 
u8 = integer >>= (\x -> return $ asWord8 x)

u16 :: Parser W.Word16 
u16 = integer >>= (\x -> return $ asWord16 x)

u32 :: Parser W.Word32 
u32 = integer >>= (\x -> return $ asWord32 x)

-- Register Stuff
data Reg = ZERO 
             | AT 
             | V0 | V1 
             | A0 | A1 | A2 | A3 
             | T0 | T1 | T2 | T3 | T4 | T5 | T6 | T7
             | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 
             | T8 | T9 
             | K0 | K1 
             | GP 
             | SP
             | FP 
             | RP 
             deriving (Show, Eq)

-- Constant value 0
zero :: Parser Reg 
zero = (string "$zero") >>= (\_ -> return ZERO)
  
-- Assembler temporary
at :: Parser Reg
at = (string "$at") >>= (\_ -> return AT)

-- 
v0 :: Parser Reg 
v0 = (string "$v0") >>= (\_ -> return V0)

v1 :: Parser Reg 
v1 = (string "$v1") >>= (\_ -> return V1)

a0 :: Parser Reg 
a0 = (string "$a0") >>= (\_ -> return A0)

a1 :: Parser Reg 
a1 = (string "$a1") >>= (\_ -> return A1)

a2 :: Parser Reg 
a2 = (string "$a2") >>= (\_ -> return A2)

a3 :: Parser Reg 
a3 = (string "$a3") >>= (\_ -> return A3)

t0 :: Parser Reg 
t0 = (string "$t0") >>= (\_ -> return T0)

t1 :: Parser Reg 
t1 = (string "$t1") >>= (\_ -> return T1)

t2 :: Parser Reg 
t2 = (string "$t2") >>= (\_ -> return T2)

t3 :: Parser Reg 
t3 = (string "$t3") >>= (\_ -> return T3)   
    
t4 :: Parser Reg 
t4 = (string "$t4") >>= (\_ -> return T4)

t5 :: Parser Reg 
t5 = (string "$t5") >>= (\_ -> return T5)

t6 :: Parser Reg 
t6 = (string "$t6") >>= (\_ -> return T6)

t7 :: Parser Reg 
t7 = (string "$t7") >>= (\_ -> return T7)      

s0 :: Parser Reg 
s0 = (string "$s0") >>= (\_ -> return S0)

s1 :: Parser Reg 
s1 = (string "$s1") >>= (\_ -> return S1)

s2 :: Parser Reg 
s2 = (string "$s2") >>= (\_ -> return S2)

s3 :: Parser Reg 
s3 = (string "$s3") >>= (\_ -> return S3)   
    
s4 :: Parser Reg 
s4 = (string "$s4") >>= (\_ -> return S4)

s5 :: Parser Reg 
s5 = (string "$s5") >>= (\_ -> return S5)

s6 :: Parser Reg 
s6 = (string "$s6") >>= (\_ -> return S6)

s7 :: Parser Reg 
s7 = (string "$s7") >>= (\_ -> return S7)  

t8 :: Parser Reg 
t8 = (string "$t8") >>= (\_ -> return T8)

t9 :: Parser Reg 
t9 = (string "$t9") >>= (\_ -> return T9)

register :: Parser Reg
register = 
        zero <|>
        at <|>
        v0 <|> v1 <|>
        a0 <|> a1 <|> a2 <|> a3 <|>
        t0 <|> t1 <|> t2 <|> t3 <|> t4 <|> t5 <|> t6 <|> t7 <|>
        s0 <|> s1 <|> s2 <|> s3 <|> s4 <|> s5 <|> s6 <|> s7 <|>
        t8 <|> t9
--------------------------------------------------------------------------------

data Rop = ADD | SUB | AND | OR | XOR
             deriving (Show,Eq)

data Iop = ADDI | LD | STO 
             deriving (Show,Eq)

data Immed = Immed W.Word32 
               deriving (Show, Eq)

data Shft = Shft W.Word8 
               deriving (Show, Eq)

    -- Instruction Set 
data Instruction = RType Rop Reg Reg Reg W.Word8
                     | IType Iop Reg Reg     W.Word32
                     | NOP 
                     deriving (Show, Eq)

add :: Parser Instruction 
add = do
        _  <- string "add"
        _  <- spaces 
        rd <- register
        _  <- comma 
        rt <- register 
        _  <- comma 
        rs <- register 
        return $ RType ADD rd rt rs 0

                                    
    ----------------------------------------------------------------------------------

regCode :: Reg -> W.Word8
regCode ZERO = 0
regCode AT   = 1 
regCode V0   = 2
regCode V1   = 3 
regCode A0   = 4
regCode A1   = 5 
regCode A3   = 6
regCode T0   = 7 
regCode T1   = 8 
regCode T2   = 9 
regCode T3   = 10 
regCode T4   = 11 
regCode T5   = 12 
regCode T6   = 13 
regCode T7   = 14
regCode S0   = 15 
regCode S1   = 16 
regCode S2   = 17 
regCode S3   = 18 
regCode S4   = 19 
regCode S5   = 20 
regCode S6   = 21 
regCode S7   = 22    
    
funCode :: Rop -> W.Word8 
funCode _ = 0
    
encode :: Instruction -> W.Word32 
encode NOP                        = 0
encode (RType rop rd rs rt shft)  = 
        (f .<<. 0  ) .|. 
        (i .<<. 6  ) .|.
        (t .<<. 11 ) .|. 
        (s .<<. 16 ) .|.
        (d .<<. 21 ) .|.
        (o .<<. 26 )
        where f = asWord32 $ funCode rop 
              d = asWord32 $ regCode rd 
              s = asWord32 $ regCode rs  
              t = asWord32 $ regCode rt
              i = asWord32 $ shft 
              o = 0
    -- pAddressMode = pAddrPreDec <|> pAddrPreInc <|> pAddrPostInc <|> pAddrPostDec <|> pAddrI <|> pAddrD <|> pData <|> pImmed
    
    -- data OpMode = Byte | Word | Long deriving (Show)

    -- encOpMode :: OpMode -> W.Word8 
    -- encOpMode Byte = 0 
    -- encOpMode Word = 1 
    -- encOpMode Long = 2 

    -- data Instruction = MOVE OpMode AddressMode AddressMode
    --                  | ADD  OpMode AddressMode AddressMode
    --                  | NOP
    --                  | STOP
    --                  deriving (Show)
 






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
