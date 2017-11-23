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
data Reg = X { regCode :: W.Word8 } 
         deriving (Show, Eq)

  
-- Assembler temporary
register :: Parser Reg
register = do 
    _ <- string "x" 
    i <- u8 
    if i <= 31 then return $ X i else failure "Not a valid register"

--------------------------------------------------------------------------------

data Rop = ADD | SUB | AND | OR | XOR deriving (Show,Eq)

data Iop = ADDI | LD | STO deriving (Show,Eq)

data Immed = Immed W.Word32 deriving (Show, Eq)

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
