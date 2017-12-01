module Instructions
where

import ParserLib
import qualified Data.ByteString as B 
import qualified Data.Word as W
import Data.Bits 
import Data.IORef 
import qualified Data.Vector as V 
import Text.Printf 
import Tokenizer

-- | Vector update
(//) = (V.//)

(!) = (V.!)

(.<<.) :: (Bits a) => a -> Int -> a 
(.<<.) x i = shiftL x i 

(.>>.) :: (Bits a) => a -> Int -> a 
(.>>.) x i = shiftR x i     
----------------------------------------------------------

type U8  = W.Word8 
type U16 = W.Word16
type U32 = W.Word32 
type U64 = W.Word64 

asU32 :: (Integral a) => a -> U32 
asU32 x = fromIntegral x

asInt :: (Integral a) => a -> Int 
asInt x = fromIntegral x 

-----------------------------------------------------------
--  Some additional parsing function 
-----------------------------------------------------------
 

--u32 :: Parser W.Word32 
--u32 = integer >>= (\x -> return $ asU32 x)

-- Assembler temporary
register :: Parser Token Int
register = Parser $ \s ->
    case s of
        (REG x):t -> Success x t 
        _         -> Failure "Nothing"
    

--------------------------------------------------------------------------------


-- pAdd :: Parser Code 
-- pAdd = do
--     _   <- match ADD
--     _   <- spaces 
--     rd  <- register
--     _   <- comma 
--     rs1 <- register 
--     _   <- comma 
--     rs2 <- register 
--     return $ R 0x33 rd 0 rs1 rs2 0 

-- pSubtract :: Parser Code
-- pSubtract = do
--     _   <- string "sub"
--     _   <- spaces 
--     rd  <- register
--     _   <- comma 
--     rs1 <- register 
--     _   <- comma 
--     rs2 <- register 
--     return $ R 0x33 rd 0 rs1 rs2 0x20                                     

-- pStoreByte :: Parser Code 
-- pStoreByte = do
--     _   <- string "sb"
--     _   <- spaces 
--     rs1 <- register 
--     _   <- comma 
--     imm <- u32 
--     _   <- lparen
--     rs2 <- register 
--     _   <- rparen 
--     let imml = trim5 imm 
--     let immu = trim7 (imm .>>. 5) 
--     let op   = 0x23
--     let f3   = 0x00
--     return $ S op imml f3 rs1 rs2 immu    
    
-- pStoreHalfWord :: Parser Code 
-- pStoreHalfWord = do
--     _   <- string "sh"
--     _   <- spaces 
--     rs1 <- register 
--     _   <- comma 
--     imm <- u32 
--     _   <- lparen
--     rs2 <- register 
--     _   <- rparen 
--     let imml = trim5 imm 
--     let immu = trim7 (imm .>>. 5)
--     let op   = 0x23 
--     let f3   = 0x01
--     return $ S op imml f3 rs1 rs2 immu     

-- pStoreWord :: Parser Code 
-- pStoreWord = do
--     _   <- string "sw"
--     _   <- spaces 
--     rs1 <- register 
--     _   <- comma 
--     imm <- u32 
--     _   <- lparen
--     rs2 <- register 
--     _   <- rparen 
--     let imml = trim5 imm 
--     let immu = trim7 (imm .>>. 5)
--     let op   = 0x23 
--     let f3   = 0x02
--     return $ S op imml f3 rs1 rs2 immu  
    
-- pStoreDoubleWord :: Parser Code 
-- pStoreDoubleWord = do
--     _   <- string "sd"
--     _   <- spaces 
--     rs1 <- register 
--     _   <- comma 
--     imm <- u32 
--     _   <- lparen
--     rs2 <- register 
--     _   <- rparen 
--     let imml = trim5 imm 
--     let immu = trim7 (imm .>>. 5)
--     let op   = 0x23 
--     let f3   = 0x03
--     return $ S op imml f3 rs1 rs2 immu    

-- pLoadByte :: Parser Code
-- pLoadByte = do
--     _   <- string "lb"
--     _   <- spaces 
--     rd  <- register 
--     _   <- comma 
--     imm <- u32 
--     _   <- lparen
--     rs1 <- register 
--     _   <- rparen 
--     let op   = 0x03 
--     let f3   = 0x00
--     return $ I op rd f3 rs1 imm       

-- pLoadHalfWord :: Parser Code
-- pLoadHalfWord = do
--     _   <- string "lh"
--     _   <- spaces 
--     rd  <- register 
--     _   <- comma 
--     imm <- u32 
--     _   <- lparen
--     rs1 <- register 
--     _   <- rparen 
--     let op   = 0x03 
--     let f3   = 0x01
--     return $ I op rd f3 rs1 imm 
    
-- pLoadWord :: Parser Code
-- pLoadWord = do
--     _   <- string "lw"
--     _   <- spaces 
--     rd  <- register 
--     _   <- comma 
--     imm <- u32 
--     _   <- lparen
--     rs1 <- register 
--     _   <- rparen 
--     let op   = 0x03 
--     let f3   = 0x02
--     return $ I op rd f3 rs1 imm  
    
-- pLoadDoubleWord :: Parser Code
-- pLoadDoubleWord = do
--     _   <- string "ld"
--     _   <- spaces 
--     rd  <- register 
--     _   <- comma 
--     imm <- u32 
--     _   <- lparen
--     rs1 <- register 
--     _   <- rparen 
--     let op   = 0x03 
--     let f3   = 0x03
--     return $ I op rd f3 rs1 imm      
-- ----------------------------------------------------------------------------------

-- data Code = R  { op  :: !U32
--                , rd  :: !U32
--                , f3  :: !U32
--                , rs1 :: !U32
--                , rs2 :: !U32
--                , f7  :: !U32 }
--           | I  { op  :: !U32
--                , rd  :: !U32
--                , f3  :: !U32
--                , rs1 :: !U32
--                , imm :: !U32 }
--           | S  { op   :: !U32
--                , imml :: !U32
--                , f3   :: !U32
--                , rs1  :: !U32
--                , rs2  :: !U32
--                , immu :: !U32 } 
--           | SB { op   :: !U32
--                , imml :: !U32
--                , f3   :: !U32
--                , rs1  :: !U32
--                , rs2  :: !U32
--                , immu :: !U32 } 
--           | UJ { op  :: !U32 
--                , rd  :: !U32 
--                , imm :: !U32 }
--           deriving (Show, Eq)


-- data CodeType = RCode | SCode | ICode | SBCode | UJCode
--                 deriving (Show,Eq)

-- codeType :: U32 -> CodeType 
-- codeType 0x33 = RCode
-- codeType 0x3B = RCode 
-- codeType 0x03 = ICode 
-- codeType 0x0F = ICode
-- codeType 0x67 = ICode
-- codeType 0x73 = ICode
-- codeType 0x23 = SCode
-- codeType 0x63 = SBCode

-- encode :: Code -> U32 
-- encode (R op rd f3 rs1 rs2 f7) = 
--      (op  .<<. 0  ) .|. 
--      (rd  .<<. 7  ) .|.
--      (f3  .<<. 12 ) .|. 
--      (rs1 .<<. 15 ) .|.
--      (rs2 .<<. 20 ) .|.
--      (f7  .<<. 25 ) 

-- encode (I op rd f3 rs imm) = 
--      (op  .<<. 0 ) .|. 
--      (rd  .<<. 7 ) .|.
--      (f3  .<<. 12) .|.
--      (rs  .<<. 15) .|. 
--      (imm .<<. 20)

-- encode (S op imml f3 rs1 rs2 immu) = 
--      (op   .<<. 0 ) .|. 
--      (imml .<<. 7 ) .|.
--      (f3   .<<. 12) .|.
--      (rs1  .<<. 15) .|. 
--      (rs2  .<<. 20) .|. 
--      (immu .<<. 25)

-- encode (UJ op rd imm) = 
--     (op  .<<. 0) .|.
--     (rd  .<<. 7) .|.
--     (imm .<<. 12)

-- decode :: U32 -> Code 
-- decode x = case codeType opCode of 
--                 RCode -> decodeR x
--                 ICode -> decodeI x 
--                 SCode -> decodeS x
--            where opCode = trim7 (x .>>. 0)  


-- trim1 :: U32 -> U32 
-- trim1 x = x .&. 0x00000001 

-- trim2 :: U32 -> U32 
-- trim2 x = x .&. 0x00000003

-- trim3 :: U32 -> U32
-- trim3 x = x .&. 0x00000007 

-- trim4 :: U32 -> U32
-- trim4 x = x .&. 0x0000000F 

-- trim5 :: U32 -> U32
-- trim5 x = x .&. 0x0000001F 

-- trim6 :: U32 -> U32
-- trim6 x = x .&. 0x0000003F 

-- trim7 :: U32 -> U32
-- trim7  x = x .&. 0x0000007F 

-- trim8 :: U32 -> U32
-- trim8  x = x .&. 0x000000FF 

-- trim9 :: U32 -> U32
-- trim9  x = x .&. 0x000001FF

-- trim11 :: U32 -> U32
-- trim11 x = x .&. 0x000007FF

-- trim19 :: U32 -> U32
-- trim19 x = x .&. 0x0007FFFF

-- decodeR :: U32 -> Code 
-- decodeR x = R op rd f3 rs1 rs2 f7
--             where op  = trim7 (x .>>. 0 )
--                   rd  = trim5 (x .>>. 7 )
--                   f3  = trim3 (x .>>. 12)
--                   rs1 = trim5 (x .>>. 15)
--                   rs2 = trim5 (x .>>. 20)
--                   f7  = trim6 (x .>>. 25)

-- decodeI :: U32 -> Code 
-- decodeI x = I op rd f3 rs1 imm 
--             where op  = trim7  (x .>>. 0 )
--                   rd  = trim5  (x .>>. 7 )
--                   f3  = trim3  (x .>>. 12)
--                   rs1 = trim5  (x .>>. 15)
--                   imm = trim11 (x .>>. 20)

-- decodeS :: U32 -> Code
-- decodeS x = S op imml f3 rs1 rs2 immu
--             where op   = trim7 (x .>>. 0 )
--                   imml = trim5 (x .>>. 7 )
--                   f3   = trim3 (x .>>. 12)
--                   rs1  = trim5 (x .>>. 15)
--                   rs2  = trim5 (x .>>. 20)
--                   immu = trim6 (x .>>. 25)


-- bitPattern :: U32 -> String 
-- bitPattern x =
--     show ((x .>>. 31) .&. 1) ++  
--     show ((x .>>. 30) .&. 1) ++  
--     show ((x .>>. 29) .&. 1) ++  
--     show ((x .>>. 28) .&. 1) ++    
--     show ((x .>>. 27) .&. 1) ++  
--     show ((x .>>. 26) .&. 1) ++  
--     show ((x .>>. 25) .&. 1) ++  
--     show ((x .>>. 24) .&. 1) ++    
--     show ((x .>>. 23) .&. 1) ++  
--     show ((x .>>. 22) .&. 1) ++  
--     show ((x .>>. 21) .&. 1) ++  
--     show ((x .>>. 20) .&. 1) ++    
--     show ((x .>>. 19) .&. 1) ++  
--     show ((x .>>. 18) .&. 1) ++  
--     show ((x .>>. 17) .&. 1) ++  
--     show ((x .>>. 16) .&. 1) ++  
--     show ((x .>>. 15) .&. 1) ++  
--     show ((x .>>. 14) .&. 1) ++  
--     show ((x .>>. 13) .&. 1) ++  
--     show ((x .>>. 12) .&. 1) ++    
--     show ((x .>>. 11) .&. 1) ++  
--     show ((x .>>. 10) .&. 1) ++  
--     show ((x .>>. 9 ) .&. 1) ++  
--     show ((x .>>. 8 ) .&. 1) ++    
--     show ((x .>>. 7 ) .&. 1) ++  
--     show ((x .>>. 6 ) .&. 1) ++  
--     show ((x .>>. 5 ) .&. 1) ++  
--     show ((x .>>. 4 ) .&. 1) ++    
--     show ((x .>>. 3 ) .&. 1) ++  
--     show ((x .>>. 2 ) .&. 1) ++  
--     show ((x .>>. 1 ) .&. 1) ++  
--     show ((x .>>. 0 ) .&. 1)             


-- -- data State = State { registers :: V.Vector U32 
-- --                    , ip        :: U32
-- --                    } deriving (Show)

-- -- initState :: State 
-- -- initState = State r 0
-- --             where r = V.replicate 32 (0::U32)

-- -- add :: Instruction -> State -> State 
-- -- add (R _ rd _ rs rt _ ) state =  
-- --     let registers' = registers state
-- --         id         = asInt rd 
-- --         is         = asInt rs 
-- --         it         = asInt rt  
-- --         xs         = registers' ! is 
-- --         xt         = registers' ! it  
-- --         xd         = xs + xt
-- --         ip'        = (ip state) + 1        
-- --     in  State  (registers' // [(id,xd)]) ip'

--     -- type Memory = V.Vector W.Word8 
    
--     -- initMemory :: W.Word32 -> Memory 
--     -- initMemory n = V.fromList [1,1,2,255]--V.replicate (asInt n) (0::W.Word8)

--     -- memoryLookup :: (Integral a) => a -> Memory -> W.Word8 
--     -- memoryLookup i mem = mem V.! (asInt i)

--     -- data CPU = CPU { ip     :: IORef W.Word32
--     --                , sp     :: IORef W.Word32 
--     --                , memory :: Memory}

--     -- initCPU :: IO CPU 
--     -- initCPU =  (newIORef 0) >>= (\ip -> 
--     --            (newIORef 0) >>= (\sp -> 
--     --            (return $ CPU ip sp (initMemory 256))))

--     -- nop :: CPU -> IO () 
--     -- nop cpu = return ()

--     -- stop :: CPU -> IO ()
--     -- stop cpu = return ()

--     -- nextInst :: CPU -> IO W.Word8
--     -- nextInst cpu = let ip'  = ip cpu
--     --                    mem' = memory cpu
--     --                in  do { i <- readIORef ip' 
--     --                       ; modifyIORef ip' (+ 1)
--     --                       ; return $ memoryLookup i mem'}

--     -- execute :: IO () 
--     -- execute = initCPU >>= ( \cpu -> execute' cpu)

-- --    step :: CPU -> IO ()
-- --    step cpu = do
-- --        inst <- nextInst cpu
-- --        let fn = lookup inst
-- --        a <- fn cpu

--     -- execute' :: CPU -> IO ()
--     -- execute' cpu = do  
--     --     x <- nextInst cpu 
--     --     
--     --     case x of 
--     --         255 -> (stop cpu) >>= (\_ -> (putStrLn "STOP") >>= (\_ -> return ()))
--     --         _   -> do 
--     --             nop cpu
--     --             putStrLn "NOP"
--     --             execute' cpu     
    

               

-- --    foo :: IO () 
-- --    foo = do { cpu <- initCPU 
-- --             ; x   <- eval (Exp3 ADD (Reg 4) (Reg 5) (Reg 6)) cpu 
-- --             ; print x }

-- --    loop :: CPU -> IO () 
-- --    loop cpu = do   
-- --                 putStr ">>"
-- --                 l <- getLine -- read 
--                  -- Want to check whether it's an expression
--                  -- or some sort of debug command
-- --                 case eval l cpu of 
-- --                    Right cpu' -> loop cpu'
-- --                    Left  msg  -> loop cpu  
    
       

-- --    main :: IO () 
-- --    main = do loop initCPU 
