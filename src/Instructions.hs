module Instructions
where

import ParserLib
import Tokenizer
import qualified Data.ByteString as B 
import Data.Bits 
import Data.IORef 
import qualified Data.Vector as V 
import Text.Printf 
import Numeric
import qualified Data.Map as Map
import qualified Data.Word as W

type U8  = W.Word8 
type U16 = W.Word16
type U32 = W.Word32 
type U64 = W.Word64 

trim1 :: U32 -> U32 
trim1 x = x .&. 0x00000001 

trim2 :: U32 -> U32 
trim2 x = x .&. 0x00000003

trim3 :: U32 -> U32
trim3 x = x .&. 0x00000007 

trim4 :: U32 -> U32
trim4 x = x .&. 0x0000000F 

trim5 :: U32 -> U32
trim5 x = x .&. 0x0000001F 

trim6 :: U32 -> U32
trim6 x = x .&. 0x0000003F 

trim7 :: U32 -> U32
trim7  x = x .&. 0x0000007F 

trim8 :: U32 -> U32
trim8  x = x .&. 0x000000FF 

trim9 :: U32 -> U32
trim9  x = x .&. 0x000001FF

trim11 :: U32 -> U32
trim11 x = x .&. 0x000007FF

trim19 :: U32 -> U32
trim19 x = x .&. 0x0007FFFF

(.<<.) :: (Bits a) => a -> Int -> a 
(.<<.) x i = shiftL x i 

(.>>.) :: (Bits a) => a -> Int -> a 
(.>>.) x i = shiftR x i     
----------------------------------------------------------



asU32 :: (Integral a) => a -> U32 
asU32 x = fromIntegral x

asInt :: (Integral a) => a -> Int 
asInt x = fromIntegral x 

-----------------------------------------------------------
--  Some additional parsing function 
-----------------------------------------------------------
 

u32 :: Parser Char U32 
u32 = integer >>= (\x -> return $ asU32 x)

-- Assembler temporary
register :: Parser Token U32
register = Parser $ \s ->
    case s of
        (REG x):t -> Success (asU32 x) t 
        _         -> Failure "Nothing"
    

immediate :: Parser Token U32 
immediate = Parser $ \s ->
    case s of
        (IMM x):t -> Success (asU32 x) t 
        _         -> Failure "Nothing"
--------------------------------------------------------------------------------

parseAdd :: Parser Token ByteCode
parseAdd = do
     _   <- match ADD
     rd  <- register
     _   <- match COMMA
     rs1 <- register 
     _   <- match COMMA 
     rs2 <- register 
     let op = 0x33
     let f3 = 0
     let f7 = 0
     return $ encodeR op rd f3 rs1 rs2 f7

encodeR :: U32  -- op
        -> U32  -- rd
        -> U32  -- f3
        -> U32  -- rs1 
        -> U32  -- rs2
        -> U32  -- f7
        -> ByteCode 
encodeR op rd f3 rs1 rs2 f7 = ByteCode $
   (op  .<<. 0  ) .|. 
   (rd  .<<. 7  ) .|.
   (f3  .<<. 12 ) .|. 
   (rs1 .<<. 15 ) .|.
   (rs2 .<<. 20 ) .|.
   (f7  .<<. 25 ) 

subtraction :: Parser Token ByteCode
subtraction = do
     _   <- match SUB
     rd  <- register
     _   <- match COMMA 
     rs1 <- register 
     _   <- match COMMA 
     rs2 <- register 
     let op = 0x33
     let f3 = 0
     let f7 = 0x20
     return $ encodeR op rd f3 rs1 rs2 f7

data ByteCode = ByteCode !U32 

instance Show ByteCode where
    show (ByteCode x) = showHex x ""

sbParse:: Parser Token ByteCode 
sbParse = do
      _   <- match SB
      rs1 <- register 
      _   <- match COMMA 
      imm <- immediate 
      _   <- match LPAREN
      rs2 <- register 
      _   <- match RPAREN 
      let op   = 0x23      
      let imml = trim5 imm 
      let immu = trim7 (imm .>>. 5) 
      let f3   = 0x00
      return $ ByteCode 0
--       (op   .<<. 0 ) .|. 
--       (imml .<<. 7 ) .|.
--       (f3   .<<. 12) .|.
--       (rs1  .<<. 15) .|. 
--       (rs2  .<<. 20) .|. 
--       (immu .<<. 25)        
    
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

data Code = RCode  { op  :: !U32
               , rd  :: !U32
               , f3  :: !U32
               , rs1 :: !U32
               , rs2 :: !U32
               , f7  :: !U32 }
          | ICode  { op  :: !U32
               , rd  :: !U32
               , f3  :: !U32
               , rs1 :: !U32
               , imm :: !U32 }
          | SCode  { op   :: !U32
               , imml :: !U32
               , f3   :: !U32
               , rs1  :: !U32
               , rs2  :: !U32
               , immu :: !U32 } 
          | SBCode { op   :: !U32
               , imml :: !U32
               , f3   :: !U32
               , rs1  :: !U32
               , rs2  :: !U32
               , immu :: !U32 } 
          | UJCode { op  :: !U32 
               , rd  :: !U32 
               , imm :: !U32 }
          deriving (Show, Eq)


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

encode :: Code -> ByteCode
encode (RCode op rd f3 rs1 rs2 f7) = ByteCode $ 
       (op  .<<. 0  ) .|. 
       (rd  .<<. 7  ) .|.
       (f3  .<<. 12 ) .|. 
       (rs1 .<<. 15 ) .|.
       (rs2 .<<. 20 ) .|.
       (f7  .<<. 25 ) 

encode (ICode op rd f3 rs imm) = ByteCode $
       (op  .<<. 0 ) .|. 
       (rd  .<<. 7 ) .|.
       (f3  .<<. 12) .|.
       (rs  .<<. 15) .|. 
       (imm .<<. 20)

encode (SCode op imml f3 rs1 rs2 immu) = ByteCode $
       (op   .<<. 0 ) .|. 
       (imml .<<. 7 ) .|.
       (f3   .<<. 12) .|.
       (rs1  .<<. 15) .|. 
       (rs2  .<<. 20) .|. 
       (immu .<<. 25)

encode (UJCode op rd imm) = ByteCode $
       (op  .<<. 0) .|.
       (rd  .<<. 7) .|.
       (imm .<<. 12)

-- decode :: U32 -> Code 
-- decode x = case codeType opCode of 
--                 RCode -> decodeR x
--                 ICode -> decodeI x 
--                 SCode -> decodeS x
--            where opCode = trim7 (x .>>. 0)  


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

            
type RegisterFile = Map.Map U32 U32

data State = State { regFile  :: RegisterFile
                   , ip       :: U32
                   } deriving (Show)

initState :: State 
initState = State initRegisters 0 

initRegisters :: RegisterFile
initRegisters = Map.fromList $
     zip [0..31] (replicate 31 0)

incInstPtr :: U32 -> State -> State
incInstPtr i (State regFile ip) = State regFile (ip + i)

getReg :: U32 -> State -> U32 
getReg idx state = 
    case (Map.lookup idx (regFile state)) of 
        Just x   -> x 
        Nothing  -> error ("Register " ++ (show idx) ++ " Not valid" )

setReg :: U32 -> U32 -> State -> State
setReg idx val (State reg ip) = State reg' ip
     where reg' = Map.insert idx val reg 
     
eval :: Code -> State -> State 
eval (RCode 0x33 rd 0 rs1 rs2 0) state =
    let d  = getReg rd state
        s1 = getReg rs1 state 
        s2 = getReg rs2 state 
    in  setReg d (s1+s2) state

eval (RCode 0x33 rd 0 rs1 rs2 0x20) state =
    let d  = getReg rd state
        s1 = getReg rs1 state 
        s2 = getReg rs2 state 
    in  setReg d (s1-s2) state

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
