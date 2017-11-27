module Instructions
where

import Parser
import qualified Data.ByteString as B 
import qualified Data.Word as W
import Data.Bits 
import Data.IORef 
import qualified Data.Vector as V 
import Text.Printf 
import Foreign
import Foreign.C.Types 

foreign import ccall "hello"
  c_hello :: IO () 

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
eol :: Parser String 
eol = (string "\n") <|> spaces

comma :: Parser Char 
comma = (char ',') 

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

pAdd :: Parser Instruction 
pAdd = do
    _   <- string "add"
    _   <- spaces 
    rd  <- register
    _   <- comma 
    rs1 <- register 
    _   <- comma 
    rs2 <- register 
    return $ R 0x33 rd 0 rs1 rs2 0 

pSubtract :: Parser Instruction
pSubtract = do
    _   <- string "sub"
    _   <- spaces 
    rd  <- register
    _   <- comma 
    rs1 <- register 
    _   <- comma 
    rs2 <- register 
    return $ R 0x33 rd 0 rs1 rs2 0x20                                     

pStoreByte :: Parser Instruction 
pStoreByte = do
    _   <- string "sb"
    _   <- spaces 
    rs1 <- register 
    _   <- comma 
    imm <- u32 
    _   <- lparen
    rs2 <- register 
    _   <- rparen 
    let imml = trim5 imm 
    let immu = trim7 (imm .>>. 5) 
    let op   = 0x23
    let f3   = 0x00
    return $ S op imml f3 rs1 rs2 immu    
    
pStoreHalfWord :: Parser Instruction 
pStoreHalfWord = do
    _   <- string "sh"
    _   <- spaces 
    rs1 <- register 
    _   <- comma 
    imm <- u32 
    _   <- lparen
    rs2 <- register 
    _   <- rparen 
    let imml = trim5 imm 
    let immu = trim7 (imm .>>. 5)
    let op   = 0x23 
    let f3   = 0x01
    return $ S op imml f3 rs1 rs2 immu     

pStoreWord :: Parser Instruction 
pStoreWord = do
    _   <- string "sw"
    _   <- spaces 
    rs1 <- register 
    _   <- comma 
    imm <- u32 
    _   <- lparen
    rs2 <- register 
    _   <- rparen 
    let imml = trim5 imm 
    let immu = trim7 (imm .>>. 5)
    let op   = 0x23 
    let f3   = 0x02
    return $ S op imml f3 rs1 rs2 immu  
    
pStoreDoubleWord :: Parser Instruction 
pStoreDoubleWord = do
    _   <- string "sd"
    _   <- spaces 
    rs1 <- register 
    _   <- comma 
    imm <- u32 
    _   <- lparen
    rs2 <- register 
    _   <- rparen 
    let imml = trim5 imm 
    let immu = trim7 (imm .>>. 5)
    let op   = 0x23 
    let f3   = 0x03
    return $ S op imml f3 rs1 rs2 immu    

pLoadByte :: Parser Instruction
pLoadByte = do
    _   <- string "lb"
    _   <- spaces 
    rd  <- register 
    _   <- comma 
    imm <- u32 
    _   <- lparen
    rs1 <- register 
    _   <- rparen 
    let op   = 0x03 
    let f3   = 0x00
    return $ I op rd f3 rs1 imm       

pLoadHalfWord :: Parser Instruction
pLoadHalfWord = do
    _   <- string "lh"
    _   <- spaces 
    rd  <- register 
    _   <- comma 
    imm <- u32 
    _   <- lparen
    rs1 <- register 
    _   <- rparen 
    let op   = 0x03 
    let f3   = 0x01
    return $ I op rd f3 rs1 imm 
    
pLoadWord :: Parser Instruction
pLoadWord = do
    _   <- string "lw"
    _   <- spaces 
    rd  <- register 
    _   <- comma 
    imm <- u32 
    _   <- lparen
    rs1 <- register 
    _   <- rparen 
    let op   = 0x03 
    let f3   = 0x02
    return $ I op rd f3 rs1 imm  
    
pLoadDoubleWord :: Parser Instruction
pLoadDoubleWord = do
    _   <- string "ld"
    _   <- spaces 
    rd  <- register 
    _   <- comma 
    imm <- u32 
    _   <- lparen
    rs1 <- register 
    _   <- rparen 
    let op   = 0x03 
    let f3   = 0x03
    return $ I op rd f3 rs1 imm      
----------------------------------------------------------------------------------

data Instruction = R { op :: !U32, rd   :: !U32, f3 :: !U32, rs1 :: !U32, rs2 :: !U32, f7   :: !U32 } 
                 | I { op :: !U32, rd   :: !U32, f3 :: !U32, rs1 :: !U32, imm :: !U32               }
                 | S { op :: !U32, imml :: !U32, f3 :: !U32, rs1 :: !U32, rs2 :: !U32, immu :: !U32 }
                 | SB{ op :: !U32, imml :: !U32, f3 :: !U32, rs1 :: !U32, rs2 :: !U32, immu :: !U32 }
                 deriving (Show, Eq)
 
data InstructionType = RType | SType | IType | SBType
                       deriving (Show,Eq)

instType :: U32 -> InstructionType 
instType 0x33 = RType
instType 0x3B = RType 
instType 0x03 = IType 
instType 0x0F = IType
instType 0x67 = IType
instType 0x73 = IType
instType 0x23 = SType
instType 0x63 = SBType

encode :: Instruction -> U32 
encode (R op rd f3 rs1 rs2 f7) = 
     (op  .<<. 0  ) .|. 
     (rd  .<<. 7  ) .|.
     (f3  .<<. 12 ) .|. 
     (rs1 .<<. 15 ) .|.
     (rs2 .<<. 20 ) .|.
     (f7  .<<. 25 ) 

encode (I op rd f3 rs imm) = 
     (op  .<<. 0 ) .|. 
     (rd  .<<. 7 ) .|.
     (f3  .<<. 12) .|.
     (rs  .<<. 15) .|. 
     (imm .<<. 20)

encode (S op imml f3 rs1 rs2 immu) = 
     (op   .<<. 0 ) .|. 
     (imml .<<. 7 ) .|.
     (f3   .<<. 12) .|.
     (rs1  .<<. 15) .|. 
     (rs2  .<<. 20) .|. 
     (immu .<<. 25)

decode :: U32 -> Instruction 
decode x = 
    let op = trim7 (x .>>.  0) 
    in  case (instType op) of 
             RType -> decodeR x --R op 0 f3 0 0 0
             IType -> decodeI x --I op 0 f3 0 0
             SType -> decodeS x 

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

decodeR :: U32 -> Instruction 
decodeR x = R op rd f3 rs1 rs2 f7
            where op  = trim7 (x .>>. 0 )
                  rd  = trim5 (x .>>. 7 )
                  f3  = trim3 (x .>>. 12)
                  rs1 = trim5 (x .>>. 15)
                  rs2 = trim5 (x .>>. 20)
                  f7  = trim6 (x .>>. 25)

decodeI :: U32 -> Instruction 
decodeI x = I op rd f3 rs1 imm 
            where op  = trim7  (x .>>. 0 )
                  rd  = trim5  (x .>>. 7 )
                  f3  = trim3  (x .>>. 12)
                  rs1 = trim5  (x .>>. 15)
                  imm = trim11 (x .>>. 20)

decodeS :: U32 -> Instruction
decodeS x = S op imml f3 rs1 rs2 immu
            where op   = trim7 (x .>>. 0 )
                  imml = trim5 (x .>>. 7 )
                  f3   = trim3 (x .>>. 12)
                  rs1  = trim5 (x .>>. 15)
                  rs2  = trim5 (x .>>. 20)
                  immu = trim6 (x .>>. 25)


bitPattern :: U32 -> String 
bitPattern x =
    show ((x .>>. 31) .&. 1) ++  
    show ((x .>>. 30) .&. 1) ++  
    show ((x .>>. 29) .&. 1) ++  
    show ((x .>>. 28) .&. 1) ++    
    show ((x .>>. 27) .&. 1) ++  
    show ((x .>>. 26) .&. 1) ++  
    show ((x .>>. 25) .&. 1) ++  
    show ((x .>>. 24) .&. 1) ++    
    show ((x .>>. 23) .&. 1) ++  
    show ((x .>>. 22) .&. 1) ++  
    show ((x .>>. 21) .&. 1) ++  
    show ((x .>>. 20) .&. 1) ++    
    show ((x .>>. 19) .&. 1) ++  
    show ((x .>>. 18) .&. 1) ++  
    show ((x .>>. 17) .&. 1) ++  
    show ((x .>>. 16) .&. 1) ++  
    show ((x .>>. 15) .&. 1) ++  
    show ((x .>>. 14) .&. 1) ++  
    show ((x .>>. 13) .&. 1) ++  
    show ((x .>>. 12) .&. 1) ++    
    show ((x .>>. 11) .&. 1) ++  
    show ((x .>>. 10) .&. 1) ++  
    show ((x .>>. 9 ) .&. 1) ++  
    show ((x .>>. 8 ) .&. 1) ++    
    show ((x .>>. 7 ) .&. 1) ++  
    show ((x .>>. 6 ) .&. 1) ++  
    show ((x .>>. 5 ) .&. 1) ++  
    show ((x .>>. 4 ) .&. 1) ++    
    show ((x .>>. 3 ) .&. 1) ++  
    show ((x .>>. 2 ) .&. 1) ++  
    show ((x .>>. 1 ) .&. 1) ++  
    show ((x .>>. 0 ) .&. 1)             


data State = State { registers :: V.Vector U32 
                   , ip        :: U32
                   } deriving (Show)

initState :: State 
initState = State r 0
            where r = V.replicate 32 (0::U32)

add :: Instruction -> State -> State 
add (R _ rd _ rs rt _ ) state =  
    let registers' = registers state
        id         = asInt rd 
        is         = asInt rs 
        it         = asInt rt  
        xs         = registers' ! is 
        xt         = registers' ! it  
        xd         = xs + xt
        ip'        = (ip state) + 1        
    in  State  (registers' // [(id,xd)]) ip'

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
