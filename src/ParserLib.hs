module ParserLib
    (
      Parser(..)
    , Result(..)
    , char
    , success
    , string
    , digit
    , anyDigit
    , integer
    , (<|>)
    , many
    , many1
    , exactlyN
    , spaces
    , anyOf
    , failure
    , anyString
    , label
    , choice
--    , Result'(..)
    ) where
        


data Result b a = Success {match :: a, rest :: b}
                | Failure {msg :: String}
                deriving (Show, Eq)

data Parser b a = Parser {run :: b -> Result b a }

--type Parser = GenParser String 
--type Result = GenResult String 

--data Result a = Success {match :: a, rest :: String}
--              | Failure {msg :: String}
--              deriving (Show, Eq)

--data Parser a = Parser { run :: String -> Result a }

parse :: Parser String a -> String -> Result String a 
parse pa str = case run pa str of 
                    Failure msg  -> Failure msg 
                    Success x "" -> Success x ""
                    Success x r  -> Failure "Failed to parse entire"

instance Functor (Parser b) where
    fmap f pa = Parser $ \s -> 
        case run pa s of 
             Failure m   -> Failure m 
             Success x c -> Success (f x) c 

--instance Functor Parser where
--    fmap f pa = Parser $ \s -> 
--        case run pa s of 
--             Failure m   -> Failure m 
--             Success x c -> Success (f x) c        

instance Applicative (Parser b) where 
    pure x    = success x 
    pf <*> pa = apply pf pa

--instance Applicative Parser where 
--    pure x    = success x 
--    pf <*> pa = apply pf pa

instance Monad (Parser b) where 
    pa >>= f = bind pa f 
    pa >> pb = pa *> pb
    return x = success x

--instance Monad Parser where 
--    pa >>= f = bind pa f 
--    pa >> pb = pa *> pb
--    return x = success x 

char :: Char -> Parser String Char
char x = Parser fn
         where fn []    = Failure "Nothing to match"
               fn (h:t) = if (h == x) 
                          then Success x t 
                          else Failure ("Expected " ++ [x] ++ " but got " ++ [h])  

success :: a -> Parser b a 
success x = Parser ( \s -> Success x s )

string :: String -> Parser String String
string x = Parser $ \s -> 
    let n = length x
    in  if   (x == (take n s)) 
        then Success x (drop n s)
        else Failure "Error"

digit :: Int -> Parser String Int
digit x = fmap (\c -> (read c :: Int)) (string (show x))

anyDigit :: Parser String Int 
anyDigit = (digit 0) <|> (digit 1) <|> (digit 2) <|> (digit 3) <|> (digit 4) <|> 
           (digit 5) <|> (digit 6) <|> (digit 7) <|> (digit 8) <|> (digit 9) 

integer :: Parser String Int
integer = 
        fmap (\l -> (fn l)) (many anyDigit)
        where fn l = read (map (head . show) l) :: Int

label :: String -> Parser String a -> Parser String a
label msg pa = Parser $ \s -> 
    case run pa s of 
         Failure _ -> Failure msg
         x         -> x 

andThen :: Parser String a -> Parser String b -> Parser String (a,b)
andThen pa pb = pa >>= (\x -> 
                pb >>= (\y -> 
                return (x,y)))

alt :: Parser String a -> Parser String a -> Parser String a 
alt p1 p2 = Parser $ \s -> 
    case run p1 s of
         Success m r -> Success m r
         Failure _   -> run p2 s

(<|>) :: Parser String a -> Parser String a -> Parser String a 
(<|>) = alt

slice :: Parser String a -> Parser String String 
slice p = 
        Parser $
             \s ->  case run p s of
                         Failure m   -> Failure m
                         Success x r -> let n = (length s) - (length r)
                                        in  Success (take n s) r

map2 :: ((a,b) -> c) -> Parser String a -> Parser String b -> Parser String c
map2 f pa pb = pa >>= (\x -> 
               pb >>= (\y -> 
               return $ f (x,y)))

many :: Parser String a -> Parser String [a]
many pa = 
        Parser (\s -> fn [] s)
        where fn accum [] = Success accum [] 
              fn accum s = case run pa s of
                Failure _    -> Success accum s
                Success x r  -> fn (accum ++ [x]) r
 
many1 :: Parser String a -> Parser String [a]
many1 pa = do { first <- pa
                  ; list  <- many pa
                  ; return (first : list)}

exactlyN :: Parser String a -> Int -> Parser String [a]
exactlyN pa n = 
        Parser $ 
             \s -> case run (many pa) s of 
                        Failure msg   -> Failure msg 
                        Success lst r -> if (length lst) == n 
                                         then (Success lst r) 
                                         else Failure "Error"

    {-- --}
bind :: Parser b a -> (a -> Parser b c) -> Parser b c 
bind pa f = 
        Parser $
             \s -> case run pa s of
                        Failure msg1  -> Failure msg1
                        Success x1 r1 -> run (f x1) r1 

apply :: Parser b (a -> c) -> Parser b a -> Parser b c
apply pf pa = pf >>= (\f -> 
              pa >>= (\x -> 
              return $ f x))

failure :: String -> Parser b a
failure msg = Parser $ \_ -> Failure msg 

-- Can we make this tail recursive
anyOf :: String -> Parser String Char 
anyOf (h:t) = (char h) <|> (anyOf t)
anyOf []    = failure "Could not match any symbols in" 

spaces :: Parser String String 
spaces = many (char ' ')

anyString :: Parser String String 
anyString = Parser $ \s -> Success s []

    -- Parse until we hit String
    --until :: Parser a -> String -> Parser a 
choice :: [Parser String a] -> Parser String a
choice (h:t) = h <|> choice t 
choice []    = failure "None of the alternatives work"



