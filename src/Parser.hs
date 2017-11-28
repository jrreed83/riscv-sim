module Parser 
    (
      Parser(..)
    , Result(..)
    , char
    , success
    , string
    , digit
    , anyDigit
    , integer
    , (~>>~)
    , (<|>)
    , (~>>)
    , (>>~)
    , many
    , many1
    , exactlyN
    , spaces
    , anyOf
    , failure
    , anyString
    , label
    ) where
        

data Result a = Success {match :: a, rest :: String}
              | Failure {msg :: String}
              deriving (Show, Eq)

data Parser a = Parser { run :: String -> Result a }

parse :: Parser a -> String -> Result a 
parse pa str = case run pa str of 
                    Failure msg  -> Failure msg 
                    Success x "" -> Success x ""
                    Success x r  -> Failure "Failed to parse entire"

instance Functor Parser where
    fmap f pa = Parser $ 
                    \s -> case run pa s of 
                               Failure m   -> Failure m 
                               Success x c -> Success (f x) c        

instance Applicative Parser where 
    pure x    = success x 
    pf <*> pa = apply pf pa

instance Monad Parser where 
    pa >>= f = bind pa f 
    pa >> pb = pa >>~ pb
    return x = success x 

char :: Char -> Parser Char
char x = Parser fn
         where fn []    = Failure "Nothing to match"
               fn (h:t) = if (h == x) 
                          then Success x t 
                          else Failure ("Expected " ++ [x] ++ " but got " ++ [h])  

success :: a -> Parser a 
success x = Parser ( \s -> Success x s )

string :: String -> Parser String
string x = Parser $
                     \s -> let n = length x
                           in  if (x == (take n s)) then 
                                    Success x (drop n s)
                               else 
                                    Failure "Error"

digit :: Int -> Parser Int
digit x = fmap (\c -> (read c :: Int)) (string (show x))

anyDigit :: Parser Int 
anyDigit = (digit 0) <|> (digit 1) <|> (digit 2) <|> (digit 3) <|> (digit 4) <|> 
               (digit 5) <|> (digit 6) <|> (digit 7) <|> (digit 8) <|> (digit 9) 

integer :: Parser Int
integer = 
        fmap (\l -> (fn l)) (many anyDigit)
        where fn l = read (map (head . show) l) :: Int

label :: String -> Parser a -> Parser a
label msg pa = 
        Parser $
             \s -> case run pa s of 
                        Failure _ -> Failure msg
                        x         -> x 

andThen :: Parser a -> Parser b -> Parser (a,b)
andThen pa pb = pa >>= (\x -> 
                pb >>= (\y -> 
                return (x,y)))

(~>>~) :: Parser a -> Parser b -> Parser (a,b) 
(~>>~) = andThen

alt :: Parser a -> Parser a -> Parser a 
alt p1 p2 = 
        Parser $
             \s -> case run p1 s of
                        Success m r -> Success m r
                        Failure _   -> run p2 s

(<|>) :: Parser a -> Parser a -> Parser a 
(<|>) = alt

slice :: Parser a -> Parser String 
slice p = 
        Parser $
             \s ->  case run p s of
                         Failure m   -> Failure m
                         Success x r -> let n = (length s) - (length r)
                                        in  Success (take n s) r

map2 :: ((a,b) -> c) -> Parser a -> Parser b -> Parser c
map2 f pa pb = pa >>= (\x -> 
               pb >>= (\y -> 
               return $ f (x,y)))

many :: Parser a -> Parser [a]
many pa = 
        Parser (fn [])
        where fn accum [] = Success accum [] 
              fn accum s = case run pa s of
                Failure _    -> Success accum s
                Success x r  -> fn (accum ++ [x]) r
 
many1 :: Parser a -> Parser [a]
many1 pa = do { first <- pa
                  ; list  <- many pa
                  ; return (first : list)}

exactlyN :: Parser a -> Int -> Parser [a]
exactlyN pa n = 
        Parser $ 
             \s -> case run (many pa) s of 
                        Failure msg   -> Failure msg 
                        Success lst r -> if (length lst) == n 
                                         then (Success lst r) 
                                         else Failure "Error"

    {-- --}
bind :: Parser a -> (a -> Parser b) -> Parser b 
bind pa f = 
        Parser $
             \s -> case run pa s of
                        Failure msg1  -> Failure msg1
                        Success x1 r1 -> run (f x1) r1 

apply :: Parser (a -> b) -> Parser a -> Parser b
apply pf pa = pf >>= (\f -> 
              pa >>= (\x -> 
              return $ f x))
    

(>>~) :: Parser a -> Parser b -> Parser b 
pa >>~ pb = pa >>= (\xa -> 
            pb >>= (\xb -> 
            return xb ))

(~>>) :: Parser a -> Parser b -> Parser a 
pa ~>> pb = pa >>= (\xa -> 
            pb >>= (\xb -> 
            return xa ))

failure :: String -> Parser a
failure msg = Parser $ \_ -> Failure msg 

-- Can we make this tail recursive
anyOf :: String -> Parser Char 
anyOf (h:t) = (char h) <|> (anyOf t)
anyOf []    = failure "Could not match any symbols in" 

spaces :: Parser String 
spaces = many (char ' ')

anyString :: Parser String 
anyString = Parser $ \s -> Success s []

    -- Parse until we hit String
    --until :: Parser a -> String -> Parser a 




