module ParserLib
    (
      Parser(..)
    , Result(..)
    , CharParser(..)    
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
    , (<!>)
    , choice
    , detect
    , scanAll
    , parse
    ) where
        


data Result t a = Success {match :: a, rest :: [t]}
                | Failure {msg :: String}
                deriving (Show, Eq)

data Parser t a = Parser {run :: [t] -> Result t a }

type CharParser = Parser Char


instance Functor (Parser t) where
    fmap f pa = Parser $ \s -> 
        case run pa s of 
             Failure m   -> Failure m 
             Success x c -> Success (f x) c        

instance Applicative (Parser t) where 
    pure x    = success x 
    pf <*> pa = apply pf pa

instance Monad (Parser t) where 
    pa >>= f = bind pa f 
    pa >> pb = pa *> pb
    return x = success x

success :: a -> Parser t a 
success x = Parser ( \s -> Success x s )

(<!>) :: Parser t a -> String -> Parser t a
pa <!> msg = Parser $ \s -> 
    case run pa s of 
         Failure _ -> Failure msg
         x         -> x 

andThen :: Parser t a -> Parser t b -> Parser t (a,b)
andThen pa pb = pa >>= (\x -> 
                pb >>= (\y -> 
                return (x,y)))

alt :: Parser t a -> Parser t a -> Parser t a 
alt p1 p2 = Parser $ \s -> 
    case run p1 s of
         Success m r -> Success m r
         Failure _   -> run p2 s

(<|>) :: Parser t a -> Parser t a -> Parser t a 
(<|>) = alt


map2 :: ((a,b) -> c) -> Parser t a -> Parser t b -> Parser t c
map2 f pa pb = pa >>= (\x -> 
               pb >>= (\y -> 
               return $ f (x,y)))

many :: Parser t a -> Parser t [a]
many pa = Parser (\s -> fn [] s)
     where fn accum [] = Success accum [] 
           fn accum s = case run pa s of
                             Failure _     -> Success accum s
                             Success x  r  -> fn (accum ++ [x]) r
 
many1 :: Parser t a -> Parser t [a]
many1 pa = pa        >>= (\first -> 
           (many pa) >>= (\list  ->
           return (first : list))) 

scanAll :: Parser t a -> Parser t [a]
scanAll pa = Parser (\s -> fn s [])
     where fn [] accum = Success accum [] 
           fn s  accum = case run pa s of
                              Failure _    -> fn (tail s) accum       
                              Success x r  -> fn r        (accum ++ [x])

exactlyN :: Parser t a -> Int -> Parser t [a]
exactlyN pa n = Parser $ \s -> 
    case run (many pa) s of 
         Failure msg   -> Failure msg 
         Success lst r -> if   (length lst) == n 
                          then (Success lst r) 
                          else Failure "Error"

bind :: Parser t a -> (a -> Parser t b) -> Parser t b 
bind pa f = Parser $ \s -> 
    case run pa s of
         Failure msg1  -> Failure msg1
         Success x1 r1 -> run (f x1) r1 

apply :: Parser b (a -> c) -> Parser b a -> Parser b c
apply pf pa = pf >>= (\f -> 
              pa >>= (\x -> 
              return $ f x))

failure :: String -> Parser b a
failure msg = Parser $ \_ -> Failure msg 

-- Parse until we hit String
--until :: Parser a -> String -> Parser a 
choice :: [Parser t a] -> Parser t a
choice (h:t) = h <|> choice t 
choice []    = failure "None of the alternatives work"

-- Generalization of char parser
detect :: (Eq t) => t -> Parser t t 
detect x = Parser $ \s -> 
    case s of 
        []     -> Failure "Empty string, doesn't match"
        h:rest -> if   h == x  
                  then Success x rest 
                  else Failure "Doesn't match"

anyOf :: (Eq t) => [t] -> Parser t t 
anyOf (h:t) = (detect h) <|> (anyOf t)
anyOf []    = failure "Could not match any symbols in" 

parse :: Parser t a -> [t] -> Either String a
parse p str = 
    case run p str of
         Success x _ -> Right x
         Failure msg -> Left msg
---------------------------------------------------------------------------------------

char :: Char -> CharParser Char
char x = detect x  

string :: String -> CharParser String
string x = Parser $ \s -> 
    let n = length x
    in  if   (x == (take n s)) 
        then Success x (drop n s)
        else Failure "Error"

digit :: Int -> CharParser Int
digit x = fmap (\c -> (read c :: Int)) (string (show x))

anyDigit :: CharParser Int 
anyDigit = (digit 0) <|> (digit 1) <|> (digit 2) <|> (digit 3) <|> (digit 4) <|> 
           (digit 5) <|> (digit 6) <|> (digit 7) <|> (digit 8) <|> (digit 9) 

integer :: CharParser Int
integer = fmap (\l -> (fn l)) (many anyDigit)
          where fn l = read (map (head . show) l) :: Int


-- Can we make this tail recursive
--anyOf :: String -> CharParser Char 
--anyOf (h:t) = (char h) <|> (anyOf t)
--anyOf []    = failure "Could not match any symbols in" 

spaces :: CharParser String 
spaces = many (char ' ')

anyString :: CharParser String 
anyString = Parser $ \s -> Success s []





