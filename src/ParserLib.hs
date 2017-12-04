module ParserLib
    (
      Parser(..)
    , Result(..)
    , success
    , string
    , digit
    , integer
    , (<|>)
    , many
    , many1
    , exactlyN
    , whiteSpace
    , anyOf
    , failure
    , anyString
    , (<!>)
    , choice
    , match
    , scanAll
    , parse
    , alphaNumeric
    ) where
        
import Data.Char as C 

data Result t a = Success {first :: a, rest :: [t]}
                | Failure {msg :: String}
                deriving (Show, Eq)

data Parser t a = Parser {run :: [t] -> Result t a }

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
           fn accum s  = case run pa s of
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

detect :: (Eq t) => (t->Bool) -> Parser t t 
detect f = Parser $ \s -> 
    case s of 
        []     -> Failure "Empty string, doesn't match"
        h:rest -> if (f h)  
                  then Success h rest 
                  else Failure "Doesn't match"

match :: (Eq t) => t -> Parser t t 
match x = detect (==x)

anyOf :: (Eq t) => [t] -> Parser t t 
anyOf (h:t) = (match h) <|> (anyOf t)
anyOf []    = failure "Could not match any symbols in" 

parse :: Parser t a -> [t] -> Either String a
parse p str = 
    case run p str of
         Success x _ -> Right x
         Failure msg -> Left msg
---------------------------------------------------------------------------------------
 

string :: String -> Parser Char String
string x = Parser $ \s -> 
    let n = length x
    in  if   (x == (take n s)) 
        then Success x (drop n s)
        else Failure "Error"

digit :: Parser Char Int
digit = (detect C.isDigit) >>= (\c -> return $ C.digitToInt c)
 

integer :: Parser Char Int
integer = fmap (\l -> (fn l)) (many1 digit)
          where fn l = read (map (head . show) l) :: Int

whiteSpace :: Parser Char String 
whiteSpace = many1 (match ' ')

anyString :: Parser Char String 
anyString = Parser $ \s -> Success s []

alphaNumeric :: Parser Char String
alphaNumeric = many1 (detect C.isAlphaNum)




