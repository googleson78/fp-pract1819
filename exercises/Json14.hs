module Json14 where

import Prelude hiding (fail, takeWhile)
import Data.Char (ord)

import Parser

-- Parser lib docs:
--
-- the Parser type
-- it eats from the beginning of a string
--
-- parse :: Parser a -> String -> Maybe a
-- parse p str "runs" the parser p on the string str and then
-- if parsing succeded with return a Just result, otherwise we return Nothing
--
-- nom :: Parse Char
-- nom consumes one character and returns it, only failing if we parse an empty string
--
-- Example:
-- > parse nom "b"
-- Just 'b'
-- > parse nom "asdf"
-- Just 'a'
-- > parse nom ""
-- Nothing
--
-- result :: a -> Parser a
-- result x doesn't consume any input and always succeds by returning x as the result of the parse
-- (result is a synonym for pure)
--
-- Example:
-- > parse (result 100) "whatever I write here doesn't matter"
-- Just 100
--
-- > parse (result "yoyo") "whatever I write here doesn't matter"
-- Just "yoyo"
--
-- fail :: Parser a
-- fails doesn't consume any input and immediately fails parsing
--
-- Example:
-- > parse fail "whatever I write here doesn't matter"
-- Nothing
--
-- Parser is a functor it has an fmap
-- fmap :: (a -> b) -> Parser a -> Parser b
--
-- we can think of the fmap as doing the following:
-- first apply the parser, and then apply f :: (a -> b) to its result
-- (<$>) is a synonym for fmap
-- (<$>) = fmap
--
-- Example:
-- -- ord :: Char -> Int, returns the ASCII code of a character
-- > parse (fmap succ nom) "a"
-- Just 'b'
-- > parse (fmap ord nom) "a"
-- Just 97
-- > parse (ord <$> nom) "A"
-- Just 65
-- > parse (fmap (=='b') nom) "a"
-- Just False
-- > parse (fmap (=='b') nom) "b"
-- Just True
--
-- (<|>) :: Parser a -> Parser a -> Parser a
-- (<|>) first attempts to use p1, if it fails it attempts to use p2 (backtracking automatically)
-- -- char isn't defined yet, but char c is "a parser that only accepts c"
-- -- we will write it soon
-- > parse (char 'c' <|> char 'b') "c"
-- Just 'c'
-- > parse (char 'c' <|> char 'b') "b"
-- Just 'b'
-- > parse (char 'c' <|> char 'b') "cb"
-- Just 'c'
-- -- string isn't defined here yet, but string str is "a parser that only accepts str"
-- -- we will write it soon
-- > parse (string "asdf" <|> string "azerty") "azerty"
-- Just "azerty"
-- > parse (char 'c' <|> char 'b') "asdf"
-- Nothing
--
-- many :: Parser a -> Parser [a]
-- many p applies the parser p zero or more times, returning the result as a list
-- be wary that if p doesn't consume any input this will succeed forever!
--
-- > parse (many nom) "asdf"
-- Just "asdf"
-- > parse (many nom) ""
-- Just ""
-- > parse (many (char 'a')) "asdf"
-- Just "a"
-- > parse (many (char 'a')) "badf"
-- Just ""
--
-- many1 :: Parser a -> Parser [a]
-- many1 is the same as many, execpt it applies p *one or more times*
-- > parse (many1 nom) "asdf"
-- Just "asdf"
-- > parse (many1 nom) ""
-- Nothing

-- demonstrate this
-- we parse symbols that satisfy the predicate p
-- > parse (satisfy isSpace) " "
-- Just ' '
-- > parse (satisfy isSpace) "a"
-- Nothing
-- > parse (satisfy isDigit) "1"
-- Just '1'
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = undefined

-- we parse the c we are given
-- > parse (char 'a') "a"
-- Just "a"
-- > parse (char 'b') "a"
-- Nothing
char :: Char -> Parser Char
char c = undefined

-- we parse string we are given
--
-- > parse (string "asdf") "asdf"
-- Just "asdf"
-- > parse (string "asdf") "asd"
-- Nothing
-- > parse (string "as") "asdf"
-- Just "as"
string :: String -> Parser String
string = undefined

-- we consume n chars from the input, failing if there are less than n
-- > parse (takeChar 5) "01234"
-- Just "01234"
-- > parse (takeChar 5) "0123456789"
-- Just "01234"
-- > parse (takeChar 5) "012"
-- Nothing
takeChar :: Int -> Parser String
takeChar = undefined

-- we execute the parser p, ignoring it's result
-- note that if p fails, we still fail alltogether
--
-- Example:
-- > parse (void nom) "asdf"
-- Just ()
-- > parse (void nom) ""
-- Nothing
void :: Parser a -> Parser ()
void p = undefined

--value
--    object
--    array
--    string
--    number
--    "true"
--    "false"
--    "null"
data Value
    = Null
    | Bool Bool
    | Number Integer
    | String String
    | Array [Value]
    | Object [(String, Value)]
    deriving (Show)

-- succeeds only on "null", returning Null of type Value
--
-- Example:
-- > parse nullParser "null"
-- Just Null
-- > parse nullParser "Null"
-- Nothing
nullParser :: Parser Value
nullParser = undefined

-- succeeds only on "false", returning Bool False of type Value
--
-- Example:
-- > parse nullParser "false"
-- Just (Bool False)
-- > parse nullParser "False"
-- Nothing
falseParser :: Parser Value
falseParser = undefined

-- succeeds only on "true", returning Bool True of type Value
--
-- Example:
-- > parse nullParser "true"
-- Just (Bool True)
-- > parse nullParser "True"
-- Nothing
trueParser :: Parser Value
trueParser = undefined

-- we first attempt to parse False and then we attempt to parse True
--
-- Example:
-- > parse boolParser "true"
-- Just (Bool True)
-- > parse boolParser "false"
-- Just (Bool False)
-- > parse boolParser "lol"
-- Nothing
boolParser :: Parser Value
boolParser = undefined

-- we parse a single digit
-- you're going to need ord and fromIntegral here
--
-- Example:
-- > parse digitParser "1"
-- Just 1
-- > parse digitParser "123"
-- Just 1
-- > parse digitParser "a"
-- Nothing
digitParser :: Parser Integer
digitParser = undefined

-- we parse many digits, as a number, returning the result as a Value, by wrapping it with Number
-- many1 is useful here
--
-- Example:
-- > parse numberParser "1234"
-- Just (Number 1234)
-- > parse numberParser "1234a"
-- Just (Number 1234)
-- > parse numberParser "a1234"
-- Nothing
numberParser :: Parser Value
numberParser = undefined

-- we apply the parser p, but we also consume the char c once before
-- and after p
--
-- Example:
-- > parse (bracket '|' (string "lol")) "|lol|"
-- Just "lol"
-- > parse (bracket '|' (string "lol")) "lol|"
-- Nothing
-- > parse (bracket '|' (string "lol")) "|lol"
-- Nothing
-- > parse (bracket '|' (string "lol")) "|notlol|"
-- Nothing
-- > parse (bracket '"' (string "lol")) "\"lol\""
-- Just "lol"
bracket :: Char -> Parser a -> Parser a
bracket c p = undefined

-- we consume characters while the predicate f holds
-- this consumes at least one character
-- > parse (takeWhile1 (=='a')) "aasdf"
-- Just "aa"
-- > parse (takeWhile1 (\x -> x == 'a' || x == 'b')) "abaabbabbsdf"
-- Just "abaabbabb"
-- > parse (takeWhile1 (=='a')) ""
-- Nothing
takeWhile1 :: (Char -> Bool) -> Parser String
takeWhile1 f = undefined

-- we parse strings in json, which are surrounded by double quotes
--
-- Example:
-- > parse stringParser "\"yoyo\""
-- Just (String "yoyo")
-- > parse stringParser "yoyo\""
-- Nothing
-- > parse stringParser "\"yoyo"
-- Nothing
stringParser :: Parser Value
stringParser = undefined

-- we *attempt* to use the parser we are given
-- if it succeds we return its result, otherwise we return Nothing
--
-- Example:
-- > parse (optional (char 'c')) "c"
-- Just (Just 'c')
-- > parse (optional (char 'c')) "b"
-- Just Nothing
optional :: Parser a -> Parser (Maybe a)
optional p = undefined

-- we apply p zero or more times, seperating the parses
-- with parses of the char c
-- many and optional are useful here
--
-- > parse (string "yo" `sepBy` ',') "yo,yo,yo,a"
-- Just ["yo","yo","yo"]
-- > parse (string "yo" `sepBy` ',') ""
-- Just []
-- > parse (string "yo" `sepBy` ',') "yo,a,yo"
-- Just ["yo"]
sepBy :: Parser a -> Char -> Parser [a]
sepBy p c = undefined

-- we parse a json array, which is the same as haskell lists,
-- surrounded by square brackets and seperated by ','
-- except we can have any Value's inside it.
-- assume that a global valueParser :: Parser Value, already exists, we are going to define it below
--
-- > parse arrayParser "[1]"
-- Just (Array [Number 1])
-- > parse arrayParser "[null]"
-- Just (Array [Null])
-- > parse arrayParser "[1,2,null,false,true,\"string\"]"
-- Just (Array [Number 1,Number 2,Null,Bool False,Bool True,String "string"])
arrayParser :: Parser Value
arrayParser = undefined

-- we parse a json object element, which is just a mapping from strings to values
-- it's in the format
-- "string" : value
-- assume that a global valueParser :: Parser Value already exists, we are going to define it below
--
-- > parse objectElemParser "\"lol\":null"
-- Just ("lol",Null)
-- > parse objectElemParser "\"lol\":5"
-- Just ("lol",Number 5)
-- > parse objectElemParser "\"kek\":\"yoyo\""
-- Just ("kek",String "yoyo")
objectElemParser :: Parser (String, Value)
objectElemParser = undefined

-- we parse a json object, json objects are a lot of object elements, surrounded by curly braces
-- and seperated by ','
-- it can be empty
-- > parse objectParser "{}"
-- Just (Object [])
-- > parse objectParser "{\"lol\":null}"
-- Just (Object [("lol",Null)])
-- > parse objectParser "{\"lol\":null,\"kek\":\"yoyo\"}"
-- Just (Object [("lol",Null),("kek",String "yoyo")])
-- > parse objectParser "{\"lol\":null,\"kek\":\"yoyo\",\"last\":true}"
-- Just (Object [("lol",Null),("kek",String "yoyo"),("last",Bool True)])
objectParser :: Parser Value
objectParser = undefined

-- parse a json object in general!
-- we must attempt all the previous parsers here!
--
-- > parse valueParser "{\"lol\":null,\"kek\":\"yoyo\",\"last\":69}"
-- Just (Object [("lol",Null),("kek",String "yoyo"),("last",Number 69)])
-- > parse valueParser "null"
-- Just Null
-- > parse valueParser "false"
-- Just (Bool False)
-- > parse valueParser "[false,true]"
-- Just (Array [Bool False,Bool True])

valueParser :: Parser Value
valueParser = undefined
