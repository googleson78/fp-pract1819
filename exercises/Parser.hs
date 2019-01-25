{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Parser
    ( Parser
    , result
    , fail
    , nom
    , parse
    , (<|>)
    , many
    , many1
    , endOfInput
    ) where

import Prelude hiding (fail, takeWhile)
import Data.Maybe (listToMaybe)
import Control.Applicative (Alternative (empty))
import qualified Control.Applicative as A ((<|>), many, some)

newtype Parser a = Parser { runParser :: String -> [(String, a)] }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $ map (fmap f) . p

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \str -> [(str, x)]
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    Parser parsef <*> Parser parsex = Parser $ \str ->
        [ (rest2, res1 res2) | (rest1, res1) <- parsef str
                             , (rest2, res2) <- parsex rest1
        ]

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ const []
    (<|>) :: Parser a -> Parser a -> Parser a
    Parser x <|> Parser y = Parser $ \str ->
        x str ++ y str


instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    Parser px >>= f
        = Parser $ \str ->
            [ (rest2, res2) | (rest1, res1) <- px str
                            , (rest2, res2) <- runParser (f res1) rest1
            ]

result :: a -> Parser a
result = pure

fail :: Parser a
fail = empty

nom :: Parser Char
nom = Parser $ \case
    []   -> []
    x:xs -> [(xs, x)]

endOfInput :: Parser ()
endOfInput = Parser $ \case
    [] -> pure $ pure ()
    _  -> []

parse :: Parser a -> String -> Maybe a
parse p = fmap snd . listToMaybe . runParser p

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = (A.<|>)

many :: Parser a -> Parser [a]
many = A.many

many1 :: Parser a -> Parser [a]
many1 = A.some
