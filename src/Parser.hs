module Parser where

import Control.Applicative ( Alternative((<|>), empty) )
import Data.Char ( isSpace )

newtype Parser a = Parser
    { runParser :: String -> Maybe (a, String)
    }

executeParser :: Parser a -> String -> Maybe a
executeParser p input = do
    (a, input) <- runParser p input
    if null input then pure a else empty

instance Functor Parser where
    fmap f (Parser p) = Parser $ fmap g . p
        where g (x, input) = (f x, input)

instance Applicative Parser where
    pure x = Parser p
        where p input = pure (x, input)
    (Parser ap) <*> (Parser p) = Parser q
        where q input = do
                (f, input) <- ap input
                (a, input) <- p input
                pure (f a, input)

instance Alternative Parser where
    empty = Parser $ const empty
    (Parser p1) <|> (Parser p2) = Parser p
        where p input = p1 input <|> p2 input

instance Monad Parser where
    (Parser p1) >>= mp = Parser q
        where q input = do
                (a, input) <- p1 input
                runParser (mp a) input

parseIf :: (Char -> Bool) -> Parser Char
parseIf predicate = Parser p
    where p (x:xs) | predicate x = pure (x, xs)
          p _                    = empty

parseWhile :: (Char -> Bool) -> Parser String
parseWhile predicate = Parser $ pure . span predicate

parseCharacter :: Char -> Parser Char
parseCharacter c = parseIf (== c)

parseString :: String -> Parser String
parseString = traverse parseCharacter

parseSpaces :: Parser String
parseSpaces = parseWhile isSpace

parseNotNull :: Parser [a] -> Parser [a]
parseNotNull p = do
    xs <- p
    if null xs then empty else pure xs