module Parser where

import Control.Applicative ( Alternative((<|>), empty) )
import Data.Char ( isSpace )

newtype ParserState = ParserState
    { buffer :: String
    } deriving Show

newtype Parser a = Parser
    { runParser :: ParserState -> Maybe (a, ParserState)
    }

instance Functor Parser where
    fmap f (Parser p) = Parser $ fmap g . p
        where g (x, state) = (f x, state)

instance Applicative Parser where
    pure x = Parser p
        where p state = pure (x, state)
    (Parser ap) <*> (Parser p) = Parser q
        where q state = do
                (f, state) <- ap state
                (a, state) <- p state
                pure (f a, state)

instance Alternative Parser where
    empty = Parser $ const empty
    (Parser p1) <|> (Parser p2) = Parser p
        where p input = p1 input <|> p2 input

instance Monad Parser where
    (Parser p1) >>= mp = Parser q
        where q state = do
                (a, state) <- p1 state
                runParser (mp a) state


executeParser :: Parser a -> String -> Maybe a
executeParser p s = do
    (a, state) <- runParser p $ ParserState { buffer = s}
    if null (buffer state) then pure a else empty

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

fromTuple :: (a, String) -> (a, ParserState)
fromTuple = mapSnd ParserState

parseIf :: (Char -> Bool) -> Parser Char
parseIf predicate = Parser p
    where p (ParserState (x:xs)) | predicate x = pure $ fromTuple (x, xs)
          p _                                  = empty

parseWhile :: (Char -> Bool) -> Parser String
parseWhile predicate = Parser p
    where p = pure . fromTuple . span predicate . buffer

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