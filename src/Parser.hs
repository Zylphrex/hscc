module Parser ( Parser
              , Parse(parse)
              , parseIf
              , parseCharacter
              , parseString
              , parseWhile
              , parseSpaces
              , parseNotNull
              , tryParser
              , executeParser
              ) where

import Control.Applicative ( Alternative((<|>), empty) )
import Control.Monad.State ( StateT(StateT)
                           , evalStateT
                           , mapStateT
                           , runStateT
                           )
import Data.Char ( isSpace )

newtype ParserState = ParserState
    { buffer :: String
    } deriving (Eq, Show)

instance Read ParserState where
    readsPrec _ s = [(ParserState { buffer = s }, "")]

newtype Parser a = Parser
    { runParser :: StateT ParserState Maybe a
    }

instance Functor Parser where
    f `fmap` (Parser p) = Parser $ f <$> p

instance Applicative Parser where
    pure x = Parser $ pure x
    (Parser ap) <*> (Parser p) = Parser $ ap <*> p

instance Alternative Parser where
    empty = Parser $ StateT $ pure empty
    (Parser p1) <|> (Parser p2) = Parser $ p1 <|> p2

instance Monad Parser where
    (Parser p) >>= mp = Parser $ StateT q
        where q state = do
                (a, state) <- runStateT p state
                runStateT (runParser (mp a)) state

class Parse a where
    parse :: Parser a

tryParser :: Parser a -> String -> Maybe (a, ParserState)
tryParser (Parser p) s = runStateT p $ read s

executeParser :: Parser a -> String -> Maybe a
executeParser p s = do
    (a, state) <- tryParser p s
    if null (buffer state)
        then pure a
        else empty

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

fromPair :: (a, String) -> (a, ParserState)
fromPair = mapSnd ParserState

parseIf :: (Char -> Bool) -> Parser Char
parseIf predicate = Parser $ StateT p
    where p (ParserState (x:xs))
              | predicate x = pure $ fromPair (x, xs)
          p _               = empty

parseWhile :: (Char -> Bool) -> Parser String
parseWhile predicate = Parser $ StateT p
    where p = pure . fromPair . span predicate . buffer

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
