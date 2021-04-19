module Ast.Identifier ( Identifier, toIdentifier ) where

import Data.Char ( isDigit, isLetter )
import Text.PrettyPrint ( text )

import Assembly ( Assembly(toAssembly)
                , joinAssembly
                , Option
                , OsOption(Darwin)
                , osOption
                )
import Parser ( Parse(parse)
              , parseIf
              , parseWhile
              )
import Pretty ( PrettyPrint(prettyPrint) )

newtype Identifier = Identifier String
    deriving (Eq, Show)

toIdentifier :: String -> Identifier
toIdentifier s = if isValidIdentifier s
                 then Identifier s
                 else error $ s ++ " is not a valid identifier"

instance Parse Identifier where
    parse = toIdentifier <$> ((:) <$> parseIf isLeadingChar <*> parseWhile isIdentifierChar)

instance Assembly Identifier where
    toAssembly opt (Identifier identifier) =
        case osOption opt of
            Darwin -> '_' : identifier
            _      -> identifier

instance PrettyPrint Identifier  where
    prettyPrint (Identifier identifier) = text identifier

isLeadingChar :: Char -> Bool
isLeadingChar c = c == '_' || isLetter c

isIdentifierChar :: Char -> Bool
isIdentifierChar c = c == '_' || isLetter c || isDigit c

isValidIdentifier :: String -> Bool
isValidIdentifier [c]    = isLeadingChar c
isValidIdentifier (c:cs) = isLeadingChar c && all isIdentifierChar cs
