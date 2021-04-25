module Ast.Identifier ( Identifier, toIdentifier ) where

import Control.Monad.State ( get )
import Data.Char ( isDigit, isLetter )
import Text.PrettyPrint ( text )

import Compiler ( Compiler(Compiler)
                , Compile(compile)
                , Os(Darwin, Other)
                , os
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

instance Compile Identifier where
    compile (Identifier identifier) = Compiler $ do
        s <- get
        let identifier' = case os s of
                              Darwin -> '_' : identifier
                              Other  -> identifier
        return [identifier']

instance PrettyPrint Identifier  where
    prettyPrint (Identifier identifier) = text identifier

isLeadingChar :: Char -> Bool
isLeadingChar c = c == '_' || isLetter c

isIdentifierChar :: Char -> Bool
isIdentifierChar c = c == '_' || isLetter c || isDigit c

isValidIdentifier :: String -> Bool
isValidIdentifier [c]    = isLeadingChar c
isValidIdentifier (c:cs) = isLeadingChar c && all isIdentifierChar cs
isValidIdentifier []     = False
