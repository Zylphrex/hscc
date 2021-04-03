module ProgramParser where

import Control.Applicative ( Alternative((<|>), empty, many) )
import Data.Char ( isDigit, isLetter, isLower, isUpper, isSpace )
import Data.Functor (($>))

import Ast ( Program(Program)
           , Function(Function, returnType, identifier, body)
           , Type(Int)
           , Statement (Return)
           , Expression (Int32)
           )
import Parser ( Parser(Parser, runParser)
              , parseCharacter
              , parseIf
              , parseNotNull
              , parseWhile
              , parseSpaces
              , parseString
              )

parseProgram :: Parser Program
parseProgram = Program <$> parseFunction <* parseSpaces

parseFunction :: Parser Function
parseFunction = Function
    <$> (parseSpaces *> parseType)
    <*> (parseSpaces *> parseIdentifier)
    <*> (  parseSpaces
        *> parseCharacter '('
        *> parseSpaces
        $> () -- TODO: parse the arguments
        <* parseSpaces
        <* parseCharacter ')'
        <* parseSpaces
        )
    <*> (  parseCharacter '{'
        *> parseSpaces
        *> parseStatement
        <* parseSpaces
        <* parseCharacter '}'
        )

parseStatement :: Parser Statement
parseStatement = parseReturn

parseReturn :: Parser Statement
parseReturn = Return <$>
    (  parseSpaces
    *> parseString "return"
    *> parseSpaces
    *> parseExpression
    <* parseSpaces
    <* parseCharacter ';'
    )

parseExpression :: Parser Expression
parseExpression = Int32 <$> (read <$> parseNotNull (parseWhile isDigit))

parseType :: Parser Type
parseType = Int <$ parseString "int"

parseIdentifier :: Parser String 
parseIdentifier = (:) <$> parseIf isLeadingChar
                      <*> parseWhile isIdentifierChar
    where isLeadingChar c = c == '_' || isLetter c
          isIdentifierChar c = c == '_' || isLetter c || isDigit c