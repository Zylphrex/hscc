module ProgramParser where

import Control.Applicative ( Alternative((<|>), empty, many) )
import Data.Char ( isDigit, isLetter, isLower, isUpper, isSpace )
import Data.Functor (($>))

import Ast ( Program(Program)
           , Function(Function, returnType, identifier, body)
           , Type(Int)
           , Statement(Return)
           , Expression(Int32, UnaryExpression)
           , UnaryOperator(Negation, BitwiseComplement, LogicalNegation)
           , BinaryOperator(Addition, Subtraction, Multiplication, Division)
           )
import Parser ( Parser(Parser, runParser)
              , parseCharacter
              , parseIf
              , parseNotNull
              , parseWhile
              , parseSpaces
              , parseString
              )
import RawExpression ( RawExpression(RawExpression)
                     , RawTerm(RawTerm)
                     , RawFactor(RawFactor, UnaryRawFactor, IntegerRawFactor)
                     , asExpression
                     )

parseProgram :: Parser Program
parseProgram = Program <$> parseFunction <* parseSpaces

parseFunction :: Parser Function
parseFunction = Function
    <$> (parseSpaces *> parseType)
    <*> (parseNotNull parseSpaces *> parseIdentifier)
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
parseExpression = asExpression <$> parseRawExpression

parseRawExpression :: Parser RawExpression
parseRawExpression = RawExpression <$> parseRawTerm <*> many parseRawTerms
    where parseRawTerms = (,) <$> parseOperator <*> parseRawTerm
          parseOperator = parseSpaces *> (parseAddition <|> parseSubtraction) <* parseSpaces

parseRawTerm :: Parser RawTerm
parseRawTerm = RawTerm <$> parseRawFactor <*> many parseRawFactors
    where parseRawFactors = (,) <$> parseOperator <*> parseRawFactor
          parseOperator   = parseSpaces *> (parseMultiplication <|> parseDivision) <* parseSpaces

parseRawFactor :: Parser RawFactor
parseRawFactor =
        RawFactor <$> (  parseCharacter '('
                      *> parseSpaces
                      *> parseRawExpression
                      <* parseSpaces
                      <* parseCharacter ')'
                      )
    <|> UnaryRawFactor <$> parseUnaryOperator <*> parseRawFactor
    <|> IntegerRawFactor <$> (read <$> parseNotNull (parseWhile isDigit))

parseUnaryOperator :: Parser UnaryOperator
parseUnaryOperator = Negation <$ parseCharacter '-'
    <|> BitwiseComplement <$ parseCharacter '~'
    <|> LogicalNegation <$ parseCharacter '!'

parseAddition :: Parser BinaryOperator
parseAddition = Addition <$ parseCharacter '+'

parseSubtraction :: Parser BinaryOperator
parseSubtraction = Subtraction <$ parseCharacter '-'

parseMultiplication :: Parser BinaryOperator
parseMultiplication = Multiplication <$ parseCharacter '*'

parseDivision :: Parser BinaryOperator
parseDivision = Division <$ parseCharacter '/'

parseType :: Parser Type
parseType = Int <$ parseString "int"

parseIdentifier :: Parser String 
parseIdentifier = (:) <$> parseIf isLeadingChar
                      <*> parseWhile isIdentifierChar
    where isLeadingChar c = c == '_' || isLetter c
          isIdentifierChar c = c == '_' || isLetter c || isDigit c
