module Ast.Operator ( UnaryOperator(..)
                    , BinaryOperator(..)
                    ) where

import Control.Applicative ( Alternative((<|>)) )
import Text.PrettyPrint ( char )

import Parser ( Parse(parse), parseCharacter )
import Pretty ( PrettyPrint(prettyPrint) )

data UnaryOperator = Negation
                   | BitwiseComplement
                   | LogicalNegation
    deriving Show

instance Parse UnaryOperator where
    parse = Negation <$ parseCharacter '-'
        <|> BitwiseComplement <$ parseCharacter '~'
        <|> LogicalNegation <$ parseCharacter '!'

instance Eq UnaryOperator where
    Negation          == Negation          = True
    BitwiseComplement == BitwiseComplement = True
    LogicalNegation   == LogicalNegation   = True
    _                 == _                 = False

instance PrettyPrint UnaryOperator where
    prettyPrint Negation          = char '-'
    prettyPrint BitwiseComplement = char '~'
    prettyPrint LogicalNegation   = char '!'

data BinaryOperator = Addition
                    | Subtraction
                    | Multiplication
                    | Division
    deriving Show

instance Eq BinaryOperator where
    Addition       == Addition       = True
    Subtraction    == Subtraction    = True
    Multiplication == Multiplication = True
    Division       == Division       = True
    _              == _              = False

instance PrettyPrint BinaryOperator where
    prettyPrint Addition       = char '+'
    prettyPrint Subtraction    = char '-'
    prettyPrint Multiplication = char '*'
    prettyPrint Division       = char '/'
