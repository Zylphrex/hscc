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
    deriving (Eq, Show)

instance Parse UnaryOperator where
    parse = Negation <$ parseCharacter '-'
        <|> BitwiseComplement <$ parseCharacter '~'
        <|> LogicalNegation <$ parseCharacter '!'

instance PrettyPrint UnaryOperator where
    prettyPrint Negation          = char '-'
    prettyPrint BitwiseComplement = char '~'
    prettyPrint LogicalNegation   = char '!'

data BinaryOperator = Addition
                    | Subtraction
                    | Multiplication
                    | Division
    deriving (Eq, Show)

instance PrettyPrint BinaryOperator where
    prettyPrint Addition       = char '+'
    prettyPrint Subtraction    = char '-'
    prettyPrint Multiplication = char '*'
    prettyPrint Division       = char '/'
