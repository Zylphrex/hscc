module Ast.Operator ( UnaryOperator(..)
                    , BinaryOperator(..)
                    ) where

import Control.Applicative ( Alternative((<|>)) )
import Text.PrettyPrint ( char, text )

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

data BinaryOperator = Multiplication
                    | Division
                    | Addition
                    | Subtraction
                    | LessThanEquals
                    | GreaterThanEquals
                    | LessThan
                    | GreaterThan
                    | Equals
                    | NotEquals
    deriving (Eq, Show)

instance PrettyPrint BinaryOperator where
    prettyPrint Multiplication    = char '*'
    prettyPrint Division          = char '/'
    prettyPrint Addition          = char '+'
    prettyPrint Subtraction       = char '-'
    prettyPrint LessThanEquals    = text "<="
    prettyPrint GreaterThanEquals = text ">="
    prettyPrint LessThan          = char '<'
    prettyPrint GreaterThan       = char '>'
    prettyPrint Equals            = text "=="
    prettyPrint NotEquals         = text "!="
