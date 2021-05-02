module Ast.Operator ( UnaryOperator(..)
                    , BinaryOperator(..)
                    , AssignmentOperator(..)
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
                    | Modulus
                    | Addition
                    | Subtraction
                    | BitwiseShiftLeft
                    | BitwiseShiftRight
                    | LessThanEquals
                    | GreaterThanEquals
                    | LessThan
                    | GreaterThan
                    | Equals
                    | NotEquals
                    | BitwiseOr
                    | BitwiseAnd
                    | BitwiseXor
                    | LogicalAnd
                    | LogicalOr
    deriving (Eq, Show)

instance PrettyPrint BinaryOperator where
    prettyPrint Multiplication    = char '*'
    prettyPrint Division          = char '/'
    prettyPrint Modulus           = char '%'
    prettyPrint Addition          = char '+'
    prettyPrint Subtraction       = char '-'
    prettyPrint BitwiseShiftLeft  = text "<<"
    prettyPrint BitwiseShiftRight = text ">>"
    prettyPrint LessThanEquals    = text "<="
    prettyPrint GreaterThanEquals = text ">="
    prettyPrint LessThan          = char '<'
    prettyPrint GreaterThan       = char '>'
    prettyPrint Equals            = text "=="
    prettyPrint NotEquals         = text "!="
    prettyPrint BitwiseOr         = char '|'
    prettyPrint BitwiseAnd        = char '&'
    prettyPrint BitwiseXor        = char '^'
    prettyPrint LogicalAnd        = text "&&"
    prettyPrint LogicalOr         = text "||"

data AssignmentOperator = Assignment
    deriving (Eq, Show)

instance Parse AssignmentOperator where
    parse = Assignment <$ parseCharacter '='

instance PrettyPrint AssignmentOperator where
    prettyPrint Assignment = char '='
