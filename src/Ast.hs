module Ast where

import Data.Int ( Int32 )

newtype Program = Program Function
    deriving Show

instance Eq Program where
    Program p1 == Program p2 = p1 == p2

data Function = Function
    { returnType :: Type
    , identifier :: String
    , arguments  :: ()
    , body       :: Statement
    } deriving Show

instance Eq Function where
    Function r1 i1 a1 b1 == Function r2 i2 a2 b2 =
        and [r1 == r2, i1 == i2, a1 == a2, b1 == b2]

data Type = Int
    deriving Show

instance Eq Type where
    Int == Int = True

newtype Statement = Return Expression
    deriving Show

instance Eq Statement where
    Return e1 == Return e2 = e1 == e2

data Expression = Int32 Int32
                | UnaryExpression UnaryOperator Expression
                | BinaryExpression Expression BinaryOperator Expression
    deriving Show

instance Eq Expression where
    Int32 x == Int32 y = x == y
    UnaryExpression op1 exp1 == UnaryExpression op2 exp2 =
        op1 == op2 && exp1 == exp2
    BinaryExpression lhs1 op1 rhs1 == BinaryExpression lhs2 op2 rhs2 =
        lhs1 == lhs2 && op1 == op2 && rhs1 == rhs2
    _ == _ = False

data UnaryOperator = Negation
                   | BitwiseComplement
                   | LogicalNegation
    deriving Show

instance Eq UnaryOperator where
    Negation          == Negation          = True
    BitwiseComplement == BitwiseComplement = True
    LogicalNegation   == LogicalNegation   = True
    _                 == _                 = False

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
