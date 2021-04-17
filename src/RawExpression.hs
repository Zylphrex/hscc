module RawExpression where

import Data.Int ( Int32 )

import Ast ( Expression(Int32, UnaryExpression, BinaryExpression)
           , BinaryOperator
           , UnaryOperator
           )

class Exp a where
    asExpression :: a -> Expression

data RawExpression = RawExpression RawTerm [(BinaryOperator, RawTerm)]
    deriving Show

instance Exp RawExpression where
    asExpression (RawExpression t []) = asExpression t
    asExpression (RawExpression t ts) = foldl g (asExpression t) ts
        where g exp (op, t) = BinaryExpression exp op $ asExpression t

data RawTerm = RawTerm RawFactor [(BinaryOperator, RawFactor)]
    deriving Show

instance Exp RawTerm where
    asExpression (RawTerm f []) = asExpression f
    asExpression (RawTerm f fs) = foldl g (asExpression f) fs
        where g exp (op, f) = BinaryExpression exp op $ asExpression f

data RawFactor = RawFactor RawExpression
               | UnaryRawFactor UnaryOperator RawFactor
               | IntegerRawFactor Int32
    deriving Show

instance Exp RawFactor where
    asExpression (RawFactor exp)       = asExpression exp
    asExpression (UnaryRawFactor op f) = UnaryExpression op $ asExpression f
    asExpression (IntegerRawFactor x)  = Int32 x
