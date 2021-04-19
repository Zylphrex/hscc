module Ast.Expression ( Expression(..) ) where

import Control.Applicative ( Alternative((<|>), many) )
import Data.Char ( isDigit )
import Data.Int ( Int32 )
import Text.PrettyPrint ( char, parens, text )

import Assembly ( Assembly(toAssembly), joinAssembly )
import Ast.Operator ( UnaryOperator(..), BinaryOperator(..) )
import Parser ( Parse(parse)
              , Parser
              , parseCharacter
              , parseNotNull
              , parseSpaces
              , parseWhile
              )
import Pretty ( PrettyPrint(prettyPrint) )

data Expression = Int32 Int32
                | UnaryExpression UnaryOperator Expression
                | BinaryExpression Expression BinaryOperator Expression
    deriving Show

instance Parse Expression where
    parse = toExpression <$> (parse :: Parser RawExpression)

instance Eq Expression where
    Int32 x == Int32 y = x == y
    UnaryExpression op1 exp1 == UnaryExpression op2 exp2 =
        op1 == op2 && exp1 == exp2
    BinaryExpression lhs1 op1 rhs1 == BinaryExpression lhs2 op2 rhs2 =
        lhs1 == lhs2 && op1 == op2 && rhs1 == rhs2
    _ == _ = False

instance Assembly Expression where
    toAssembly _ (Int32 value) =
        joinAssembly [ "\tmovq\t$" ++ show value ++ ", %rax" ]
    toAssembly opt (UnaryExpression Negation exp) =
        joinAssembly [ toAssembly opt exp
                     , "\tnegq\t%rax"
                     ]
    toAssembly opt (UnaryExpression BitwiseComplement exp) =
        joinAssembly [ toAssembly opt exp
                     , "\tnotq\t%rax"
                     ]
    toAssembly opt (UnaryExpression LogicalNegation exp) =
        joinAssembly [ toAssembly opt exp
                     , "\tcmpq\t$0, %rax"
                     , "\tmovq\t$0, %rax"
                     , "\tsete\t%al"
                     ]
    toAssembly opt (BinaryExpression exp1 Addition exp2) =
        joinAssembly [ toAssembly opt exp1
                     , "\tpush\t%rax"
                     , toAssembly opt exp2
                     , "\tpop\t%rcx"
                     , "\taddq\t%rcx, %rax"
                     ]
    toAssembly opt (BinaryExpression exp1 Subtraction exp2) =
        joinAssembly [ toAssembly opt exp2
                     , "\tpush\t%rax"
                     , toAssembly opt exp1
                     , "\tpop\t%rcx"
                     , "\tsubq\t%rcx, %rax"
                     ]
    toAssembly opt (BinaryExpression exp1 Multiplication exp2) =
        joinAssembly [ toAssembly opt exp1
                     , "\tpush\t%rax"
                     , toAssembly opt exp2
                     , "\tpop\t%rcx"
                     , "\timulq\t%rcx, %rax"
                     ]
    toAssembly opt (BinaryExpression exp1 Division exp2) =
        joinAssembly [ toAssembly opt exp2
                     , "\tpush\t%rax"
                     , toAssembly opt exp1
                     , "\tcqto"
                     , "\tpop\t%rcx"
                     , "\tidivq\t%rcx"
                     ]

instance PrettyPrint Expression where
    prettyPrint (Int32 num) = text $ show num
    prettyPrint (UnaryExpression op exp) = prettyPrint op <> prettyPrint exp
    prettyPrint (BinaryExpression exp1 op exp2) =
        parens $ prettyPrint exp1 <> prettyPrint op <> prettyPrint exp2

data RawBinaryOperatorP2 = RawAddition | RawSubtraction
    deriving Show

instance Parse RawBinaryOperatorP2 where
    parse = RawAddition <$ parseCharacter '+'
        <|> RawSubtraction <$ parseCharacter '-'

instance BinaryOp RawBinaryOperatorP2 where
    toBinaryOperator RawAddition    = Addition
    toBinaryOperator RawSubtraction = Subtraction

newtype RawExpression = RawExpression (RawExp RawTerm RawBinaryOperatorP2)
    deriving Show

instance Parse RawExpression where
    parse = RawExpression <$> parse

instance Exp RawExpression where
    toExpression (RawExpression t) = toExpression t

data RawBinaryOperatorP3 = RawMultiplication | RawDivision
    deriving Show

instance Parse RawBinaryOperatorP3 where
    parse = RawMultiplication <$ parseCharacter '*'
        <|> RawDivision <$ parseCharacter '/'

instance BinaryOp RawBinaryOperatorP3 where
    toBinaryOperator RawMultiplication = Multiplication
    toBinaryOperator RawDivision       = Division

newtype RawTerm = RawTerm (RawExp RawFactor RawBinaryOperatorP3)
    deriving Show

instance Parse RawTerm where
    parse = RawTerm <$> parse

instance Exp RawTerm where
    toExpression (RawTerm t) = toExpression t

data RawFactor = RawFactor RawExpression
               | UnaryRawFactor UnaryOperator RawFactor
               | IntegerRawFactor Int32
    deriving Show

instance Parse RawFactor where
    parse = RawFactor <$> (  parseCharacter '('
                          *> parseSpaces
                          *> parse
                          <* parseSpaces
                          <* parseCharacter ')'
                          )
        <|> UnaryRawFactor <$> parse <*> parse
        <|> IntegerRawFactor <$> (read <$> parseNotNull (parseWhile isDigit))

instance Exp RawFactor where
    toExpression (RawFactor exp)       = toExpression exp
    toExpression (UnaryRawFactor op f) = UnaryExpression op $ toExpression f
    toExpression (IntegerRawFactor x)  = Int32 x

data RawExp t o = RawExp t [(o, t)]
    deriving Show

class Exp a where
    toExpression :: a -> Expression

class BinaryOp a where
    toBinaryOperator :: a -> BinaryOperator

instance (Parse t, Parse o) => Parse (RawExp t o) where
    parse = RawExp <$> parse <*> many parseMany
        where parseMany = (,) <$> (parseSpaces *> parse <* parseSpaces) <*> parse

instance (Exp t, BinaryOp o) => Exp (RawExp t o) where
    toExpression (RawExp t []) = toExpression t
    toExpression (RawExp t ts) = foldl f (toExpression t) ts
        where f exp (op, t) = BinaryExpression exp (toBinaryOperator op) $ toExpression t
