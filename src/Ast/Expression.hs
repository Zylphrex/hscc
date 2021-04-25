module Ast.Expression ( Expression(..) ) where

import Control.Applicative ( Alternative((<|>), many) )
import Data.Char ( isDigit )
import Data.Int ( Int32 )
import Text.PrettyPrint ( char, parens, text )

import Ast.Operator ( UnaryOperator(..), BinaryOperator(..) )
import Compiler ( Compiler(Compiler)
                , Compile(compile)
                , runCompiler
                )
import Parser ( Parse(parse)
              , Parser
              , parseCharacter
              , parseNotNull
              , parseSpaces
              , parseString
              , parseWhile
              )
import Pretty ( PrettyPrint(prettyPrint) )

data Expression = Int32 Int32
                | UnaryExpression UnaryOperator Expression
                | BinaryExpression Expression BinaryOperator Expression
    deriving (Eq, Show)

instance Parse Expression where
    parse = toExpression <$> (parse :: Parser RawExpression)

instance Compile Expression where
    compile (Int32 value) = Compiler $
        return [ "\tmovq\t$" ++ show value ++ ", %rax" ]
    compile (UnaryExpression Negation exp) = Compiler $ do
        exp' <- runCompiler $ compile exp
        return $ exp' ++ [ "\tnegq\t%rax" ]
    compile (UnaryExpression BitwiseComplement exp) = Compiler $ do
        exp' <- runCompiler $ compile exp
        return $ exp' ++ [ "\tnotq\t%rax" ]
    compile (UnaryExpression LogicalNegation exp) = Compiler $ do
        exp' <- runCompiler $ compile exp
        return $ exp'
              ++ [ "\tcmpq\t$0, %rax"
                 , "\tmovq\t$0, %rax"
                 , "\tsete\t%al"
                 ]
    compile (BinaryExpression exp1 Addition exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp1'
              ++ [ "\tpush\t%rax" ]
              ++ exp2'
              ++ [ "\tpop\t%rcx"
                 , "\taddq\t%rcx, %rax"
                 ]
    compile (BinaryExpression exp1 Subtraction exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp2'
              ++ [ "\tpush\t%rax" ]
              ++ exp1'
              ++ [ "\tpop\t%rcx"
                 , "\tsubq\t%rcx, %rax"
                 ]
    compile (BinaryExpression exp1 Multiplication exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp1'
              ++ ["\tpush\t%rax"]
              ++ exp2'
              ++ [ "\tpop\t%rcx"
                 , "\timulq\t%rcx, %rax"
                 ]
    compile (BinaryExpression exp1 Division exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp2'
              ++ ["\tpush\t%rax"]
              ++ exp1'
              ++ [ "\tcqto"
                 , "\tpop\t%rcx"
                 , "\tidivq\t%rcx"
                 ]
    compile (BinaryExpression exp1 LessThanEquals exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp1'
              ++ ["\tpush\t%rax"]
              ++ exp2'
              ++ [ "\tpop\t%rcx"
                 , "\tcmpq\t%rax, %rcx"
                 , "\tmovq\t$0, %rax"
                 , "\tsetle\t%al"
                 ]
    compile (BinaryExpression exp1 GreaterThanEquals exp2) = Compiler $  do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp1'
              ++ ["\tpush\t%rax"]
              ++ exp2'
              ++ [ "\tpop\t%rcx"
                 , "\tcmpq\t%rax, %rcx"
                 , "\tmovq\t$0, %rax"
                 , "\tsetge\t%al"
                 ]
    compile (BinaryExpression exp1 LessThan exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp1'
              ++ ["\tpush\t%rax"]
              ++ exp2'
              ++ [ "\tpop\t%rcx"
                 , "\tcmpq\t%rax, %rcx"
                 , "\tmovq\t$0, %rax"
                 , "\tsetl\t%al"
                 ]
    compile (BinaryExpression exp1 GreaterThan exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp1'
              ++ ["\tpush\t%rax"]
              ++ exp2'
              ++ [ "\tpop\t%rcx"
                 , "\tcmpq\t%rax, %rcx"
                 , "\tmovq\t$0, %rax"
                 , "\tsetg\t%al"
                 ]
    compile (BinaryExpression exp1 Equals exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp1'
              ++ ["\tpush\t%rax"]
              ++ exp2'
              ++ [ "\tpop\t%rcx"
                 , "\tcmpq\t%rax, %rcx"
                 , "\tmovq\t$0, %rax"
                 , "\tsete\t%al"
                 ]
    compile (BinaryExpression exp1 NotEquals exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp1'
              ++ ["\tpush\t%rax"]
              ++ exp2'
              ++ [ "\tpop\t%rcx"
                 , "\tcmpq\t%rax, %rcx"
                 , "\tmovq\t$0, %rax"
                 , "\tsetne\t%al"
                 ]

instance PrettyPrint Expression where
    prettyPrint (Int32 num) = text $ show num
    prettyPrint (UnaryExpression op exp) = prettyPrint op <> prettyPrint exp
    prettyPrint (BinaryExpression exp1 op exp2) =
        parens $ prettyPrint exp1 <> prettyPrint op <> prettyPrint exp2

type RawExpression = RawEqualityExpression

data RawEqualityOperator = RawEquals | RawNotEquals
    deriving Show

instance Parse RawEqualityOperator where
    parse = RawEquals <$ parseString "=="
        <|> RawNotEquals <$ parseString "!="

instance BinaryOp RawEqualityOperator where
    toBinaryOperator RawEquals    = Equals
    toBinaryOperator RawNotEquals = NotEquals

newtype RawEqualityExpression = RawEqualityExpression (RawExp RawRelationalExpression RawEqualityOperator)
    deriving Show

instance Parse RawEqualityExpression where
    parse = RawEqualityExpression <$> parse

instance Exp RawEqualityExpression where
    toExpression (RawEqualityExpression t) = toExpression t

data RawRelationalOperator = RawLessThanEquals
                           | RawGreaterThanEquals
                           | RawLessThan
                           | RawGreaterThan
    deriving Show

instance Parse RawRelationalOperator where
    parse = RawLessThanEquals <$ parseString "<="
        <|> RawGreaterThanEquals <$ parseString ">="
        <|> RawLessThan <$ parseCharacter '<'
        <|> RawGreaterThan <$ parseCharacter '>'

instance BinaryOp RawRelationalOperator where
  toBinaryOperator RawLessThanEquals    = LessThanEquals
  toBinaryOperator RawGreaterThanEquals = GreaterThanEquals
  toBinaryOperator RawLessThan          = LessThan
  toBinaryOperator RawGreaterThan       = GreaterThan

newtype RawRelationalExpression = RawRelationalExpression (RawExp RawAdditiveExpression RawRelationalOperator)
    deriving Show

instance Parse RawRelationalExpression where
    parse = RawRelationalExpression <$> parse

instance Exp RawRelationalExpression where
    toExpression (RawRelationalExpression t) = toExpression t

data RawAdditiveOperator = RawAddition | RawSubtraction
    deriving Show

instance Parse RawAdditiveOperator where
    parse = RawAddition <$ parseCharacter '+'
        <|> RawSubtraction <$ parseCharacter '-'

instance BinaryOp RawAdditiveOperator where
    toBinaryOperator RawAddition    = Addition
    toBinaryOperator RawSubtraction = Subtraction

newtype RawAdditiveExpression = RawAdditiveExpression (RawExp RawTerm RawAdditiveOperator)
    deriving Show

instance Parse RawAdditiveExpression where
    parse = RawAdditiveExpression <$> parse

instance Exp RawAdditiveExpression where
    toExpression (RawAdditiveExpression t) = toExpression t

data RawBinaryTermOperator = RawMultiplication | RawDivision
    deriving Show

instance Parse RawBinaryTermOperator where
    parse = RawMultiplication <$ parseCharacter '*'
        <|> RawDivision <$ parseCharacter '/'

instance BinaryOp RawBinaryTermOperator where
    toBinaryOperator RawMultiplication = Multiplication
    toBinaryOperator RawDivision       = Division

newtype RawTerm = RawTerm (RawExp RawFactor RawBinaryTermOperator)
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
