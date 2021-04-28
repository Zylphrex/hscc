module Ast.Expression ( Expression(..) ) where

import Control.Applicative ( Alternative((<|>), many) )
import Control.Monad ( when )
import Control.Monad.State ( get, put )
import Data.Char ( isDigit )
import Data.Int ( Int64 )
import Data.Maybe ( fromJust, isNothing )
import Text.PrettyPrint ( char, equals, parens, space, text )

import Ast.Operator ( UnaryOperator(..), BinaryOperator(..) )
import Compiler ( Compiler(Compiler)
                , Compile(compile)
                , runCompiler
                , n
                , stackFrame
                , getOffset
                )
import Ast.Identifier ( Identifier, fromIdentifier )
import Parser ( Parse(parse)
              , Parser
              , parseCharacter
              , parseNotNull
              , parseSpaces
              , parseString
              , parseWhile
              )
import Pretty ( PrettyPrint(prettyPrint) )

data Expression = Int64 Int64
                | UnaryExpression UnaryOperator Expression
                | BinaryExpression Expression BinaryOperator Expression
                | Assignment Identifier Expression
                | Variable Identifier
    deriving (Eq, Show)

instance Parse Expression where
    parse = toExpression <$> (parse :: Parser RawExpression)

instance Compile Expression where
    compile (Int64 value) = Compiler $
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
    compile (BinaryExpression exp1 LogicalAnd exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        s <- get
        let i = n s
        let rhs = "_rhs_and" ++ show i
            end = "_end_and" ++ show i
        put $ s { n = i + 1 }
        return $ exp1'
              ++ [ "\tcmpq\t$0, %rax"
                 , "\tjne " ++ rhs
                 , "\tjmp " ++ end
                 , rhs ++ ":"
                 ]
              ++ exp2'
              ++ [ "\tcmpq\t$0, %rax"
                 , "\tmovq\t$0, %rax"
                 , "\tsetne\t%al"
                 , end ++ ":"
                 ]
    compile (BinaryExpression exp1 LogicalOr exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        s <- get
        let i = n s
        let rhs = "_rhs_or" ++ show i
            end = "_end_or" ++ show i
        put $ s { n = i + 1 }
        return $ exp1'
              ++ [ "\tcmpq\t$0, %rax"
                 , "\tje " ++ rhs
                 , "\tmovq\t$1, %rax"
                 , "\tjmp " ++ end
                 , rhs ++ ":"
                 ]
              ++ exp2'
              ++ [ "\tcmpq\t$0, %rax"
                 , "\tmovq\t$0, %rax"
                 , "\tsetne\t%al"
                 , end ++ ":"
                 ]
    compile (Assignment identifier exp) = Compiler $ do
        state <- get
        let identifier' = fromIdentifier identifier
            stackFrame' = stackFrame state
            stackOffset = getOffset identifier' stackFrame'
        when (isNothing stackOffset)
             (fail $ "Variable: " ++ identifier' ++ " is not declared")
        exp' <- runCompiler $ compile exp
        return $ exp'
              ++ [ "\tmovq\t%rax, " ++ show (fromJust stackOffset) ++ "(%rbp)" ]
    compile (Variable identifier) = Compiler $ do
        state <- get
        let identifier' = fromIdentifier identifier
            stackFrame' = stackFrame state
            stackOffset = getOffset identifier' stackFrame'
        when (isNothing stackOffset)
             (fail $ "Variable: " ++ identifier' ++ " is not declared")
        return [ "\tmovq\t" ++ show (fromJust stackOffset) ++ "(%rbp), %rax" ]

instance PrettyPrint Expression where
    prettyPrint (Int64 num) = text $ show num
    prettyPrint (UnaryExpression op exp) = prettyPrint op <> prettyPrint exp
    prettyPrint (BinaryExpression exp1 op exp2) =
        parens $ prettyPrint exp1 <> prettyPrint op <> prettyPrint exp2
    prettyPrint (Assignment identifier exp) =
        prettyPrint identifier <> space <> equals <> space <> prettyPrint exp
    prettyPrint (Variable identifier) = prettyPrint identifier

data RawExpression = RawAssignnmentExpressionWrapper RawAssignmentExpression
                   | RawLogicalOrExpressionWrapper RawLogicalOrExpression

instance Parse RawExpression where
    parse = RawAssignnmentExpressionWrapper <$> parse
        <|> RawLogicalOrExpressionWrapper <$> parse

instance Exp RawExpression where
    toExpression (RawAssignnmentExpressionWrapper exp) = toExpression exp
    toExpression (RawLogicalOrExpressionWrapper exp)  = toExpression exp

data RawAssignmentExpression = RawAssignmentExpression Identifier RawExpression

instance Parse RawAssignmentExpression where
    parse = RawAssignmentExpression <$> parse
                                    <*> (  parseSpaces
                                        *> parseCharacter '='
                                        *> parseSpaces
                                        *> parse
                                        <* parseSpaces
                                        )

instance Exp RawAssignmentExpression where
    toExpression (RawAssignmentExpression v exp) = Assignment v $ toExpression exp

data RawLogicalOrOperator = RawLogicalOr

instance Parse RawLogicalOrOperator where
    parse = RawLogicalOr <$ parseString "||"

instance BinaryOp RawLogicalOrOperator where
    toBinaryOperator RawLogicalOr = LogicalOr

newtype RawLogicalOrExpression = RawLogicalOrExpression (RawExp RawLogicalAndExpression RawLogicalOrOperator)

instance Parse RawLogicalOrExpression where
    parse = RawLogicalOrExpression <$> parse

instance Exp RawLogicalOrExpression where
    toExpression (RawLogicalOrExpression t) = toExpression t

data RawLogicalAndOperator = RawLogicalAnd

instance Parse RawLogicalAndOperator where
    parse = RawLogicalAnd <$ parseString "&&"

instance BinaryOp RawLogicalAndOperator where
    toBinaryOperator RawLogicalAnd = LogicalAnd

newtype RawLogicalAndExpression = RawLogicalAndExpression (RawExp RawEqualityExpression RawLogicalAndOperator)

instance Parse RawLogicalAndExpression where
    parse = RawLogicalAndExpression <$> parse

instance Exp RawLogicalAndExpression where
    toExpression (RawLogicalAndExpression t) = toExpression t

data RawEqualityOperator = RawEquals | RawNotEquals

instance Parse RawEqualityOperator where
    parse = RawEquals <$ parseString "=="
        <|> RawNotEquals <$ parseString "!="

instance BinaryOp RawEqualityOperator where
    toBinaryOperator RawEquals    = Equals
    toBinaryOperator RawNotEquals = NotEquals

newtype RawEqualityExpression = RawEqualityExpression (RawExp RawRelationalExpression RawEqualityOperator)

instance Parse RawEqualityExpression where
    parse = RawEqualityExpression <$> parse

instance Exp RawEqualityExpression where
    toExpression (RawEqualityExpression t) = toExpression t

data RawRelationalOperator = RawLessThanEquals
                           | RawGreaterThanEquals
                           | RawLessThan
                           | RawGreaterThan

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

instance Parse RawRelationalExpression where
    parse = RawRelationalExpression <$> parse

instance Exp RawRelationalExpression where
    toExpression (RawRelationalExpression t) = toExpression t

data RawAdditiveOperator = RawAddition | RawSubtraction

instance Parse RawAdditiveOperator where
    parse = RawAddition <$ parseCharacter '+'
        <|> RawSubtraction <$ parseCharacter '-'

instance BinaryOp RawAdditiveOperator where
    toBinaryOperator RawAddition    = Addition
    toBinaryOperator RawSubtraction = Subtraction

newtype RawAdditiveExpression = RawAdditiveExpression (RawExp RawTerm RawAdditiveOperator)

instance Parse RawAdditiveExpression where
    parse = RawAdditiveExpression <$> parse

instance Exp RawAdditiveExpression where
    toExpression (RawAdditiveExpression t) = toExpression t

data RawBinaryTermOperator = RawMultiplication | RawDivision

instance Parse RawBinaryTermOperator where
    parse = RawMultiplication <$ parseCharacter '*'
        <|> RawDivision <$ parseCharacter '/'

instance BinaryOp RawBinaryTermOperator where
    toBinaryOperator RawMultiplication = Multiplication
    toBinaryOperator RawDivision       = Division

newtype RawTerm = RawTerm (RawExp RawFactor RawBinaryTermOperator)

instance Parse RawTerm where
    parse = RawTerm <$> parse

instance Exp RawTerm where
    toExpression (RawTerm t) = toExpression t

data RawFactor = RawFactor RawExpression
               | UnaryRawFactor UnaryOperator RawFactor
               | IntegerRawFactor Int64
               | VariableRawFactor Identifier

instance Parse RawFactor where
    parse = RawFactor <$> (  parseCharacter '('
                          *> parseSpaces
                          *> parse
                          <* parseSpaces
                          <* parseCharacter ')'
                          )
        <|> UnaryRawFactor <$> parse <*> parse
        <|> IntegerRawFactor <$> (read <$> parseNotNull (parseWhile isDigit))
        <|> VariableRawFactor <$> parse

instance Exp RawFactor where
    toExpression (RawFactor exp)       = toExpression exp
    toExpression (UnaryRawFactor op f) = UnaryExpression op $ toExpression f
    toExpression (IntegerRawFactor x)  = Int64 x
    toExpression (VariableRawFactor v) = Variable v

data RawExp t o = RawExp t [(o, t)]

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
