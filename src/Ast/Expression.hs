module Ast.Expression ( Expression(..) ) where

import Control.Applicative ( Alternative((<|>), many) )
import Control.Monad ( mapM, when )
import Data.Char ( isDigit )
import Data.Int ( Int64 )
import Data.Maybe ( fromJust, isNothing )
import Text.PrettyPrint ( char, colon, equals, hcat, parens, space, text )

import Ast.Operator ( UnaryOperator(..)
                    , BinaryOperator(..)
                    , AssignmentOperator(..)
                    )
import Compiler ( Compiler(Compiler)
                , Compile(compile)
                , FunctionSignature(FunctionSignature)
                , argumentRegister
                , runCompiler
                , getFunctionSignature
                , getVariable
                , getSymbols
                , saveFunctionArgs
                , restoreFunctionArgs
                )
import Ast.Identifier ( Identifier, fromIdentifier )
import Ast.Type ( bytes, toType )
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
                | AssignmentExpression Identifier AssignmentOperator Expression
                | ConditionalExpression Expression Expression Expression
                | FunctionCallExpression Identifier [Expression]
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
              ++ [ "\tpop\t%rbx"
                 , "\taddq\t%rbx, %rax"
                 ]
    compile (BinaryExpression exp1 Subtraction exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp2'
              ++ [ "\tpush\t%rax" ]
              ++ exp1'
              ++ [ "\tpop\t%rbx"
                 , "\tsubq\t%rbx, %rax"
                 ]
    compile (BinaryExpression exp1 Multiplication exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp1'
              ++ ["\tpush\t%rax"]
              ++ exp2'
              ++ [ "\tpop\t%rbx"
                 , "\timulq\t%rbx, %rax"
                 ]
    compile (BinaryExpression exp1 Division exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp2'
              ++ ["\tpush\t%rax"]
              ++ exp1'
              ++ [ "\tcqto"
                 , "\tpop\t%rbx"
                 , "\tidivq\t%rbx"
                 ]
    compile (BinaryExpression exp1 Modulus exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp2'
              ++ ["\tpush\t%rax"]
              ++ exp1'
              ++ [ "\tcqto"
                 , "\tpop\t%rbx"
                 , "\tidivq\t%rbx"
                 , "\tmovq\t%rdx, %rax"
                 ]
    compile (BinaryExpression exp1 LessThanEquals exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp1'
              ++ ["\tpush\t%rax"]
              ++ exp2'
              ++ [ "\tpop\t%rbx"
                 , "\tcmpq\t%rax, %rbx"
                 , "\tmovq\t$0, %rax"
                 , "\tsetle\t%al"
                 ]
    compile (BinaryExpression exp1 GreaterThanEquals exp2) = Compiler $  do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp1'
              ++ ["\tpush\t%rax"]
              ++ exp2'
              ++ [ "\tpop\t%rbx"
                 , "\tcmpq\t%rax, %rbx"
                 , "\tmovq\t$0, %rax"
                 , "\tsetge\t%al"
                 ]
    compile (BinaryExpression exp1 LessThan exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp1'
              ++ ["\tpush\t%rax"]
              ++ exp2'
              ++ [ "\tpop\t%rbx"
                 , "\tcmpq\t%rax, %rbx"
                 , "\tmovq\t$0, %rax"
                 , "\tsetl\t%al"
                 ]
    compile (BinaryExpression exp1 GreaterThan exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp1'
              ++ ["\tpush\t%rax"]
              ++ exp2'
              ++ [ "\tpop\t%rbx"
                 , "\tcmpq\t%rax, %rbx"
                 , "\tmovq\t$0, %rax"
                 , "\tsetg\t%al"
                 ]
    compile (BinaryExpression exp1 Equals exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp1'
              ++ ["\tpush\t%rax"]
              ++ exp2'
              ++ [ "\tpop\t%rbx"
                 , "\tcmpq\t%rax, %rbx"
                 , "\tmovq\t$0, %rax"
                 , "\tsete\t%al"
                 ]
    compile (BinaryExpression exp1 NotEquals exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp1'
              ++ ["\tpush\t%rax"]
              ++ exp2'
              ++ [ "\tpop\t%rbx"
                 , "\tcmpq\t%rax, %rbx"
                 , "\tmovq\t$0, %rax"
                 , "\tsetne\t%al"
                 ]
    compile (BinaryExpression exp1 BitwiseAnd exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp1'
              ++ [ "\tpush\t%rax" ]
              ++ exp2'
              ++ [ "\tpop\t%rbx"
                 , "\tandq\t%rbx, %rax"
                 ]
    compile (BinaryExpression exp1 BitwiseXor exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp1'
              ++ [ "\tpush\t%rax" ]
              ++ exp2'
              ++ [ "\tpop\t%rbx"
                 , "\txorq\t%rbx, %rax"
                 ]
    compile (BinaryExpression exp1 BitwiseOr exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp1'
              ++ [ "\tpush\t%rax" ]
              ++ exp2'
              ++ [ "\tpop\t%rbx"
                 , "\torq\t%rbx, %rax"
                 ]
    compile (BinaryExpression exp1 BitwiseShiftLeft exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp2'
              ++ [ "\tpush\t%rax" ]
              ++ exp1'
              ++ [ "\tpop\t%rbx"
                 , "\tsalq\t%cl, %rax"
                 ]
    compile (BinaryExpression exp1 BitwiseShiftRight exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        return $ exp2'
              ++ [ "\tpush\t%rax" ]
              ++ exp1'
              ++ [ "\tpop\t%rbx"
                 , "\tsarq\t%cl, %rax"
                 ]
    compile (BinaryExpression exp1 LogicalAnd exp2) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        [rhs, end] <- getSymbols ["_rhs_and", "_end_and"]
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
        [rhs, end] <- getSymbols ["_rhs_or", "_end_or"]
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
    compile (AssignmentExpression identifier Assignment exp) = Compiler $ do
        let identifier' = fromIdentifier identifier
        register <- getVariable identifier'
        when (isNothing register)
             (fail $ "Variable: " ++ identifier' ++ " is not declared")
        exp' <- runCompiler $ compile exp
        return $ exp'
              ++ [ "\tmovq\t%rax, " ++ fromJust register ]
    compile (AssignmentExpression identifier operator exp) = Compiler $ do
        let binaryOperator = case operator of
                MultiplicationAssignment    -> Multiplication
                DivisionAssignment          -> Division
                ModulusAssignment           -> Modulus
                AdditionAssignment          -> Addition
                SubtractionAssignment       -> Subtraction
                BitwiseShiftLeftAssignment  -> BitwiseShiftLeft
                BitwiseShiftRightAssignment -> BitwiseShiftRight
                BitwiseOrAssignment         -> BitwiseOr
                BitwiseAndAssignment        -> BitwiseAnd
                BitwiseXorAssignment        -> BitwiseXor
            binaryExpression = BinaryExpression (Variable identifier) binaryOperator exp
            assignmentExpression = AssignmentExpression identifier Assignment binaryExpression
        runCompiler $ compile assignmentExpression
    compile (Variable identifier) = Compiler $ do
        let identifier' = fromIdentifier identifier
        register <- getVariable identifier'
        when (isNothing register)
             (fail $ "Variable: " ++ identifier' ++ " is not declared")
        return [ "\tmovq\t" ++ fromJust register ++ ", %rax" ]
    compile (ConditionalExpression exp1 exp2 exp3) = Compiler $ do
        exp1' <- runCompiler $ compile exp1
        exp2' <- runCompiler $ compile exp2
        exp3' <- runCompiler $ compile exp3
        [false, end] <- getSymbols ["_if_false", "_if_end"]
        return $ exp1'
              ++ [ "\tcmpq\t$0, %rax"
                 , "\tje " ++ false
                 ]
              ++ exp2'
              ++ [ "\tjmp " ++ end
                 , false ++ ":"
                 ]
              ++ exp3'
              ++ [ end ++ ":" ]
    compile (FunctionCallExpression identifier arguments) = Compiler $ do
        let identifier' = fromIdentifier identifier
        msignature <- getFunctionSignature identifier'
        when (isNothing msignature)
             (fail $ "Function: " ++ identifier' ++ " is not declared")
        let FunctionSignature _ _ argumentMetas = fromJust msignature
        when (length arguments /= length argumentMetas)
             (fail $ "Function: " ++ identifier' ++ " called with the wrong number of arguments")
        saveArgs <- saveFunctionArgs
        arguments' <- mapM pushArg $ reverse $ zip3 [0..] arguments argumentMetas
        identifier' <- runCompiler $ compile identifier
        restoreArgs <- restoreFunctionArgs
        return $ saveArgs
              ++ concat arguments'
              ++ [ "\tcallq " ++ head identifier' ]
              ++ restoreArgs
      where pushArg (index, argExp, (argType, argIdentifier)) = do
              argExp' <- runCompiler $ compile argExp
              if index >= 6
              then return $ argExp' ++ [ "\tpush\t%rax" ]
              else let register = argumentRegister index
                   in return $ argExp' ++ [ "\tmovq\t%rax, " ++ register]

instance PrettyPrint Expression where
    prettyPrint (Int64 num) = text $ show num
    prettyPrint (UnaryExpression op exp) = prettyPrint op <> prettyPrint exp
    prettyPrint (BinaryExpression exp1 op exp2) =
        parens $ prettyPrint exp1 <> prettyPrint op <> prettyPrint exp2
    prettyPrint (AssignmentExpression identifier op exp) =
        prettyPrint identifier <> space <> prettyPrint op <> space <> prettyPrint exp
    prettyPrint (ConditionalExpression exp1 exp2 exp3) =
        parens (prettyPrint exp1)
        <> space <> char '?' <> space
        <> parens (prettyPrint exp2)
        <> space <> colon <> space
        <> parens (prettyPrint exp3)
    prettyPrint (FunctionCallExpression identifier exps) =
        prettyPrint identifier <> parens (hcat $ map prettyPrint exps)
    prettyPrint (Variable identifier) = prettyPrint identifier

data RawExpression = RawAssignmentExpressionWrapper RawAssignmentExpression
                   | RawConditionalExpressionWrapper RawConditionalExpression

instance Parse RawExpression where
    parse = RawAssignmentExpressionWrapper <$> parse
        <|> RawConditionalExpressionWrapper <$> parse

instance Exp RawExpression where
    toExpression (RawAssignmentExpressionWrapper exp) = toExpression exp
    toExpression (RawConditionalExpressionWrapper exp) = toExpression exp

data RawAssignmentExpression = RawAssignmentExpression Identifier AssignmentOperator RawExpression

instance Parse RawAssignmentExpression where
    parse = RawAssignmentExpression <$> parse
                                    <*> (parseSpaces *> parse)
                                    <*> (parseSpaces *> parse <* parseSpaces)

instance Exp RawAssignmentExpression where
    toExpression (RawAssignmentExpression v op exp) = AssignmentExpression v op $ toExpression exp

data RawConditionalExpression = RawConditionalExpression RawLogicalOrExpression RawExpression RawConditionalExpression
                              | RawConditionalOrExpression RawLogicalOrExpression

instance Parse RawConditionalExpression where
    parse = RawConditionalExpression <$> parse
                                     <*> (parseSpaces *> parseCharacter '?' *> parseSpaces *> parse)
                                     <*> (parseSpaces *> parseCharacter ':' *> parseSpaces *> parse)
        <|> RawConditionalOrExpression <$> parse

instance Exp RawConditionalExpression where
    toExpression (RawConditionalExpression exp1 exp2 exp3) =
        ConditionalExpression (toExpression exp1) (toExpression exp2) (toExpression exp3)
    toExpression (RawConditionalOrExpression exp) = toExpression exp

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

newtype RawLogicalAndExpression = RawLogicalAndExpression (RawExp RawBitwiseOrExpression RawLogicalAndOperator)

instance Parse RawLogicalAndExpression where
    parse = RawLogicalAndExpression <$> parse

instance Exp RawLogicalAndExpression where
    toExpression (RawLogicalAndExpression t) = toExpression t

data RawBitwiseOrOperator = RawBitwiseOr

instance Parse RawBitwiseOrOperator where
    parse = RawBitwiseOr <$ parseCharacter '|'

instance BinaryOp RawBitwiseOrOperator where
    toBinaryOperator RawBitwiseOr = BitwiseOr

newtype RawBitwiseOrExpression = RawBitwiseOrExpression (RawExp RawBitwiseXorExpression RawBitwiseOrOperator)

instance Parse RawBitwiseOrExpression  where
    parse = RawBitwiseOrExpression <$> parse

instance Exp RawBitwiseOrExpression  where
    toExpression (RawBitwiseOrExpression t) = toExpression t

data RawBitwiseXorOperator = RawBitwiseXor

instance Parse RawBitwiseXorOperator where
    parse = RawBitwiseXor <$ parseCharacter '^'

instance BinaryOp RawBitwiseXorOperator where
    toBinaryOperator RawBitwiseXor = BitwiseXor

newtype RawBitwiseXorExpression = RawBitwiseXorExpression (RawExp RawBitwiseAndExpression RawBitwiseXorOperator)

instance Parse RawBitwiseXorExpression  where
    parse = RawBitwiseXorExpression <$> parse

instance Exp RawBitwiseXorExpression  where
    toExpression (RawBitwiseXorExpression t) = toExpression t

data RawBitwiseAndOperator = RawBitwiseAnd

instance Parse RawBitwiseAndOperator where
    parse = RawBitwiseAnd <$ parseCharacter '&'

instance BinaryOp RawBitwiseAndOperator where
    toBinaryOperator RawBitwiseAnd = BitwiseAnd

newtype RawBitwiseAndExpression = RawBitwiseAndExpression (RawExp RawEqualityExpression RawBitwiseAndOperator)

instance Parse RawBitwiseAndExpression  where
    parse = RawBitwiseAndExpression <$> parse

instance Exp RawBitwiseAndExpression  where
    toExpression (RawBitwiseAndExpression t) = toExpression t

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

newtype RawRelationalExpression = RawRelationalExpression (RawExp RawBitwiseShiftExpression RawRelationalOperator)

instance Parse RawRelationalExpression where
    parse = RawRelationalExpression <$> parse

instance Exp RawRelationalExpression where
    toExpression (RawRelationalExpression t) = toExpression t

data RawBitwiseShiftOperator = RawBitwiseShiftLeft | RawBitwiseShiftRight

instance Parse RawBitwiseShiftOperator where
    parse = RawBitwiseShiftLeft <$ parseString "<<"
        <|> RawBitwiseShiftRight <$ parseString ">>"

instance BinaryOp RawBitwiseShiftOperator where
    toBinaryOperator RawBitwiseShiftLeft  = BitwiseShiftLeft
    toBinaryOperator RawBitwiseShiftRight = BitwiseShiftRight

newtype RawBitwiseShiftExpression = RawBitwiseShiftExpression (RawExp RawAdditiveExpression RawBitwiseShiftOperator)

instance Parse RawBitwiseShiftExpression where
    parse = RawBitwiseShiftExpression <$> parse

instance Exp RawBitwiseShiftExpression where
    toExpression (RawBitwiseShiftExpression t) = toExpression t

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

data RawBinaryTermOperator = RawMultiplication | RawDivision | RawModulus

instance Parse RawBinaryTermOperator where
    parse = RawMultiplication <$ parseCharacter '*'
        <|> RawDivision <$ parseCharacter '/'
        <|> RawModulus <$ parseCharacter '%'

instance BinaryOp RawBinaryTermOperator where
    toBinaryOperator RawMultiplication = Multiplication
    toBinaryOperator RawDivision       = Division
    toBinaryOperator RawModulus        = Modulus

newtype RawTerm = RawTerm (RawExp RawFactor RawBinaryTermOperator)

instance Parse RawTerm where
    parse = RawTerm <$> parse

instance Exp RawTerm where
    toExpression (RawTerm t) = toExpression t

data RawFactor = RawFactor RawExpression
               | UnaryRawFactor UnaryOperator RawFactor
               | IntegerRawFactor Int64
               | FunctionCallRawFactor Identifier [RawExpression]
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
        <|> do
                identifier <- parse
                _ <- parseSpaces *> parseCharacter '('
                margument <- parseSpaces *> (Just <$> parse <|> pure Nothing)
                case margument of
                    Nothing -> do
                        _ <- parseSpaces *> parseCharacter ')'
                        return $ FunctionCallRawFactor identifier []
                    Just argument -> do
                        arguments <- many (  parseSpaces
                                          *> parseCharacter ','
                                          *> parseSpaces
                                          *> (parse :: Parser RawExpression)
                                          )
                        _ <- parseSpaces *> parseCharacter ')'
                        return $ FunctionCallRawFactor identifier (argument:arguments)
        <|> VariableRawFactor <$> parse

instance Exp RawFactor where
    toExpression (RawFactor exp)                = toExpression exp
    toExpression (UnaryRawFactor op f)          = UnaryExpression op $ toExpression f
    toExpression (IntegerRawFactor x)           = Int64 x
    toExpression (FunctionCallRawFactor i args) = FunctionCallExpression i $ map toExpression args
    toExpression (VariableRawFactor v)          = Variable v

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
