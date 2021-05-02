{-# HLINT ignore "Redundant do" #-}
{-# HLINT ignore "Reduce duplication" #-}
module Ast.ExpressionSpec ( spec ) where

import Control.Applicative ( Alternative(empty) )
import Test.Hspec

import Ast.Expression ( Expression(..) )
import Ast.Identifier ( toIdentifier )
import Ast.Operator ( UnaryOperator(..)
                    , BinaryOperator(..)
                    , AssignmentOperator(..)
                    )
import Parser ( Parser, Parse(parse), tryParser )
import Pretty ( PrettyPrint(render) )

spec :: Spec
spec = do
    describe "Expression" $ do
        describe "Parse" $ do
            it "fails to parse empty expression" $ do
                let mResult = tryParser (parse :: Parser Expression) ""
                mResult `shouldBe` empty

            it "parses integer expression" $ do
                let mResult = tryParser (parse :: Parser Expression) "123"
                mResult `shouldBe` pure (Int64 123, read "")

            it "parses integer expression and leaves the rest" $ do
                let mResult = tryParser (parse :: Parser Expression) "123c"
                mResult `shouldBe` pure (Int64 123, read "c")

            it "parses unary expression with negation" $ do
                let mResult = tryParser (parse :: Parser Expression) "-123"
                mResult `shouldBe` pure (UnaryExpression Negation (Int64 123), read "")

            it "parses unary expression with bitwise complement" $ do
                let mResult = tryParser (parse :: Parser Expression) "~123"
                mResult `shouldBe` pure (UnaryExpression BitwiseComplement (Int64 123), read "")

            it "parses unary expression with logical negation" $ do
                let mResult = tryParser (parse :: Parser Expression) "!123"
                mResult `shouldBe` pure (UnaryExpression LogicalNegation (Int64 123), read "")

            it "parse binary expression with addition" $ do
                let mResult = tryParser (parse :: Parser Expression) "1+1"
                mResult `shouldBe` pure (BinaryExpression (Int64 1) Addition (Int64 1), read "")

            it "parse binary expression with subtraction" $ do
                let mResult = tryParser (parse :: Parser Expression) "1-1"
                mResult `shouldBe` pure (BinaryExpression (Int64 1) Subtraction (Int64 1), read "")

            it "parse binary expression with multiplication" $ do
                let mResult = tryParser (parse :: Parser Expression) "1*1"
                mResult `shouldBe` pure (BinaryExpression (Int64 1) Multiplication (Int64 1), read "")

            it "parse binary expression with division" $ do
                let mResult = tryParser (parse :: Parser Expression) "1/1"
                mResult `shouldBe` pure (BinaryExpression (Int64 1) Division (Int64 1), read "")

            it "parse binary expression with modulus" $ do
                let mResult = tryParser (parse :: Parser Expression) "1%1"
                mResult `shouldBe` pure (BinaryExpression (Int64 1) Modulus (Int64 1), read "")

            it "parse binary expression with bitwise and" $ do
                let mResult = tryParser (parse :: Parser Expression) "1&1"
                mResult `shouldBe` pure (BinaryExpression (Int64 1) BitwiseAnd (Int64 1), read "")

            it "parse binary expression with bitwise or" $ do
                let mResult = tryParser (parse :: Parser Expression) "1|1"
                mResult `shouldBe` pure (BinaryExpression (Int64 1) BitwiseOr (Int64 1), read "")

            it "parse binary expression with bitwise xor" $ do
                let mResult = tryParser (parse :: Parser Expression) "1^1"
                mResult `shouldBe` pure (BinaryExpression (Int64 1) BitwiseXor (Int64 1), read "")

            it "parse binary expression with bitwise shift left" $ do
                let mResult = tryParser (parse :: Parser Expression) "1<<1"
                mResult `shouldBe` pure (BinaryExpression (Int64 1) BitwiseShiftLeft (Int64 1), read "")

            it "parse binary expression with bitwise shift right" $ do
                let mResult = tryParser (parse :: Parser Expression) "1>>1"
                mResult `shouldBe` pure (BinaryExpression (Int64 1) BitwiseShiftRight (Int64 1), read "")

            it "parse complex binary arithmetic expression" $ do
                let mResult = tryParser (parse :: Parser Expression) "1    +  4 *(   3   - 1)/ 2"
                    exp1    = BinaryExpression (Int64 3) Subtraction (Int64 1)
                    exp2    = BinaryExpression (Int64 4) Multiplication exp1
                    exp3    = BinaryExpression exp2 Division (Int64 2)
                    exp4    = BinaryExpression (Int64 1) Addition exp3
                mResult `shouldBe` pure (exp4, read "")

            it "parse binary expression with less than equals" $ do
                let mResult = tryParser (parse :: Parser Expression) "1<=2"
                mResult `shouldBe` pure (BinaryExpression (Int64 1) LessThanEquals (Int64 2), read "")

            it "parse binary expression with greater than equals" $ do
                let mResult = tryParser (parse :: Parser Expression) "1>=2"
                mResult `shouldBe` pure (BinaryExpression (Int64 1) GreaterThanEquals (Int64 2), read "")

            it "parse binary expression with less than" $ do
                let mResult = tryParser (parse :: Parser Expression) "1<2"
                mResult `shouldBe` pure (BinaryExpression (Int64 1) LessThan (Int64 2), read "")

            it "parse binary expression with greater than" $ do
                let mResult = tryParser (parse :: Parser Expression) "1>2"
                mResult `shouldBe` pure (BinaryExpression (Int64 1) GreaterThan (Int64 2), read "")

            it "parse binary expression with equals" $ do
                let mResult = tryParser (parse :: Parser Expression) "1==2"
                mResult `shouldBe` pure (BinaryExpression (Int64 1) Equals (Int64 2), read "")

            it "parse binary expression with not equals" $ do
                let mResult = tryParser (parse :: Parser Expression) "1!=2"
                mResult `shouldBe` pure (BinaryExpression (Int64 1) NotEquals (Int64 2), read "")

            it "parses binary expression with logical and" $ do
                let mResult = tryParser (parse :: Parser Expression) "1&&2"
                mResult `shouldBe` pure (BinaryExpression (Int64 1) LogicalAnd (Int64 2), read "")

            it "parses binary expression with logical or" $ do
                let mResult = tryParser (parse :: Parser Expression) "1||2"
                mResult `shouldBe` pure (BinaryExpression (Int64 1) LogicalOr (Int64 2), read "")

            it "parse complex binary expression with relationals and logicals" $ do
                let mResult = tryParser (parse :: Parser Expression) "1 + (2 > 3) == 4 && 5 <= 6 || 7"
                    exp1    = BinaryExpression (Int64 2) GreaterThan (Int64 3)
                    exp2    = BinaryExpression (Int64 1) Addition exp1
                    exp3    = BinaryExpression exp2 Equals (Int64 4)
                    exp4    = BinaryExpression (Int64 5) LessThanEquals (Int64 6)
                    exp5    = BinaryExpression exp3 LogicalAnd exp4
                    exp6    = BinaryExpression exp5 LogicalOr (Int64 7)
                mResult `shouldBe` pure (exp6, read "")

            it "parses variable expression" $ do
                let mResult = tryParser (parse :: Parser Expression) "x"
                mResult `shouldBe` pure (Variable (toIdentifier "x"), read "")

            it "parses variable expression and leaves the rest" $ do
                let mResult = tryParser (parse :: Parser Expression) "x 123"
                mResult `shouldBe` pure (Variable (toIdentifier "x"), read " 123")

            it "parses assignment expression" $ do
                let mResult = tryParser (parse :: Parser Expression) "x = 1 * (2 + 3)"
                    exp1 = BinaryExpression (Int64 2) Addition (Int64 3)
                    exp2 = BinaryExpression (Int64 1) Multiplication exp1
                    exp3 = AssignmentExpression (toIdentifier "x") Assignment exp2
                mResult `shouldBe` pure (exp3, read "")

        describe "PrettyPrint" $ do
            it "should render integer expression" $ do
                let expression = Int64 124
                render expression `shouldBe` "124"

            it "should render unary expression" $ do
                let expression = UnaryExpression LogicalNegation $ Int64 124
                render expression `shouldBe` "!124"

            it "should render binary expression" $ do
                let expression = BinaryExpression (Int64 124) Addition (Int64 456)
                render expression `shouldBe` "(124+456)"

            it "should render complex binary expression" $ do
                let expression1 = BinaryExpression (Int64 3) Subtraction (Int64 1)
                    expression2 = BinaryExpression (Int64 4) Multiplication expression1
                    expression3 = BinaryExpression expression2 Division (Int64 2)
                    expression4 = BinaryExpression (Int64 1) Addition expression3
                render expression4 `shouldBe` "(1+((4*(3-1))/2))"

            it "should render variable expression" $ do
                let expression = Variable $ toIdentifier "x"
                render expression `shouldBe` "x"

            it "should render assignment expression" $ do
                let expression1 = BinaryExpression (Int64 124) Addition (Int64 456)
                    expression2 = AssignmentExpression (toIdentifier "x") Assignment expression1
                render expression2 `shouldBe` "x = (124+456)"
