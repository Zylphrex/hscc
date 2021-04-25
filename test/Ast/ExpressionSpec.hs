{-# HLINT ignore "Redundant do" #-}
{-# HLINT ignore "Reduce duplication" #-}
module Ast.ExpressionSpec ( spec ) where

import Control.Applicative ( Alternative(empty) )
import Test.Hspec
import Text.PrettyPrint ( render )

import Ast.Expression ( Expression(..) )
import Ast.Operator ( UnaryOperator(..), BinaryOperator(..) )
import Compiler ( Compiler(Compiler)
                , Compile(compile)
                , executeCompiler
                )
import Parser ( Parser, Parse(parse), tryParser )
import Pretty ( PrettyPrint(prettyPrint) )

spec :: Spec
spec = do
    describe "Expression" $ do
        describe "Parse" $ do
            it "fails to parse empty expression" $ do
                let mResult = tryParser (parse :: Parser Expression) ""
                mResult `shouldBe` empty

            it "fails to parse bad expression" $ do
                let mResult = tryParser (parse :: Parser Expression) "cccc"
                mResult `shouldBe` empty

            it "parses integer expression" $ do
                let mResult = tryParser (parse :: Parser Expression) "123"
                mResult `shouldBe` pure (Int32 123, read "")

            it "parses integer expression and leaves the rest" $ do
                let mResult = tryParser (parse :: Parser Expression) "123c"
                mResult `shouldBe` pure (Int32 123, read "c")

            it "parses unary expression with negation" $ do
                let mResult = tryParser (parse :: Parser Expression) "-123"
                mResult `shouldBe` pure (UnaryExpression Negation (Int32 123), read "")

            it "parses unary expression with bitwise complement" $ do
                let mResult = tryParser (parse :: Parser Expression) "~123"
                mResult `shouldBe` pure (UnaryExpression BitwiseComplement (Int32 123), read "")

            it "parses unary expression with logical negation" $ do
                let mResult = tryParser (parse :: Parser Expression) "!123"
                mResult `shouldBe` pure (UnaryExpression LogicalNegation (Int32 123), read "")

            it "parse binary expression with addition" $ do
                let mResult = tryParser (parse :: Parser Expression) "1+1"
                mResult `shouldBe` pure (BinaryExpression (Int32 1) Addition (Int32 1), read "")

            it "parse binary expression with subtraction" $ do
                let mResult = tryParser (parse :: Parser Expression) "1-1"
                mResult `shouldBe` pure (BinaryExpression (Int32 1) Subtraction (Int32 1), read "")

            it "parse binary expression with multiplication" $ do
                let mResult = tryParser (parse :: Parser Expression) "1*1"
                mResult `shouldBe` pure (BinaryExpression (Int32 1) Multiplication (Int32 1), read "")

            it "parse binary expression with division" $ do
                let mResult = tryParser (parse :: Parser Expression) "1/1"
                mResult `shouldBe` pure (BinaryExpression (Int32 1) Division (Int32 1), read "")

            it "parse complex binary arithmetic expression" $ do
                let mResult = tryParser (parse :: Parser Expression) "1    +  4 *(   3   - 1)/ 2"
                    exp1    = BinaryExpression (Int32 3) Subtraction (Int32 1)
                    exp2    = BinaryExpression (Int32 4) Multiplication exp1
                    exp3    = BinaryExpression exp2 Division (Int32 2)
                    exp4    = BinaryExpression (Int32 1) Addition exp3
                mResult `shouldBe` pure (exp4, read "")

            it "parse binary expression with less than equals" $ do
                let mResult = tryParser (parse :: Parser Expression) "1<=2"
                mResult `shouldBe` pure (BinaryExpression (Int32 1) LessThanEquals (Int32 2), read "")

            it "parse binary expression with greater than equals" $ do
                let mResult = tryParser (parse :: Parser Expression) "1>=2"
                mResult `shouldBe` pure (BinaryExpression (Int32 1) GreaterThanEquals (Int32 2), read "")

            it "parse binary expression with less than" $ do
                let mResult = tryParser (parse :: Parser Expression) "1<2"
                mResult `shouldBe` pure (BinaryExpression (Int32 1) LessThan (Int32 2), read "")

            it "parse binary expression with greater than" $ do
                let mResult = tryParser (parse :: Parser Expression) "1>2"
                mResult `shouldBe` pure (BinaryExpression (Int32 1) GreaterThan (Int32 2), read "")

            it "parse binary expression with equals" $ do
                let mResult = tryParser (parse :: Parser Expression) "1==2"
                mResult `shouldBe` pure (BinaryExpression (Int32 1) Equals (Int32 2), read "")

            it "parse binary expression with not equals" $ do
                let mResult = tryParser (parse :: Parser Expression) "1!=2"
                mResult `shouldBe` pure (BinaryExpression (Int32 1) NotEquals (Int32 2), read "")

            it "parse complex binary expression with relationals" $ do
                let mResult = tryParser (parse :: Parser Expression) "1 + (2 > 3) == 4"
                    exp1    = BinaryExpression (Int32 2) GreaterThan (Int32 3)
                    exp2    = BinaryExpression (Int32 1) Addition exp1
                    exp3    = BinaryExpression exp2 Equals (Int32 4)
                mResult `shouldBe` pure (exp3, read "")

        -- describe "Compile" $ do
        --     undefined

        describe "PrettyPrint" $ do
            it "should render integer expression" $ do
                let expression = Int32 124
                render (prettyPrint expression) `shouldBe` "124"

            it "should render unary expression" $ do
                let expression = UnaryExpression LogicalNegation $ Int32 124
                render (prettyPrint expression) `shouldBe` "!124"

            it "should render binary expression" $ do
                let expression = BinaryExpression (Int32 124) Addition (Int32 456)
                render (prettyPrint expression) `shouldBe` "(124+456)"

            it "should render complex binary expression" $ do
                let expression1 = BinaryExpression (Int32 3) Subtraction (Int32 1)
                    expression2 = BinaryExpression (Int32 4) Multiplication expression1
                    expression3 = BinaryExpression expression2 Division (Int32 2)
                    expression4 = BinaryExpression (Int32 1) Addition expression3
                render (prettyPrint expression4) `shouldBe` "(1+((4*(3-1))/2))"
