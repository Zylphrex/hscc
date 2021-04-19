module Ast.ExpressionSpec ( spec ) where

import Control.Applicative ( Alternative(empty) )
import Test.Hspec

import Ast.Expression ( Expression(..) )
import Ast.Operator ( UnaryOperator(..), BinaryOperator(..) )
import Parser ( Parser, Parse(parse), tryParser )

spec :: Spec
spec = describe "Expression" $ do
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

        it "parse complex binary expression" $ do
            let mResult = tryParser (parse :: Parser Expression) "1    +  4 *(   3   - 1)/ 2"
                exp1    = BinaryExpression (Int32 3) Subtraction (Int32 1)
                exp2    = BinaryExpression (Int32 4) Multiplication exp1
                exp3    = BinaryExpression exp2 Division (Int32 2)
                exp4    = BinaryExpression (Int32 1) Addition exp3
            mResult `shouldBe` pure (exp4, read "")
