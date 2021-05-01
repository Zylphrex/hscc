{-# HLINT ignore "Redundant do" #-}
{-# HLINT ignore "Reduce duplication" #-}
module Ast.OperatorSpec ( spec ) where

import Control.Applicative ( Alternative(empty) )
import Test.Hspec
import Text.PrettyPrint ( render )

import Ast.Operator ( UnaryOperator(..), BinaryOperator(..) )
import Parser ( Parser, Parse(parse), tryParser )
import Pretty ( PrettyPrint(prettyPrint) )

spec :: Spec
spec = do
    describe "Operator" $ do
        describe "Unary Operator" $ do
            describe "Parse" $ do
                it "fails to parse empty operator" $ do
                    let mResult = tryParser (parse :: Parser UnaryOperator) ""
                    mResult `shouldBe` empty

                it "fails to parse bad unary operator" $ do
                    let mResult = tryParser (parse :: Parser UnaryOperator) "$"
                    mResult `shouldBe` empty

                it "parse negation operator" $ do
                    let mResult = tryParser (parse :: Parser UnaryOperator) "-"
                    mResult `shouldBe` pure (Negation, read "")

                it "parse negation operator and leaves the rest" $ do
                    let mResult = tryParser (parse :: Parser UnaryOperator) "-5"
                    mResult `shouldBe` pure (Negation, read "5")

                it "parse bitwise complement operator" $ do
                    let mResult = tryParser (parse :: Parser UnaryOperator) "~"
                    mResult `shouldBe` pure (BitwiseComplement, read "")

                it "parse bitwise complement operator and leaves the rest" $ do
                    let mResult = tryParser (parse :: Parser UnaryOperator) "~5"
                    mResult `shouldBe` pure (BitwiseComplement, read "5")

                it "parse logical negation operator" $ do
                    let mResult = tryParser (parse :: Parser UnaryOperator) "!"
                    mResult `shouldBe` pure (LogicalNegation, read "")

                it "parse bitwise complement operator and leaves the rest" $ do
                    let mResult = tryParser (parse :: Parser UnaryOperator) "!5"
                    mResult `shouldBe` pure (LogicalNegation, read "5")

            describe "PrettyPrint" $ do
                it "should render negation" $ do
                    render (prettyPrint Negation) `shouldBe` "-"

                it "should render bitwise complement" $ do
                    render (prettyPrint BitwiseComplement) `shouldBe` "~"

                it "should render logical negation" $ do
                    render (prettyPrint LogicalNegation) `shouldBe` "!"

        describe "Binary Operator" $ do
            describe "PrettyPrint" $ do
                it "should render multiplication" $ do
                    render (prettyPrint Multiplication) `shouldBe` "*"

                it "should render division" $ do
                    render (prettyPrint Division) `shouldBe` "/"

                it "should render modulus" $ do
                    render (prettyPrint Modulus) `shouldBe` "%"

                it "should render addition" $ do
                    render (prettyPrint Addition) `shouldBe` "+"

                it "should render subtraction" $ do
                    render (prettyPrint Subtraction) `shouldBe` "-"

                it "should render less than equals" $ do
                    render (prettyPrint LessThanEquals) `shouldBe` "<="

                it "should render less than" $ do
                    render (prettyPrint LessThan) `shouldBe` "<"

                it "should render greater than equals" $ do
                    render (prettyPrint GreaterThanEquals) `shouldBe` ">="

                it "should render greater than" $ do
                    render (prettyPrint GreaterThan) `shouldBe` ">"

                it "should render equals" $ do
                    render (prettyPrint Equals) `shouldBe` "=="

                it "should render not equals" $ do
                    render (prettyPrint NotEquals) `shouldBe` "!="

                it "should render logical and" $ do
                    render (prettyPrint LogicalAnd) `shouldBe` "&&"

                it "should render logical or" $ do
                    render (prettyPrint LogicalOr) `shouldBe` "||"

                it "should render bitwise and" $ do
                    render (prettyPrint BitwiseAnd) `shouldBe` "&"

                it "should render bitwise or" $ do
                    render (prettyPrint BitwiseOr) `shouldBe` "|"

                it "should render bitwise xor" $ do
                    render (prettyPrint BitwiseXor) `shouldBe` "^"

                it "should render bitwise shift left" $ do
                    render (prettyPrint BitwiseShiftLeft) `shouldBe` "<<"

                it "should render bitwise shift right" $ do
                    render (prettyPrint BitwiseShiftRight) `shouldBe` ">>"
