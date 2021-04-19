module Ast.OperatorSpec ( spec ) where

import Control.Applicative ( Alternative(empty) )
import Test.Hspec

import Ast.Operator ( UnaryOperator(..) )
import Parser ( Parser, Parse(parse), tryParser )

spec :: Spec
spec = describe "Operator" $ do
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
