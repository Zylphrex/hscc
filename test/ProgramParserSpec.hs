module ProgramParserSpec ( spec ) where

import Test.Hspec

import Control.Applicative ( Alternative(empty) )

import Ast ( Expression(Int32, UnaryExpression, BinaryExpression)
           , Function(Function)
           , Statement(Return)
           , Type(Int)
           , UnaryOperator(Negation, BitwiseComplement, LogicalNegation)
           , BinaryOperator(Addition, Subtraction, Multiplication, Division)
           , returnType
           , identifier
           , arguments
           , body
           )
import Parser ( tryParser )
import ProgramParser ( parseExpression
                     , parseFunction
                     , parseIdentifier
                     , parseStatement
                     , parseType
                     , parseUnaryOperator
                     )

spec :: Spec
spec = describe "ProgramParser" $ do
    describe "parseIdentifier" $ do
        it "fails when identifier is blank" $ do
            let parser  = parseIdentifier
                mResult = tryParser parser ""
            mResult `shouldBe` empty

        it "fails when identifier starts with space" $ do
            let parser  = parseIdentifier
                mResult = tryParser parser " "
            mResult `shouldBe` empty

        it "fails when identifier starts with unexpected symbol" $ do
            let parser  = parseIdentifier
                mResult = tryParser parser "$"
            mResult `shouldBe` empty

        it "fails when identifier starts with digit" $ do
            let parser  = parseIdentifier
                mResult = tryParser parser "1"
            mResult `shouldBe` empty

        it "parses when identifier is single letter" $ do
            let parser  = parseIdentifier
                mResult = tryParser parser "c"
            mResult `shouldBe` pure ("c", read "")

        it "parses when identifier is single letter and leaves the rest" $ do
            let parser  = parseIdentifier
                mResult = tryParser parser "c$"
            mResult `shouldBe` pure ("c", read "$")

        it "parses when identifier is single underscore" $ do
            let parser  = parseIdentifier
                mResult = tryParser parser "_"
            mResult `shouldBe` pure ("_", read "")

        it "parses when identifier is single underscore and leaves the rest" $ do
            let parser  = parseIdentifier
                mResult = tryParser parser "_$"
            mResult `shouldBe` pure ("_", read "$")

        it "parses when identifier is complex" $ do
            let parser  = parseIdentifier
                mResult = tryParser parser "aB_1"
            mResult `shouldBe` pure ("aB_1", read "")

        it "parses when identifier is complex and leaves the rest" $ do
            let parser  = parseIdentifier
                mResult = tryParser parser "aB_1$"
            mResult `shouldBe` pure ("aB_1", read "$")

    describe "parseUnaryOperator" $ do
        it "fails to parse empty operator" $ do
            let parser  = parseUnaryOperator
                mResult = tryParser parser ""
            mResult `shouldBe` empty

        it "fails to parse bad unary operator" $ do
            let parser  = parseUnaryOperator
                mResult = tryParser parser "$"
            mResult `shouldBe` empty

        it "parse negation operator" $ do
            let parser  = parseUnaryOperator
                mResult = tryParser parser "-"
            mResult `shouldBe` pure (Negation, read "")

        it "parse negation operator and leaves the rest" $ do
            let parser  = parseUnaryOperator
                mResult = tryParser parser "-5"
            mResult `shouldBe` pure (Negation, read "5")

        it "parse bitwise complement operator" $ do
            let parser  = parseUnaryOperator
                mResult = tryParser parser "~"
            mResult `shouldBe` pure (BitwiseComplement, read "")

        it "parse bitwise complement operator and leaves the rest" $ do
            let parser  = parseUnaryOperator
                mResult = tryParser parser "~5"
            mResult `shouldBe` pure (BitwiseComplement, read "5")

        it "parse logical negation operator" $ do
            let parser  = parseUnaryOperator
                mResult = tryParser parser "!"
            mResult `shouldBe` pure (LogicalNegation, read "")

        it "parse bitwise complement operator and leaves the rest" $ do
            let parser  = parseUnaryOperator
                mResult = tryParser parser "!5"
            mResult `shouldBe` pure (LogicalNegation, read "5")

    describe "parseType" $ do
        it "fails to parse empty type" $ do
            let parser  = parseType
                mResult = tryParser parser ""
            mResult `shouldBe` empty

        it "fails to parse bad type" $ do
            let parser  = parseType
                mResult = tryParser parser "stuff"
            mResult `shouldBe` empty

        it "parse int type" $ do
            let parser  = parseType
                mResult = tryParser parser "int"
            mResult `shouldBe` pure (Int, read "")

        it "parse int type and leaves the rest" $ do
            let parser  = parseType
                mResult = tryParser parser "intx"
            mResult `shouldBe` pure (Int, read "x")

    describe "parseExpression" $ do
        it "fails to parse empty expression" $ do
            let parser  = parseExpression
                mResult = tryParser parser ""
            mResult `shouldBe` empty

        it "fails to parse bad expression" $ do
            let parser  = parseExpression
                mResult = tryParser parser "cccc"
            mResult `shouldBe` empty

        it "parses integer expression" $ do
            let parser  = parseExpression
                mResult = tryParser parser "123"
            mResult `shouldBe` pure (Int32 123, read "")

        it "parses integer expression and leaves the rest" $ do
            let parser  = parseExpression
                mResult = tryParser parser "123c"
            mResult `shouldBe` pure (Int32 123, read "c")

        it "parses unary expression with negation" $ do
            let parser  = parseExpression
                mResult = tryParser parser "-123"
            mResult `shouldBe` pure (UnaryExpression Negation (Int32 123), read "")

        it "parses unary expression with bitwise complement" $ do
            let parser  = parseExpression
                mResult = tryParser parser "~123"
            mResult `shouldBe` pure (UnaryExpression BitwiseComplement (Int32 123), read "")

        it "parses unary expression with logical negation" $ do
            let parser  = parseExpression
                mResult = tryParser parser "!123"
            mResult `shouldBe` pure (UnaryExpression LogicalNegation (Int32 123), read "")

        it "parse binary expression with addition" $ do
            let parser  = parseExpression
                mResult = tryParser parser "1+1"
            mResult `shouldBe` pure (BinaryExpression (Int32 1) Addition (Int32 1), read "")

        it "parse binary expression with subtraction" $ do
            let parser  = parseExpression
                mResult = tryParser parser "1-1"
            mResult `shouldBe` pure (BinaryExpression (Int32 1) Subtraction (Int32 1), read "")

        it "parse binary expression with multiplication" $ do
            let parser  = parseExpression
                mResult = tryParser parser "1*1"
            mResult `shouldBe` pure (BinaryExpression (Int32 1) Multiplication (Int32 1), read "")

        it "parse binary expression with division" $ do
            let parser  = parseExpression
                mResult = tryParser parser "1/1"
            mResult `shouldBe` pure (BinaryExpression (Int32 1) Division (Int32 1), read "")

        it "parse complex binary expression" $ do
            let parser  = parseExpression
                mResult = tryParser parser "1    +  4 *(   3   - 1)/ 2"
                exp1    = BinaryExpression (Int32 3) Subtraction (Int32 1)
                exp2    = BinaryExpression (Int32 4) Multiplication exp1
                exp3    = BinaryExpression exp2 Division (Int32 2)
                exp4    = BinaryExpression (Int32 1) Addition exp3
            mResult `shouldBe` pure (exp4, read "")

    describe "parseStatement" $ do
        it "fails to parse empty statement" $ do
            let parser  = parseStatement
                mResult = tryParser parser ""
            mResult `shouldBe` empty

        it "fails to parse bad statement" $ do
            let parser  = parseStatement
                mResult = tryParser parser "stuff"
            mResult `shouldBe` empty

        it "fails to parse incomplete return statement" $ do
            let parser  = parseStatement
                mResult = tryParser parser "return"
            mResult `shouldBe` empty

        it "fails to parse invalid return statement" $ do
            let parser  = parseStatement
                mResult = tryParser parser "return stuff"
            mResult `shouldBe` empty

        it "fails to parse invalid return statement missing semicolon" $ do
            let parser  = parseStatement
                mResult = tryParser parser "return 124"
            mResult `shouldBe` empty

        it "parses return statement" $ do
            let parser  = parseStatement
                mResult = tryParser parser "return 124;"
            mResult `shouldBe` pure (Return (Int32 124), read "")

        it "parses return statement and leaves the rest" $ do
            let parser  = parseStatement
                mResult = tryParser parser "return 124;c"
            mResult `shouldBe` pure (Return (Int32 124), read "c")

    describe "parseFunction" $ do
        it "fails to parse empty function" $ do
            let parser  = parseFunction
                mResult = tryParser parser ""
            mResult `shouldBe` empty

        it "fails to parse function without return type" $ do
            let parser  = parseFunction
                mResult = tryParser parser "main() {return 123;}"
            mResult `shouldBe` empty

        it "fails to parse function without identifier" $ do
            let parser  = parseFunction
                mResult = tryParser parser "int () {return 123;}"
            mResult `shouldBe` empty

        it "fails to parse function without arguments" $ do
            let parser  = parseFunction
                mResult = tryParser parser "int main {return 123;}"
            mResult `shouldBe` empty

        it "fails to parse function without body braces" $ do
            let parser  = parseFunction
                mResult = tryParser parser "int main () return 123;"
            mResult `shouldBe` empty

        it "fails to parse function without body" $ do
            let parser  = parseFunction
                mResult = tryParser parser "int main () {}"
            mResult `shouldBe` empty

        it "fails to parse function with bad identifier" $ do
            let parser  = parseFunction
                mResult = tryParser parser "int 1main () {}"
            mResult `shouldBe` empty

        it "fails to parse function with bad return statement" $ do
            let parser  = parseFunction
                mResult = tryParser parser "int main () {return ;}"
            mResult `shouldBe` empty

        it "fails to parse function without space between type and identifier" $ do
            let parser  = parseFunction
                mResult = tryParser parser "intmain () {return 124;}"
            mResult `shouldBe` empty

        it "parses simple integer function" $ do
            let parser  = parseFunction
                mResult = tryParser parser "int main () {return 124;}"
                function = Function { returnType = Int
                                    , identifier = "main"
                                    , arguments  = ()
                                    , body       = Return $ Int32 124
                                    }
            mResult `shouldBe` pure (function, read "")
