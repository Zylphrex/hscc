module Ast.StatementSpec ( spec ) where

import Control.Applicative ( Alternative(empty) )
import Test.Hspec

import Ast.Expression ( Expression(Int32) )
import Ast.Statement ( Statement(..) )
import Parser ( Parser, Parse(parse), tryParser )

spec :: Spec
spec = describe "Statement" $ do
    describe "Parse" $ do
        it "fails to parse empty statement" $ do
            let mResult = tryParser (parse :: Parser Statement) ""
            mResult `shouldBe` empty

        it "fails to parse bad statement" $ do
            let mResult = tryParser (parse :: Parser Statement) "stuff"
            mResult `shouldBe` empty

        it "fails to parse incomplete return statement" $ do
            let mResult = tryParser (parse :: Parser Statement) "return"
            mResult `shouldBe` empty

        it "fails to parse invalid return statement" $ do
            let mResult = tryParser (parse :: Parser Statement) "return stuff"
            mResult `shouldBe` empty

        it "fails to parse invalid return statement missing semicolon" $ do
            let mResult = tryParser (parse :: Parser Statement) "return 124"
            mResult `shouldBe` empty

        it "parses return statement" $ do
            let mResult = tryParser (parse :: Parser Statement) "return 124;"
            mResult `shouldBe` pure (Return (Int32 124), read "")

        it "parses return statement and leaves the rest" $ do
            let mResult = tryParser (parse :: Parser Statement) "return 124;c"
            mResult `shouldBe` pure (Return (Int32 124), read "c")


