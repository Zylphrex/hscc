{-# HLINT ignore "Redundant do" #-}
{-# HLINT ignore "Reduce duplication" #-}
module Ast.StatementSpec ( spec ) where

import Control.Applicative ( Alternative(empty) )
import Test.Hspec

import Ast.Expression ( Expression(Int64) )
import Ast.Identifier ( toIdentifier )
import Ast.Statement ( Statement(..) )
import Ast.Type ( Type(..) )
import Parser ( Parser, Parse(parse), tryParser )
import Pretty ( PrettyPrint(render) )

spec :: Spec
spec = do
    describe "Statement" $ do
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
                mResult `shouldBe` pure (Return $ Int64 124, read "")

            it "parses return statement and leaves the rest" $ do
                let mResult = tryParser (parse :: Parser Statement) "return 124;c"
                mResult `shouldBe` pure (Return $ Int64 124, read "c")

            it "parses expression statement" $ do
                let mResult = tryParser (parse :: Parser Statement) "124;"
                mResult `shouldBe` pure (Expression $ Int64 124, read "")

            it "parses expression statement and leaves the rest" $ do
                let mResult = tryParser (parse :: Parser Statement) "124;stuff"
                mResult `shouldBe` pure (Expression $ Int64 124, read "stuff")

        describe "PrettyPrint" $ do
            it "should render return statement" $ do
                let statement = Return $ Int64 124
                render statement `shouldBe` "RETURN 124"

            it "should render expression statement" $ do
                let statement = Expression $ Int64 124
                render statement `shouldBe` "124"
