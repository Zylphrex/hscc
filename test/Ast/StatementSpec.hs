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
                mResult `shouldBe` pure (Return (Int64 124), read "")

            it "parses return statement and leaves the rest" $ do
                let mResult = tryParser (parse :: Parser Statement) "return 124;c"
                mResult `shouldBe` pure (Return (Int64 124), read "c")

            it "parses expression statement" $ do
                let mResult = tryParser (parse :: Parser Statement) "124;"
                mResult `shouldBe` pure (Expression (Int64 124), read "")

            it "parses expression statement and leaves the rest" $ do
                let mResult = tryParser (parse :: Parser Statement) "124;stuff"
                mResult `shouldBe` pure (Expression (Int64 124), read "stuff")

            it "parses declaration statement without assignment" $ do
                let mResult = tryParser (parse :: Parser Statement) "int stuff;"
                mResult `shouldBe` pure (Declaration Int (toIdentifier "stuff") Nothing, read "")

            it "parses declaration statement without assignment and leaves the rest" $ do
                let mResult = tryParser (parse :: Parser Statement) "int stuff;ccc"
                mResult `shouldBe` pure (Declaration Int (toIdentifier "stuff") Nothing, read "ccc")

            it "parses declaration statement with assignment" $ do
                let mResult = tryParser (parse :: Parser Statement) "int stuff = 0;"
                mResult `shouldBe` pure (Declaration Int (toIdentifier "stuff") (Just (Int64 0)), read "")

            it "parses declaration statement wit assignment and leaves the rest" $ do
                let mResult = tryParser (parse :: Parser Statement) "int stuff = 0;ccc"
                mResult `shouldBe` pure (Declaration Int (toIdentifier "stuff") (Just (Int64 0)), read "ccc")

        describe "PrettyPrint" $ do
            it "should render return statement" $ do
                let statement = Return $ Int64 124
                render statement `shouldBe` "RETURN 124"

            it "should render expression statement" $ do
                let statement = Expression $ Int64 124
                render statement `shouldBe` "124"

            it "should render declaration statement without assignment" $ do
                let statement = Declaration Int (toIdentifier "x") Nothing
                render statement `shouldBe` "INT x"

            it "should render declaration statement with assignment" $ do
                let statement = Declaration Int (toIdentifier "x") $ Just $ Int64 124
                render statement `shouldBe` "INT x = 124"
