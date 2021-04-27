{-# HLINT ignore "Redundant do" #-}
{-# HLINT ignore "Reduce duplication" #-}
module Ast.TypeSpec ( spec ) where

import Control.Applicative ( Alternative(empty) )
import Test.Hspec
import Text.PrettyPrint ( render )

import Ast.Type ( Type(..), bytes )
import Parser ( Parser, Parse(parse), tryParser )
import Pretty ( PrettyPrint(prettyPrint) )

spec :: Spec
spec = do
    describe "Type" $ do
        describe "Parse" $ do
            it "fails to parse empty type" $ do
                let mResult = tryParser (parse :: Parser Type) ""
                mResult `shouldBe` empty

            it "fails to parse bad type" $ do
                let mResult = tryParser (parse :: Parser Type) "stuff"
                mResult `shouldBe` empty

            it "parse int type" $ do
                let mResult = tryParser (parse :: Parser Type) "int"
                mResult `shouldBe` pure (Int, read "")

            it "parse int type and leaves the rest" $ do
                let mResult = tryParser (parse :: Parser Type) "intx"
                mResult `shouldBe` pure (Int, read "x")

        describe "PrettyPrint" $ do
            it "should render int type" $ do
                render (prettyPrint Int) `shouldBe` "INT"

        describe "bytes" $ do
            it "should return the correct size in bytes" $ do
                bytes Int `shouldBe` 8
