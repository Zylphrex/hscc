module Ast.TypeSpec ( spec ) where

import Control.Applicative ( Alternative(empty) )
import Test.Hspec

import Ast.Type ( Type(..) )
import Parser ( Parser, Parse(parse), tryParser )

spec :: Spec
spec = describe "Type" $ do
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
