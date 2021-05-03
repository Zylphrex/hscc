{-# HLINT ignore "Redundant do" #-}
{-# HLINT ignore "Reduce duplication" #-}
module Ast.BlockItemSpec ( spec ) where

import Test.Hspec

import Ast.BlockItem ( BlockItem(..) )
import Ast.Expression ( Expression(Int64) )
import Ast.Identifier ( toIdentifier )
import Ast.Statement ( Statement(..) )
import Ast.Type ( Type(..) )
import Parser ( Parser, Parse(parse), tryParser )
import Pretty ( PrettyPrint(render) )

spec :: Spec
spec = do
    describe "BlockItem" $ do
        describe "Parse" $ do
            it "parses statement block item" $ do
                let mResult = tryParser (parse :: Parser BlockItem) "return 124;"
                mResult `shouldBe` pure (Statement $ Return $ Int64 124, read "")

            it "parses declaration block item without assignment" $ do
                let mResult = tryParser (parse :: Parser BlockItem) "int stuff;"
                mResult `shouldBe` pure (Declaration Int (toIdentifier "stuff") Nothing, read "")

            it "parses declaration block item without assignment and leaves the rest" $ do
                let mResult = tryParser (parse :: Parser BlockItem) "int stuff;ccc"
                mResult `shouldBe` pure (Declaration Int (toIdentifier "stuff") Nothing, read "ccc")

            it "parses declaration block item with assignment" $ do
                let mResult = tryParser (parse :: Parser BlockItem) "int stuff = 0;"
                mResult `shouldBe` pure (Declaration Int (toIdentifier "stuff") (Just (Int64 0)), read "")

            it "parses declaration block item with assignment and leaves the rest" $ do
                let mResult = tryParser (parse :: Parser BlockItem) "int stuff = 0;ccc"
                mResult `shouldBe` pure (Declaration Int (toIdentifier "stuff") (Just (Int64 0)), read "ccc")

        describe "PrettyPrint" $ do
            it "should render return statement block item" $ do
                let blockItem = Statement $ Return $ Int64 124
                render blockItem `shouldBe` "RETURN 124"

            it "should render expression statement block item" $ do
                let blockItem = Statement $ Expression $ Int64 124
                render blockItem `shouldBe` "124"

            it "should render declaration block item without assignment" $ do
                let blockItem = Declaration Int (toIdentifier "x") Nothing
                render blockItem `shouldBe` "INT x"

            it "should render declaration block item with assignment" $ do
                let blockItem = Declaration Int (toIdentifier "x") $ Just $ Int64 124
                render blockItem `shouldBe` "INT x = 124"
