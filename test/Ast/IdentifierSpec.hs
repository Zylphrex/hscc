{-# HLINT ignore "Redundant do" #-}
{-# HLINT ignore "Reduce duplication" #-}
module Ast.IdentifierSpec ( spec ) where

import Control.Applicative ( Alternative(empty) )
import Control.Exception ( evaluate )
import Test.Hspec
import Text.PrettyPrint ( render )

import Assembly ( OsOption(Darwin, Other)
                , Option(Option)
                , osOption
                , toAssembly
                )
import Ast.Identifier ( Identifier, toIdentifier )
import Parser ( Parse(parse), Parser, tryParser )
import Pretty ( PrettyPrint(prettyPrint) )

spec :: Spec
spec = do
    describe "Identifier" $ do
        describe "toIdentifier" $ do
            it "rejects blank identifiers" $ do
                evaluate (toIdentifier "") `shouldThrow` anyException

            it "rejects identifiers starting with space" $ do
                evaluate (toIdentifier " ") `shouldThrow` anyException

            it "rejects identifiers starting with unexpected symbol" $ do
                evaluate (toIdentifier "$") `shouldThrow` anyException

            it "rejects identifiers starting with digit" $ do
                evaluate (toIdentifier "1") `shouldThrow` anyException

        describe "Parse" $ do
            it "fails when identifier is blank" $ do
                let mResult = tryParser (parse :: Parser Identifier) ""
                mResult `shouldBe` empty

            it "fails when identifier starts with space" $ do
                let mResult = tryParser (parse :: Parser Identifier) " "
                mResult `shouldBe` empty

            it "fails when identifier starts with unexpected symbol" $ do
                let mResult = tryParser (parse :: Parser Identifier) "$"
                mResult `shouldBe` empty

            it "fails when identifier starts with digit" $ do
                let mResult = tryParser (parse :: Parser Identifier) "1"
                mResult `shouldBe` empty

            it "parses when identifier is single letter" $ do
                let mResult = tryParser (parse :: Parser Identifier) "c"
                mResult `shouldBe` pure (toIdentifier "c", read "")

            it "parses when identifier is single letter and leaves the rest" $ do
                let mResult = tryParser (parse :: Parser Identifier) "c$"
                mResult `shouldBe` pure (toIdentifier "c", read "$")

            it "parses when identifier is single underscore" $ do
                let mResult = tryParser (parse :: Parser Identifier) "_"
                mResult `shouldBe` pure (toIdentifier "_", read "")

            it "parses when identifier is single underscore and leaves the rest" $ do
                let mResult = tryParser (parse :: Parser Identifier) "_$"
                mResult `shouldBe` pure (toIdentifier "_", read "$")

            it "parses when identifier is complex" $ do
                let mResult = tryParser (parse :: Parser Identifier) "aB_1"
                mResult `shouldBe` pure (toIdentifier "aB_1", read "")

            it "parses when identifier is complex and leaves the rest" $ do
                let mResult = tryParser (parse :: Parser Identifier) "aB_1$"
                mResult `shouldBe` pure (toIdentifier "aB_1", read "$")

        describe "Assembly" $ do
            it "prefixes with an underscore on Darwin" $ do
                let option   = Option { osOption = Darwin}
                    assembly = toAssembly option (toIdentifier "main")
                assembly `shouldBe` "_main"

            it "does not prefixes with an underscore on Other" $ do
                let option   = Option { osOption = Other}
                    assembly = toAssembly option (toIdentifier "main")
                assembly `shouldBe` "main"

        describe "PrettyPrint" $ do
            it "renders identifier" $ do
                let identifier = toIdentifier "main"
                render (prettyPrint identifier) `shouldBe` "main"
