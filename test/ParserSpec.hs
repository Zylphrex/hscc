module ParserSpec ( spec ) where

import Test.Hspec

import Control.Applicative ( Alternative(empty) )

import Parser ( tryParser
              , parseCharacter
              , parseIf
              , parseNotNull
              , parseSpaces
              , parseString
              , parseWhile
              )

spec :: Spec
spec = do
    describe "Parser" $ do
        describe "parseIf" $ do
            it "fails when predicate is false" $ do
                let parser  = parseIf (== 'd')
                    mResult = tryParser parser "c"
                mResult `shouldBe` empty

            it "parses when predicate is true" $ do
                let parser  = parseIf (== 'c')
                    mResult = tryParser parser "c"
                mResult `shouldBe` pure ('c', read "")

            it "parses when predicate is true and leaves the rest" $ do
                let parser  = parseIf (== 'c')
                    mResult = tryParser parser "ch"
                mResult `shouldBe` pure ('c', read "h")

        describe "parseWhile" $ do
            it "parses when predicate is always false" $ do
                let parser  = parseWhile (== 'd')
                    mResult = tryParser parser "c"
                mResult `shouldBe` pure ("", read "c")

            it "parses when predicate is true once" $ do
                let parser  = parseWhile (== 'c')
                    mResult = tryParser parser "c"
                mResult `shouldBe` pure ("c", read "")

            it "parses when predicate is true once and leaves the rest" $ do
                let parser  = parseWhile (== 'c')
                    mResult = tryParser parser "ch"
                mResult `shouldBe` pure ("c", read "h")

            it "parses when predicate is true multiple times" $ do
                let parser  = parseWhile (== 'c')
                    mResult = tryParser parser "cccc"
                mResult `shouldBe` pure ("cccc", read "")

            it "parses when predicate is true multiple times and leaves the rest" $ do
                let parser  = parseWhile (== 'c')
                    mResult = tryParser parser "cccch"
                mResult `shouldBe` pure ("cccc", read "h")

        describe "parseCharacter" $ do
            it "fails to parse a single character" $ do
                let parser  = parseCharacter 'c'
                    mResult = tryParser parser "d"
                mResult `shouldBe` empty

            it "parses a single character" $ do
                let parser  = parseCharacter 'c'
                    mResult = tryParser parser "c"
                mResult `shouldBe` pure ('c', read "")

            it "parses a single character and leaves the rest" $ do
                let parser  = parseCharacter 'c'
                    mResult = tryParser parser "ch"
                mResult `shouldBe` pure ('c', read "h")

        describe "parseString" $ do
            it "fails to parse a string" $ do
                let parser  = parseString "c"
                    mResult = tryParser parser "d"
                mResult `shouldBe` empty

            it "parses a single string" $ do
                let parser  = parseString "cccc"
                    mResult = tryParser parser "cccc"
                mResult `shouldBe` pure ("cccc", read "")

            it "parses a single string and leaves the rest" $ do
                let parser  = parseString "cccc"
                    mResult = tryParser parser "cccch"
                mResult `shouldBe` pure ("cccc", read "h")

        describe "parseSpaces" $ do
            it "parses zero white spaces" $ do
                let parser  = parseSpaces
                    mResult = tryParser parser "c"
                mResult `shouldBe` pure ("", read "c")

            it "parses one white space" $ do
                let parser  = parseSpaces
                    mResult = tryParser parser " "
                mResult `shouldBe` pure (" ", read "")

            it "parses one white space and leaves the rest" $ do
                let parser  = parseSpaces
                    mResult = tryParser parser " c"
                mResult `shouldBe` pure (" ", read "c")

            it "parses multiple white space" $ do
                let parser  = parseSpaces
                    mResult = tryParser parser "   "
                mResult `shouldBe` pure ("   ", read "")

            it "parses multiple white space and leaves the rest" $ do
                let parser  = parseSpaces
                    mResult = tryParser parser "   c"
                mResult `shouldBe` pure ("   ", read "c")

        describe "parseNotNull" $ do
            it "fails to parse no white spaces" $ do
                let parser  = parseNotNull parseSpaces
                    mResult = tryParser parser ""
                mResult `shouldBe` empty

            it "parses white spaces" $ do
                let parser  = parseNotNull parseSpaces
                    mResult = tryParser parser "    "
                mResult `shouldBe` pure ("    ", read "")
