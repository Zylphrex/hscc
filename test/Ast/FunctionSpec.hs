{-# HLINT ignore "Redundant do" #-}
{-# HLINT ignore "Reduce duplication" #-}
module Ast.FunctionSpec ( spec ) where

import Control.Applicative ( Alternative(empty) )
import Test.Hspec

import Ast.Expression ( Expression(Int32) )
import Ast.Function ( Function(..) )
import Ast.Identifier ( toIdentifier )
import Ast.Statement ( Statement(..) )
import Ast.Type ( Type(..) )
import Parser ( Parser, Parse(parse), tryParser )
import Pretty ( PrettyPrint(render) )

spec :: Spec
spec = do
    describe "Function" $ do
        describe "accessors" $ do
            it "should have accessors for properties" $ do
                let function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [Return $ Int32 124]
                                        }
                returnType function `shouldBe` Int
                identifier function `shouldBe` toIdentifier "main"
                arguments function `shouldBe` ()
                body function `shouldBe` [Return (Int32 124)]

        describe "Parse" $ do
            it "fails to parse empty function" $ do
                let mResult = tryParser (parse :: Parser Function) ""
                mResult `shouldBe` empty

            it "fails to parse function without return type" $ do
                let mResult = tryParser (parse :: Parser Function) "main() {return 123;}"
                mResult `shouldBe` empty

            it "fails to parse function without identifier" $ do
                let mResult = tryParser (parse :: Parser Function) "int () {return 123;}"
                mResult `shouldBe` empty

            it "fails to parse function without arguments" $ do
                let mResult = tryParser (parse :: Parser Function) "int main {return 123;}"
                mResult `shouldBe` empty

            it "fails to parse function without body braces" $ do
                let mResult = tryParser (parse :: Parser Function) "int main () return 123;"
                mResult `shouldBe` empty

            it "fails to parse function without body" $ do
                let mResult = tryParser (parse :: Parser Function) "int main () {}"
                mResult `shouldBe` empty

            it "fails to parse function with bad identifier" $ do
                let mResult = tryParser (parse :: Parser Function) "int 1main () {}"
                mResult `shouldBe` empty

            it "fails to parse function with bad return statement" $ do
                let mResult = tryParser (parse :: Parser Function) "int main () {return ;}"
                mResult `shouldBe` empty

            it "fails to parse function without space between type and identifier" $ do
                let mResult = tryParser (parse :: Parser Function) "intmain () {return 124;}"
                mResult `shouldBe` empty

            it "parses simple integer function" $ do
                let mResult = tryParser (parse :: Parser Function) "int main () {return 124;}"
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [Return $ Int32 124]
                                        }
                mResult `shouldBe` pure (function, read "")

        describe "PrettyPrint" $ do
            it "should render function" $ do
                let function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [Return $ Int32 124]
                                        }
                    rendered = reverse $ dropWhile (== '\n') $ reverse $ unlines
                        [ "FUN INT main:"
                        , "    params: ()"
                        , "    body:"
                        , "        RETURN 124"
                        ]
                render function `shouldBe` rendered
