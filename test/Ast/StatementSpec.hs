{-# HLINT ignore "Redundant do" #-}
{-# HLINT ignore "Reduce duplication" #-}
module Ast.StatementSpec ( spec ) where

import Control.Applicative ( Alternative(empty) )
import Test.Hspec

import Ast.BlockItem ( BlockItem(..), Statement(..), Declaration(..) )
import Ast.Expression ( Expression(..) )
import Ast.Identifier ( toIdentifier )
import Ast.Operator ( AssignmentOperator(..), BinaryOperator(..) )
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
                mResult `shouldBe` pure (Expression $ Just $ Int64 124, read "")

            it "parses expression statement and leaves the rest" $ do
                let mResult = tryParser (parse :: Parser Statement) "124;stuff"
                mResult `shouldBe` pure (Expression $ Just $ Int64 124, read "stuff")

            it "parses empty expression statement" $ do
                let mResult = tryParser (parse :: Parser Statement) ";"
                mResult `shouldBe` pure (Expression $ Nothing, read "")

            it "parse if statement without else" $ do
                let mResult = tryParser (parse :: Parser Statement) "if (1) return 5;"
                mResult `shouldBe` pure (Conditional (Int64 1) (Return (Int64 5)) Nothing, read "")

            it "parse if statement with else" $ do
                let mResult = tryParser (parse :: Parser Statement) "if (1) return 5; else return 10;"
                    return1 = Return $ Int64 5
                    return2 = Return $ Int64 10
                mResult `shouldBe` pure (Conditional (Int64 1) return1 $ Just return2, read "")

            it "parse compound statement" $ do
                let mResult = tryParser (parse :: Parser Statement) "{return 0;}"
                mResult `shouldBe` pure (Compound [StatementItem $ Return $ Int64 0], read "")

            it "parses while loops" $ do
                let mResult = tryParser (parse :: Parser Statement) "while (0) ;"
                mResult `shouldBe` pure (While (Int64 0) (Expression Nothing), read "")

            it "parses do while loops" $ do
                let mResult = tryParser (parse :: Parser Statement) "do ; while (0);"
                mResult `shouldBe` pure (DoWhile (Expression Nothing) (Int64 0), read "")

            it "parses for loops" $ do
                let mResult = tryParser (parse :: Parser Statement) "for (i = 0; i < 1; i += 1) ;"
                    i = toIdentifier "i"
                mResult `shouldBe` pure ( For (Just $ AssignmentExpression i Assignment (Int64 0))
                                              (Just $ BinaryExpression (Variable i) LessThan (Int64 1))
                                              (Just $ AssignmentExpression i AdditionAssignment (Int64 1))
                                              (Expression Nothing)
                                        , read ""
                                        )

            it "parses empty for loops" $ do
                let mResult = tryParser (parse :: Parser Statement) "for (;;);"
                mResult `shouldBe` pure (For Nothing Nothing Nothing (Expression Nothing), read "")

            it "parses for loops with declaration" $ do
                let mResult = tryParser (parse :: Parser Statement) "for (int i = 0; i < 1; i += 1) ;"
                    i = toIdentifier "i"
                mResult `shouldBe` pure ( ForDeclaration (Just $ Declaration Int i (Just $ Int64 0))
                                                         (Just $ BinaryExpression (Variable i) LessThan (Int64 1))
                                                         (Just $ AssignmentExpression i AdditionAssignment (Int64 1))
                                                         (Expression Nothing)
                                        , read ""
                                        )

            it "parses empty for loops with Declaration" $ do
                let mResult = tryParser (parse :: Parser Statement) "for (int i = 0;;);"
                    i = toIdentifier "i"
                mResult `shouldBe` pure ( ForDeclaration (Just $ Declaration Int i (Just $ Int64 0))
                                                         Nothing Nothing
                                                         (Expression Nothing)
                                        , read ""
                                        )

            it "parses loop with break" $ do
                let mResult = tryParser (parse :: Parser Statement) "for (;;) break;"
                mResult `shouldBe` pure (For Nothing Nothing Nothing Break, read "")

            it "parses for loop with continue" $ do
                let mResult = tryParser (parse :: Parser Statement) "for (;;) continue;"
                mResult `shouldBe` pure (For Nothing Nothing Nothing Continue, read "")

        describe "PrettyPrint" $ do
            it "should render return statement" $ do
                let statement = Return $ Int64 124
                render statement `shouldBe` "RETURN 124"

            it "should render expression statement" $ do
                let statement = Expression $ Just $ Int64 124
                render statement `shouldBe` "124"

            it "should render empty expression statement" $ do
                let statement = Expression Nothing
                render statement `shouldBe` "NOOP"

            it "should render condition statement without else" $ do
                let statement = Conditional (Int64 1) (Return (Int64 5)) Nothing
                    rendered = reverse $ dropWhile (== '\n') $ reverse $ unlines
                        [ "IF 1"
                        , "    RETURN 5"
                        ]
                render statement `shouldBe` rendered

            it "should render condition statement with else" $ do
                let statement = Conditional (Int64 1) (Return (Int64 5)) $ Just (Return (Int64 10))
                    rendered = reverse $ dropWhile (== '\n') $ reverse $ unlines
                        [ "IF 1"
                        , "    RETURN 5"
                        , "ELSE"
                        , "    RETURN 10"
                        ]
                render statement `shouldBe` rendered

            it "should render compound statement" $ do
                let a = toIdentifier "a"
                    statement = Compound [ DeclarationItem $ Declaration Int a Nothing
                                         , StatementItem $ Return $ Variable a
                                         ]
                    rendered = reverse $ dropWhile (== '\n') $ reverse $ unlines
                        [ "{   INT a"
                        , "    RETURN a"
                        , "}"
                        ]
                render statement `shouldBe` rendered

            it "should render compound statement in conditional statement" $ do
                let statement1 = Compound [ StatementItem $ Return $ Int64 1 ]
                    statement2 = Compound [ StatementItem $ Return $ Int64 2 ]
                    statement3 = Conditional (Int64 1) statement1 $ Just statement2
                    rendered = reverse $ dropWhile (== '\n') $ reverse $ unlines
                        [ "IF 1 {"
                        , "    RETURN 1"
                        , "}"
                        , "ELSE {"
                        , "    RETURN 2"
                        , "}"
                        ]
                render statement3 `shouldBe` rendered

            it "should render while loops" $ do
                let while = While (Int64 0) (Expression Nothing)
                    rendered = reverse $ dropWhile (== '\n') $ reverse $ unlines
                        [ "WHILE 0"
                        , "    NOOP"
                        ]
                render while `shouldBe` rendered

            it "should render do while loops" $ do
                let while = DoWhile (Expression Nothing) (Int64 0)
                    rendered = reverse $ dropWhile (== '\n') $ reverse $ unlines
                        [ "DO  NOOP"
                        , "WHILE 0"
                        ]
                render while `shouldBe` rendered

            it "should render for loops" $ do
                let i = toIdentifier "i"
                    for = For (Just $ AssignmentExpression i Assignment (Int64 0))
                              (Just $ BinaryExpression (Variable i) LessThan (Int64 1))
                              (Just $ AssignmentExpression i AdditionAssignment (Int64 1))
                              (Expression Nothing)
                    rendered = reverse $ dropWhile (== '\n') $ reverse $ unlines
                        [ "FOR ( i = 0 ; (i<1) ; i += 1 )"
                        , "    NOOP"
                        ]
                render for `shouldBe` rendered

            it "should render empty for loops" $ do
                let for = For Nothing Nothing Nothing (Expression Nothing)
                    rendered = reverse $ dropWhile (== '\n') $ reverse $ unlines
                        [ "FOR ( ; ; )"
                        , "    NOOP"
                        ]
                render for `shouldBe` rendered

            it "should render for loops with declaration" $ do
                let i = toIdentifier "i"
                    for = ForDeclaration (Just $ Declaration Int i (Just $ Int64 0))
                                         (Just $ BinaryExpression (Variable i) LessThan (Int64 1))
                                         (Just $ AssignmentExpression i AdditionAssignment (Int64 1))
                                         (Expression Nothing)
                    rendered = reverse $ dropWhile (== '\n') $ reverse $ unlines
                        [ "FOR ( INT i = 0 ; (i<1) ; i += 1 )"
                        , "    NOOP"
                        ]
                render for `shouldBe` rendered

            it "should render empty for loops with declaration" $ do
                let for = ForDeclaration Nothing Nothing Nothing (Expression Nothing)
                    rendered = reverse $ dropWhile (== '\n') $ reverse $ unlines
                        [ "FOR ( ; ; )"
                        , "    NOOP"
                        ]
                render for `shouldBe` rendered

            it "should render loops with break" $ do
                let loop = For Nothing Nothing Nothing Break
                    rendered = reverse $ dropWhile (== '\n') $ reverse $ unlines
                        [ "FOR ( ; ; )"
                        , "    BREAK"
                        ]
                render loop `shouldBe` rendered

            it "should render loops with continue" $ do
                let loop = For Nothing Nothing Nothing Continue
                    rendered = reverse $ dropWhile (== '\n') $ reverse $ unlines
                        [ "FOR ( ; ; )"
                        , "    CONTINUE"
                        ]
                render loop `shouldBe` rendered
