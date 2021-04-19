{-# HLINT ignore "Redundant do" #-}
{-# HLINT ignore "Reduce duplication" #-}
module Ast.ProgramSpec ( spec ) where

import Control.Applicative ( Alternative(empty) )
import Data.Default ( def )
import Test.Hspec
import Text.PrettyPrint ( render )

import Assembly ( joinAssembly, toAssembly )
import Ast.Expression ( Expression(..) )
import Ast.Function ( Function(..) )
import Ast.Identifier ( toIdentifier )
import Ast.Operator ( UnaryOperator(..), BinaryOperator(..) )
import Ast.Program ( Program(..) )
import Ast.Statement ( Statement(..) )
import Ast.Type ( Type(..) )
import Parser ( Parser, Parse(parse), tryParser )
import Pretty ( PrettyPrint(prettyPrint) )

spec :: Spec
spec = do
    describe "Function" $ do
        describe "Parse" $ do
            it "fails to parse empty program" $ do
                let mResult = tryParser (parse :: Parser Program) ""
                mResult `shouldBe` empty

            it "parses simple program" $ do
                let mResult = tryParser (parse :: Parser Program) "int main () {return 124;}"
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = Return $ Int32 124
                                        }
                    program = Program function
                mResult `shouldBe` pure (program, read "")

            it "parses simple program and leaves the rest" $ do
                let mResult = tryParser (parse :: Parser Program) "int main () {return 124;}ccc"
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = Return $ Int32 124
                                        }
                    program = Program function
                mResult `shouldBe` pure (program, read "ccc")

        describe "Assembly" $ do
            it "translates simple program" $ do
                let function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = Return $ Int32 124
                                        }
                    program  = Program function
                    assembly = toAssembly def program
                assembly `shouldBe` joinAssembly [ "\t.globl\tmain"
                                                 , "main:"
                                                 , "\tmovq\t$124, %rax"
                                                 , "\tretq"
                                                 ]

            it "translates programs with negation" $ do
                let expression = UnaryExpression Negation $ Int32 124
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = Return expression
                                        }
                    program  = Program function
                    assembly = toAssembly def program
                assembly `shouldBe` joinAssembly [ "\t.globl\tmain"
                                                 , "main:"
                                                 , "\tmovq\t$124, %rax"
                                                 , "\tnegq\t%rax"
                                                 , "\tretq"
                                                 ]

            it "translates programs with bitwise complement" $ do
                let expression = UnaryExpression BitwiseComplement $ Int32 124
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = Return expression
                                        }
                    program  = Program function
                    assembly = toAssembly def program
                assembly `shouldBe` joinAssembly [ "\t.globl\tmain"
                                                 , "main:"
                                                 , "\tmovq\t$124, %rax"
                                                 , "\tnotq\t%rax"
                                                 , "\tretq"
                                                 ]

            it "translates programs with logical negation" $ do
                let expression = UnaryExpression LogicalNegation $ Int32 124
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = Return expression
                                        }
                    program  = Program function
                    assembly = toAssembly def program
                assembly `shouldBe` joinAssembly [ "\t.globl\tmain"
                                                 , "main:"
                                                 , "\tmovq\t$124, %rax"
                                                 , "\tcmpq\t$0, %rax"
                                                 , "\tmovq\t$0, %rax"
                                                 , "\tsete\t%al"
                                                 , "\tretq"
                                                 ]

            it "translates programs with addition" $ do
                let expression = BinaryExpression (Int32 4) Addition (Int32 2)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = Return expression
                                        }
                    program  = Program function
                    assembly = toAssembly def program
                assembly `shouldBe` joinAssembly [ "\t.globl\tmain"
                                                 , "main:"
                                                 , "\tmovq\t$4, %rax"
                                                 , "\tpush\t%rax"
                                                 , "\tmovq\t$2, %rax"
                                                 , "\tpop\t%rcx"
                                                 , "\taddq\t%rcx, %rax"
                                                 , "\tretq"
                                                 ]

            it "translates programs with subtraction" $ do
                let expression = BinaryExpression (Int32 4) Subtraction (Int32 2)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = Return expression
                                        }
                    program  = Program function
                    assembly = toAssembly def program
                assembly `shouldBe` joinAssembly [ "\t.globl\tmain"
                                                 , "main:"
                                                 , "\tmovq\t$2, %rax"
                                                 , "\tpush\t%rax"
                                                 , "\tmovq\t$4, %rax"
                                                 , "\tpop\t%rcx"
                                                 , "\tsubq\t%rcx, %rax"
                                                 , "\tretq"
                                                 ]

            it "translates programs with multiplication" $ do
                let expression = BinaryExpression (Int32 4) Multiplication (Int32 2)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = Return expression
                                        }
                    program  = Program function
                    assembly = toAssembly def program
                assembly `shouldBe` joinAssembly [ "\t.globl\tmain"
                                                 , "main:"
                                                 , "\tmovq\t$4, %rax"
                                                 , "\tpush\t%rax"
                                                 , "\tmovq\t$2, %rax"
                                                 , "\tpop\t%rcx"
                                                 , "\timulq\t%rcx, %rax"
                                                 , "\tretq"
                                                 ]

            it "translates programs with division" $ do
                let expression = BinaryExpression (Int32 4) Division (Int32 2)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = Return expression
                                        }
                    program  = Program function
                    assembly = toAssembly def program
                assembly `shouldBe` joinAssembly [ "\t.globl\tmain"
                                                 , "main:"
                                                 , "\tmovq\t$2, %rax"
                                                 , "\tpush\t%rax"
                                                 , "\tmovq\t$4, %rax"
                                                 , "\tcqto"
                                                 , "\tpop\t%rcx"
                                                 , "\tidivq\t%rcx"
                                                 , "\tretq"
                                                 ]

        describe "PrettyPrint" $ do
            it "should render Program" $ do
                let function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = Return $ Int32 124
                                        }
                    program = Program function
                    rendered = reverse $ dropWhile (== '\n') $ reverse $ unlines
                        [ "FUN INT main:"
                        , "    params: ()"
                        , "    body:"
                        , "        RETURN 124"
                        ]
                render (prettyPrint program) `shouldBe` rendered
