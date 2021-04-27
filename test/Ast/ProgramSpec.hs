{-# HLINT ignore "Redundant do" #-}
{-# HLINT ignore "Reduce duplication" #-}
module Ast.ProgramSpec ( spec ) where

import Control.Applicative ( Alternative(empty) )
import Data.Default ( def )
import Test.Hspec

-- import Assembly ( joinAssembly, toAssembly )
import Ast.Expression ( Expression(..) )
import Ast.Function ( Function(..) )
import Ast.Identifier ( toIdentifier )
import Ast.Operator ( UnaryOperator(..), BinaryOperator(..) )
import Ast.Program ( Program(..) )
import Ast.Statement ( Statement(..) )
import Ast.Type ( Type(..) )
import Compiler ( Compile(compile), executeCompiler)
import Parser ( Parser, Parse(parse), tryParser )
import Pretty ( PrettyPrint(render) )

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
                                        , body       = [Return $ Int32 124]
                                        }
                    program = Program function
                mResult `shouldBe` pure (program, read "")

            it "parses simple program and leaves the rest" $ do
                let mResult = tryParser (parse :: Parser Program) "int main () {return 124;}ccc"
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [Return $ Int32 124]
                                        }
                    program = Program function
                mResult `shouldBe` pure (program, read "ccc")

        describe "Compile" $ do
            it "compiles simple program" $ do
                let function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [Return $ Int32 124]
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tpush\t%rbp"
                                         , "\tmovq\t%rsp, %rbp"
                                         , "\tmovq\t$124, %rax"
                                         , "\tmovq\t%rbp, %rsp"
                                         , "\tpop\t%rbp"
                                         , "\tretq"
                                         ]

            it "translates programs with negation" $ do
                let expression = UnaryExpression Negation $ Int32 124
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [Return expression]
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tpush\t%rbp"
                                         , "\tmovq\t%rsp, %rbp"
                                         , "\tmovq\t$124, %rax"
                                         , "\tnegq\t%rax"
                                         , "\tmovq\t%rbp, %rsp"
                                         , "\tpop\t%rbp"
                                         , "\tretq"
                                         ]

            it "translates programs with bitwise complement" $ do
                let expression = UnaryExpression BitwiseComplement $ Int32 124
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [Return expression]
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tpush\t%rbp"
                                         , "\tmovq\t%rsp, %rbp"
                                         , "\tmovq\t$124, %rax"
                                         , "\tnotq\t%rax"
                                         , "\tmovq\t%rbp, %rsp"
                                         , "\tpop\t%rbp"
                                         , "\tretq"
                                         ]

            it "translates programs with logical negation" $ do
                let expression = UnaryExpression LogicalNegation $ Int32 124
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [Return expression]
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tpush\t%rbp"
                                         , "\tmovq\t%rsp, %rbp"
                                         , "\tmovq\t$124, %rax"
                                         , "\tcmpq\t$0, %rax"
                                         , "\tmovq\t$0, %rax"
                                         , "\tsete\t%al"
                                         , "\tmovq\t%rbp, %rsp"
                                         , "\tpop\t%rbp"
                                         , "\tretq"
                                         ]

            it "translates programs with addition" $ do
                let expression = BinaryExpression (Int32 4) Addition (Int32 2)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [Return expression]
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tpush\t%rbp"
                                         , "\tmovq\t%rsp, %rbp"
                                         , "\tmovq\t$4, %rax"
                                         , "\tpush\t%rax"
                                         , "\tmovq\t$2, %rax"
                                         , "\tpop\t%rcx"
                                         , "\taddq\t%rcx, %rax"
                                         , "\tmovq\t%rbp, %rsp"
                                         , "\tpop\t%rbp"
                                         , "\tretq"
                                         ]

            it "translates programs with subtraction" $ do
                let expression = BinaryExpression (Int32 4) Subtraction (Int32 2)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [Return expression]
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tpush\t%rbp"
                                         , "\tmovq\t%rsp, %rbp"
                                         , "\tmovq\t$2, %rax"
                                         , "\tpush\t%rax"
                                         , "\tmovq\t$4, %rax"
                                         , "\tpop\t%rcx"
                                         , "\tsubq\t%rcx, %rax"
                                         , "\tmovq\t%rbp, %rsp"
                                         , "\tpop\t%rbp"
                                         , "\tretq"
                                         ]

            it "translates programs with multiplication" $ do
                let expression = BinaryExpression (Int32 4) Multiplication (Int32 2)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [Return expression]
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tpush\t%rbp"
                                         , "\tmovq\t%rsp, %rbp"
                                         , "\tmovq\t$4, %rax"
                                         , "\tpush\t%rax"
                                         , "\tmovq\t$2, %rax"
                                         , "\tpop\t%rcx"
                                         , "\timulq\t%rcx, %rax"
                                         , "\tmovq\t%rbp, %rsp"
                                         , "\tpop\t%rbp"
                                         , "\tretq"
                                         ]

            it "translates programs with division" $ do
                let expression = BinaryExpression (Int32 4) Division (Int32 2)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [Return expression]
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tpush\t%rbp"
                                         , "\tmovq\t%rsp, %rbp"
                                         , "\tmovq\t$2, %rax"
                                         , "\tpush\t%rax"
                                         , "\tmovq\t$4, %rax"
                                         , "\tcqto"
                                         , "\tpop\t%rcx"
                                         , "\tidivq\t%rcx"
                                         , "\tmovq\t%rbp, %rsp"
                                         , "\tpop\t%rbp"
                                         , "\tretq"
                                         ]

            it "translates programs with less than equals" $ do
                let expression = BinaryExpression (Int32 4) LessThanEquals (Int32 2)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [Return expression]
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tpush\t%rbp"
                                         , "\tmovq\t%rsp, %rbp"
                                         , "\tmovq\t$4, %rax"
                                         , "\tpush\t%rax"
                                         , "\tmovq\t$2, %rax"
                                         , "\tpop\t%rcx"
                                         , "\tcmpq\t%rax, %rcx"
                                         , "\tmovq\t$0, %rax"
                                         , "\tsetle\t%al"
                                         , "\tmovq\t%rbp, %rsp"
                                         , "\tpop\t%rbp"
                                         , "\tretq"
                                         ]

            it "translates programs with greater than equals" $ do
                let expression = BinaryExpression (Int32 4) GreaterThanEquals (Int32 2)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [Return expression]
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tpush\t%rbp"
                                         , "\tmovq\t%rsp, %rbp"
                                         , "\tmovq\t$4, %rax"
                                         , "\tpush\t%rax"
                                         , "\tmovq\t$2, %rax"
                                         , "\tpop\t%rcx"
                                         , "\tcmpq\t%rax, %rcx"
                                         , "\tmovq\t$0, %rax"
                                         , "\tsetge\t%al"
                                         , "\tmovq\t%rbp, %rsp"
                                         , "\tpop\t%rbp"
                                         , "\tretq"
                                         ]

            it "translates programs with less than" $ do
                let expression = BinaryExpression (Int32 4) LessThan (Int32 2)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [Return expression]
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tpush\t%rbp"
                                         , "\tmovq\t%rsp, %rbp"
                                         , "\tmovq\t$4, %rax"
                                         , "\tpush\t%rax"
                                         , "\tmovq\t$2, %rax"
                                         , "\tpop\t%rcx"
                                         , "\tcmpq\t%rax, %rcx"
                                         , "\tmovq\t$0, %rax"
                                         , "\tsetl\t%al"
                                         , "\tmovq\t%rbp, %rsp"
                                         , "\tpop\t%rbp"
                                         , "\tretq"
                                         ]

            it "translates programs with equals" $ do
                let expression = BinaryExpression (Int32 4) Equals (Int32 2)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [Return expression]
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tpush\t%rbp"
                                         , "\tmovq\t%rsp, %rbp"
                                         , "\tmovq\t$4, %rax"
                                         , "\tpush\t%rax"
                                         , "\tmovq\t$2, %rax"
                                         , "\tpop\t%rcx"
                                         , "\tcmpq\t%rax, %rcx"
                                         , "\tmovq\t$0, %rax"
                                         , "\tsete\t%al"
                                         , "\tmovq\t%rbp, %rsp"
                                         , "\tpop\t%rbp"
                                         , "\tretq"
                                         ]

            it "translates programs with not equals" $ do
                let expression = BinaryExpression (Int32 4) NotEquals (Int32 2)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [Return expression]
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tpush\t%rbp"
                                         , "\tmovq\t%rsp, %rbp"
                                         , "\tmovq\t$4, %rax"
                                         , "\tpush\t%rax"
                                         , "\tmovq\t$2, %rax"
                                         , "\tpop\t%rcx"
                                         , "\tcmpq\t%rax, %rcx"
                                         , "\tmovq\t$0, %rax"
                                         , "\tsetne\t%al"
                                         , "\tmovq\t%rbp, %rsp"
                                         , "\tpop\t%rbp"
                                         , "\tretq"
                                         ]

            it "translates programs with greater than" $ do
                let expression = BinaryExpression (Int32 4) GreaterThan (Int32 2)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [Return expression]
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tpush\t%rbp"
                                         , "\tmovq\t%rsp, %rbp"
                                         , "\tmovq\t$4, %rax"
                                         , "\tpush\t%rax"
                                         , "\tmovq\t$2, %rax"
                                         , "\tpop\t%rcx"
                                         , "\tcmpq\t%rax, %rcx"
                                         , "\tmovq\t$0, %rax"
                                         , "\tsetg\t%al"
                                         , "\tmovq\t%rbp, %rsp"
                                         , "\tpop\t%rbp"
                                         , "\tretq"
                                         ]

            it "translates programs with logical and" $ do
                let expression = BinaryExpression (Int32 0) LogicalAnd (Int32 1)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [Return expression]
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tpush\t%rbp"
                                         , "\tmovq\t%rsp, %rbp"
                                         , "\tmovq\t$0, %rax"
                                         , "\tcmpq\t$0, %rax"
                                         , "\tjne _rhs_and0"
                                         , "\tjmp _end_and0"
                                         , "_rhs_and0:"
                                         , "\tmovq\t$1, %rax"
                                         , "\tcmpq\t$0, %rax"
                                         , "\tmovq\t$0, %rax"
                                         , "\tsetne\t%al"
                                         , "_end_and0:"
                                         , "\tmovq\t%rbp, %rsp"
                                         , "\tpop\t%rbp"
                                         , "\tretq"
                                         ]

            it "translates programs with logical or" $ do
                let expression = BinaryExpression (Int32 0) LogicalOr (Int32 1)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [Return expression]
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tpush\t%rbp"
                                         , "\tmovq\t%rsp, %rbp"
                                         , "\tmovq\t$0, %rax"
                                         , "\tcmpq\t$0, %rax"
                                         , "\tje _rhs_or0"
                                         , "\tmovq\t$1, %rax"
                                         , "\tjmp _end_or0"
                                         , "_rhs_or0:"
                                         , "\tmovq\t$1, %rax"
                                         , "\tcmpq\t$0, %rax"
                                         , "\tmovq\t$0, %rax"
                                         , "\tsetne\t%al"
                                         , "_end_or0:"
                                         , "\tmovq\t%rbp, %rsp"
                                         , "\tpop\t%rbp"
                                         , "\tretq"
                                         ]

            it "translates programs with variables" $ do
                let a          = toIdentifier "a"
                    b          = toIdentifier "b"
                    statement1 = Declaration Int a Nothing
                    statement2 = Expression $ Assignment a $ Int32 1
                    statement3 = Declaration Int b $ Just $ Int32 2
                    statement4 = Return $ BinaryExpression (Variable a) Addition (Variable b)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [ statement1
                                                       , statement2
                                                       , statement3
                                                       , statement4
                                                       ]
                                        }
                    program = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tpush\t%rbp"
                                         , "\tmovq\t%rsp, %rbp"
                                         , "\tpush\t%rax"
                                         , "\tmovq\t$1, %rax"
                                         , "\tmovq\t%rax, -8(%rbp)"
                                         , "\tmovq\t$2, %rax"
                                         , "\tpush\t%rax"
                                         , "\tmovq\t-8(%rbp), %rax"
                                         , "\tpush\t%rax"
                                         , "\tmovq\t-16(%rbp), %rax"
                                         , "\tpop\t%rcx"
                                         , "\taddq\t%rcx, %rax"
                                         , "\tmovq\t%rbp, %rsp"
                                         , "\tpop\t%rbp"
                                         , "\tretq"
                                         ]

            it "fails to translate program with invalid assignment" $ do
                let function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [ Expression $ Assignment (toIdentifier "a") (Int32 1)
                                                       , Return $ Int32 1
                                                       ]
                                        }
                    program = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` empty

            it "fails to translate program with invalid variable" $ do
                let function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [ Return $ Variable $ toIdentifier "a" ]
                                        }
                    program = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` empty

            it "fails to translate program redeclaring a variable" $ do
                let function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [ Declaration Int (toIdentifier "a") Nothing
                                                       , Declaration Int (toIdentifier "a") Nothing
                                                       , Return $ Int32 1
                                                       ]
                                        }
                    program = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` empty

        describe "PrettyPrint" $ do
            it "should render Program" $ do
                let function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = [Return $ Int32 124]
                                        }
                    program = Program function
                    rendered = reverse $ dropWhile (== '\n') $ reverse $ unlines
                        [ "FUN INT main:"
                        , "    params: ()"
                        , "    body:"
                        , "        RETURN 124"
                        ]
                render program `shouldBe` rendered
