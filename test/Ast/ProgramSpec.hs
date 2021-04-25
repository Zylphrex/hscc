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

        describe "Compile" $ do
            it "compiles simple program" $ do
                let function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = Return $ Int32 124
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
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
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
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
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
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
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
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
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
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
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
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
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
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
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tmovq\t$2, %rax"
                                         , "\tpush\t%rax"
                                         , "\tmovq\t$4, %rax"
                                         , "\tcqto"
                                         , "\tpop\t%rcx"
                                         , "\tidivq\t%rcx"
                                         , "\tretq"
                                         ]

            it "translates programs with less than equals" $ do
                let expression = BinaryExpression (Int32 4) LessThanEquals (Int32 2)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = Return expression
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tmovq\t$4, %rax"
                                         , "\tpush\t%rax"
                                         , "\tmovq\t$2, %rax"
                                         , "\tpop\t%rcx"
                                         , "\tcmpq\t%rax, %rcx"
                                         , "\tmovq\t$0, %rax"
                                         , "\tsetle\t%al"
                                         , "\tretq"
                                         ]

            it "translates programs with greater than equals" $ do
                let expression = BinaryExpression (Int32 4) GreaterThanEquals (Int32 2)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = Return expression
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tmovq\t$4, %rax"
                                         , "\tpush\t%rax"
                                         , "\tmovq\t$2, %rax"
                                         , "\tpop\t%rcx"
                                         , "\tcmpq\t%rax, %rcx"
                                         , "\tmovq\t$0, %rax"
                                         , "\tsetge\t%al"
                                         , "\tretq"
                                         ]

            it "translates programs with less than" $ do
                let expression = BinaryExpression (Int32 4) LessThan (Int32 2)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = Return expression
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tmovq\t$4, %rax"
                                         , "\tpush\t%rax"
                                         , "\tmovq\t$2, %rax"
                                         , "\tpop\t%rcx"
                                         , "\tcmpq\t%rax, %rcx"
                                         , "\tmovq\t$0, %rax"
                                         , "\tsetl\t%al"
                                         , "\tretq"
                                         ]

            it "translates programs with equals" $ do
                let expression = BinaryExpression (Int32 4) Equals (Int32 2)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = Return expression
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tmovq\t$4, %rax"
                                         , "\tpush\t%rax"
                                         , "\tmovq\t$2, %rax"
                                         , "\tpop\t%rcx"
                                         , "\tcmpq\t%rax, %rcx"
                                         , "\tmovq\t$0, %rax"
                                         , "\tsete\t%al"
                                         , "\tretq"
                                         ]

            it "translates programs with not equals" $ do
                let expression = BinaryExpression (Int32 4) NotEquals (Int32 2)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = Return expression
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tmovq\t$4, %rax"
                                         , "\tpush\t%rax"
                                         , "\tmovq\t$2, %rax"
                                         , "\tpop\t%rcx"
                                         , "\tcmpq\t%rax, %rcx"
                                         , "\tmovq\t$0, %rax"
                                         , "\tsetne\t%al"
                                         , "\tretq"
                                         ]

            it "translates programs with greater than" $ do
                let expression = BinaryExpression (Int32 4) GreaterThan (Int32 2)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = Return expression
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tmovq\t$4, %rax"
                                         , "\tpush\t%rax"
                                         , "\tmovq\t$2, %rax"
                                         , "\tpop\t%rcx"
                                         , "\tcmpq\t%rax, %rcx"
                                         , "\tmovq\t$0, %rax"
                                         , "\tsetg\t%al"
                                         , "\tretq"
                                         ]

            it "translates programs with logical and" $ do
                let expression = BinaryExpression (Int32 0) LogicalAnd (Int32 1)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = Return expression
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tmovq\t$0, %rax"
                                         , "\tcmpq\t$0, %rax"
                                         , "\tjne _rhs0"
                                         , "\tjmp _end0"
                                         , "_rhs0:"
                                         , "\tmovq\t$1, %rax"
                                         , "\tcmpq\t$0, %rax"
                                         , "\tmovq\t$0, %rax"
                                         , "\tsetne\t%al"
                                         , "_end0:"
                                         , "\tretq"
                                         ]

            it "translates programs with logical or" $ do
                let expression = BinaryExpression (Int32 0) LogicalOr (Int32 1)
                    function = Function { returnType = Int
                                        , identifier = toIdentifier "main"
                                        , arguments  = ()
                                        , body       = Return expression
                                        }
                    program  = Program function
                    assembly = executeCompiler (compile program) def
                assembly `shouldBe` pure [ "\t.globl\tmain"
                                         , "main:"
                                         , "\tmovq\t$0, %rax"
                                         , "\tcmpq\t$0, %rax"
                                         , "\tje _rhs0"
                                         , "\tmovq\t$1, %rax"
                                         , "\tjmp _end0"
                                         , "_rhs0:"
                                         , "\tmovq\t$1, %rax"
                                         , "\tcmpq\t$0, %rax"
                                         , "\tmovq\t$0, %rax"
                                         , "\tsetne\t%al"
                                         , "_end0:"
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
                render program `shouldBe` rendered
