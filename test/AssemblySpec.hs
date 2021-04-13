module AssemblySpec ( spec ) where

import Test.Hspec

import Ast ( Expression(Int32, UnaryExpression)
           , Function(Function)
           , Program(Program)
           , Statement(Return)
           , Type(Int)
           , UnaryOperator(Negation, BitwiseComplement, LogicalNegation)
           , returnType
           , identifier
           , arguments
           , body
           )
import Assembly ( OsOption(Darwin, Other)
                , Option(Option)
                , asAssembly
                , osOption
                )

spec :: Spec
spec = do
    describe "Assembly" $ do
        describe "Darwin" $ do
            let option = Option { osOption = Darwin}

            it "translates simple program" $ do
                let function = Function { returnType = Int
                                        , identifier = "main"
                                        , arguments  = ()
                                        , body       = Return $ Int32 124
                                        }
                    program  = Program function
                    assembly = asAssembly option program
                assembly `shouldBe` "\t.globl\t_main\n_main:\n\tmovl\t$124, %eax\n\tretq\n"

            it "translates programs with negation" $ do
                let expression = UnaryExpression Negation $ Int32 124
                    function = Function { returnType = Int
                                        , identifier = "main"
                                        , arguments  = ()
                                        , body       = Return expression
                                        }
                    program  = Program function
                    assembly = asAssembly option program
                assembly `shouldBe` "\t.globl\t_main\n_main:\n\tmovl\t$124, %eax\n\tneg\t%eax\n\tretq\n"

            it "translates programs with bitwise complement" $ do
                let expression = UnaryExpression BitwiseComplement $ Int32 124
                    function = Function { returnType = Int
                                        , identifier = "main"
                                        , arguments  = ()
                                        , body       = Return expression
                                        }
                    program  = Program function
                    assembly = asAssembly option program
                assembly `shouldBe` "\t.globl\t_main\n_main:\n\tmovl\t$124, %eax\n\tnot\t%eax\n\tretq\n"

            it "translates programs with logical negation" $ do
                let expression = UnaryExpression LogicalNegation $ Int32 124
                    function = Function { returnType = Int
                                        , identifier = "main"
                                        , arguments  = ()
                                        , body       = Return expression
                                        }
                    program  = Program function
                    assembly = asAssembly option program
                assembly `shouldBe` "\t.globl\t_main\n_main:\n\tmovl\t$124, %eax\n\tcmpl\t$0, %eax\n\tsete\t%al\n\tretq\n"

        describe "Other" $ do
            let option = Option { osOption = Other }

            it "translates simple program" $ do
                let function = Function { returnType = Int
                                        , identifier = "main"
                                        , arguments  = ()
                                        , body       = Return $ Int32 124
                                        }
                    program  = Program function
                    assembly = asAssembly option program
                assembly `shouldBe` "\t.globl\tmain\nmain:\n\tmovl\t$124, %eax\n\tretq\n"

            it "translates programs with negation" $ do
                let expression = UnaryExpression Negation $ Int32 124
                    function = Function { returnType = Int
                                        , identifier = "main"
                                        , arguments  = ()
                                        , body       = Return expression
                                        }
                    program  = Program function
                    assembly = asAssembly option program
                assembly `shouldBe` "\t.globl\tmain\nmain:\n\tmovl\t$124, %eax\n\tneg\t%eax\n\tretq\n"

            it "translates programs with bitwise complement" $ do
                let expression = UnaryExpression BitwiseComplement $ Int32 124
                    function = Function { returnType = Int
                                        , identifier = "main"
                                        , arguments  = ()
                                        , body       = Return expression
                                        }
                    program  = Program function
                    assembly = asAssembly option program
                assembly `shouldBe` "\t.globl\tmain\nmain:\n\tmovl\t$124, %eax\n\tnot\t%eax\n\tretq\n"

            it "translates programs with logical negation" $ do
                let expression = UnaryExpression LogicalNegation $ Int32 124
                    function = Function { returnType = Int
                                        , identifier = "main"
                                        , arguments  = ()
                                        , body       = Return expression
                                        }
                    program  = Program function
                    assembly = asAssembly option program
                assembly `shouldBe` "\t.globl\tmain\nmain:\n\tmovl\t$124, %eax\n\tcmpl\t$0, %eax\n\tsete\t%al\n\tretq\n"
