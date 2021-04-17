{-# HLINT ignore "Reduce duplication" #-}
module AssemblySpec ( spec ) where

import Test.Hspec

import Ast ( Expression(Int32, UnaryExpression, BinaryExpression)
           , Function(Function)
           , Program(Program)
           , Statement(Return)
           , Type(Int)
           , UnaryOperator(Negation, BitwiseComplement, LogicalNegation)
           , BinaryOperator(Addition, Subtraction, Multiplication, Division)
           , returnType
           , identifier
           , arguments
           , body
           )
import Assembly ( OsOption(Darwin, Other)
                , Option(Option)
                , asAssembly
                , joinAssembly
                , osOption
                )

spec :: Spec
spec = describe "Assembly" $ do
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
            assembly `shouldBe` joinAssembly [ "\t.globl\t_main"
                                             , "_main:"
                                             , "\tmovq\t$124, %rax"
                                             , "\tretq"
                                             ]

        it "translates programs with negation" $ do
            let expression = UnaryExpression Negation $ Int32 124
                function = Function { returnType = Int
                                    , identifier = "main"
                                    , arguments  = ()
                                    , body       = Return expression
                                    }
                program  = Program function
                assembly = asAssembly option program
            assembly `shouldBe` joinAssembly [ "\t.globl\t_main"
                                             , "_main:"
                                             , "\tmovq\t$124, %rax"
                                             , "\tnegq\t%rax"
                                             , "\tretq"
                                             ]

        it "translates programs with bitwise complement" $ do
            let expression = UnaryExpression BitwiseComplement $ Int32 124
                function = Function { returnType = Int
                                    , identifier = "main"
                                    , arguments  = ()
                                    , body       = Return expression
                                    }
                program  = Program function
                assembly = asAssembly option program
            assembly `shouldBe` joinAssembly [ "\t.globl\t_main"
                                             , "_main:"
                                             , "\tmovq\t$124, %rax"
                                             , "\tnotq\t%rax"
                                             , "\tretq"
                                             ]

        it "translates programs with logical negation" $ do
            let expression = UnaryExpression LogicalNegation $ Int32 124
                function = Function { returnType = Int
                                    , identifier = "main"
                                    , arguments  = ()
                                    , body       = Return expression
                                    }
                program  = Program function
                assembly = asAssembly option program
            assembly `shouldBe` joinAssembly [ "\t.globl\t_main"
                                             , "_main:"
                                             , "\tmovq\t$124, %rax"
                                             , "\tcmpq\t$0, %rax"
                                             , "\tmovq\t$0, %rax"
                                             , "\tsete\t%al"
                                             , "\tretq"
                                             ]

        it "translates programs with addition" $ do
            let expression = BinaryExpression (Int32 4) Addition (Int32 2)
                function = Function { returnType = Int
                                    , identifier = "main"
                                    , arguments  = ()
                                    , body       = Return expression
                                    }
                program  = Program function
                assembly = asAssembly option program
            assembly `shouldBe` joinAssembly [ "\t.globl\t_main"
                                             , "_main:"
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
                                    , identifier = "main"
                                    , arguments  = ()
                                    , body       = Return expression
                                    }
                program  = Program function
                assembly = asAssembly option program
            assembly `shouldBe` joinAssembly [ "\t.globl\t_main"
                                             , "_main:"
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
                                    , identifier = "main"
                                    , arguments  = ()
                                    , body       = Return expression
                                    }
                program  = Program function
                assembly = asAssembly option program
            assembly `shouldBe` joinAssembly [ "\t.globl\t_main"
                                             , "_main:"
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
                                    , identifier = "main"
                                    , arguments  = ()
                                    , body       = Return expression
                                    }
                program  = Program function
                assembly = asAssembly option program
            assembly `shouldBe` joinAssembly [ "\t.globl\t_main"
                                             , "_main:"
                                             , "\tmovq\t$2, %rax"
                                             , "\tpush\t%rax"
                                             , "\tmovq\t$4, %rax"
                                             , "\tcqto"
                                             , "\tpop\t%rcx"
                                             , "\tidivq\t%rcx"
                                             , "\tretq"
                                             ]

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
            assembly `shouldBe` joinAssembly [ "\t.globl\tmain"
                                             , "main:"
                                             , "\tmovq\t$124, %rax"
                                             , "\tretq"
                                             ]

        it "translates programs with negation" $ do
            let expression = UnaryExpression Negation $ Int32 124
                function = Function { returnType = Int
                                    , identifier = "main"
                                    , arguments  = ()
                                    , body       = Return expression
                                    }
                program  = Program function
                assembly = asAssembly option program
            assembly `shouldBe` joinAssembly [ "\t.globl\tmain"
                                             , "main:"
                                             , "\tmovq\t$124, %rax"
                                             , "\tnegq\t%rax"
                                             , "\tretq"
                                             ]

        it "translates programs with bitwise complement" $ do
            let expression = UnaryExpression BitwiseComplement $ Int32 124
                function = Function { returnType = Int
                                    , identifier = "main"
                                    , arguments  = ()
                                    , body       = Return expression
                                    }
                program  = Program function
                assembly = asAssembly option program
            assembly `shouldBe` joinAssembly [ "\t.globl\tmain"
                                             , "main:"
                                             , "\tmovq\t$124, %rax"
                                             , "\tnotq\t%rax"
                                             , "\tretq"
                                             ]

        it "translates programs with logical negation" $ do
            let expression = UnaryExpression LogicalNegation $ Int32 124
                function = Function { returnType = Int
                                    , identifier = "main"
                                    , arguments  = ()
                                    , body       = Return expression
                                    }
                program  = Program function
                assembly = asAssembly option program
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
                                    , identifier = "main"
                                    , arguments  = ()
                                    , body       = Return expression
                                    }
                program  = Program function
                assembly = asAssembly option program
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
                                    , identifier = "main"
                                    , arguments  = ()
                                    , body       = Return expression
                                    }
                program  = Program function
                assembly = asAssembly option program
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
                                    , identifier = "main"
                                    , arguments  = ()
                                    , body       = Return expression
                                    }
                program  = Program function
                assembly = asAssembly option program
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
                                    , identifier = "main"
                                    , arguments  = ()
                                    , body       = Return expression
                                    }
                program  = Program function
                assembly = asAssembly option program
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
