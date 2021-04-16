{-# HLINT ignore "Reduce duplication" #-}
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
