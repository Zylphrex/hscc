module AssemblySpec ( spec ) where

import Test.Hspec

import Ast ( Expression(Int32)
           , Function(Function)
           , Program(Program)
           , Statement(Return)
           , Type(Int)
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
        it "transalates simple program into assembly for darwin" $ do
            let function = Function { returnType = Int
                                    , identifier = "main"
                                    , arguments  = ()
                                    , body       = Return $ Int32 124
                                    }
                program  = Program function
                option   = Option { osOption = Darwin }
                assembly = asAssembly option program
            assembly `shouldBe` "\t.globl\t_main\n_main:\n\tmovl\t$124, %eax\n\tret\n"

        it "transalates simple program into assembly for other" $ do
            let function = Function { returnType = Int
                                    , identifier = "main"
                                    , arguments  = ()
                                    , body       = Return $ Int32 124
                                    }
                program  = Program function
                option   = Option { osOption = Other }
                assembly = asAssembly option program
            assembly `shouldBe` "\t.globl\tmain\nmain:\n\tmovl\t$124, %eax\n\tret\n"
