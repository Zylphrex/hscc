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
import Assembly (asAssembly)

spec :: Spec
spec = do
    describe "Assembly" $ do
        it "transalates simple program into assembly" $ do
            let function = Function { returnType = Int
                                    , identifier = "main"
                                    , arguments  = ()
                                    , body       = Return $ Int32 124
                                    }
                program  = Program function
                assembly = asAssembly program
            assembly `shouldBe` "\t.globl\t_main\n_main:\n\tmovl\t$124, %eax\n\tret\n"
