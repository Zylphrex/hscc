module Assembly where

import Ast ( Program(Program)
           , Function(Function)
           , Statement(Return)
           , Expression(Int32)
           )

class Assembly a where
    asAssembly :: a -> String

instance Assembly Program where
    asAssembly (Program function) = asAssembly function ++ "\n"

instance Assembly Function where
    asAssembly (Function returnType identifier arguments body) =
        "\t.globl\t" ++ identifier ++ "\n" ++ identifier ++ ":\n" ++ asAssembly body

instance Assembly Statement where
    asAssembly (Return expression) =
        "\tmovl\t" ++ asAssembly expression ++ ", %eax\n\tret"

instance Assembly Expression where
    asAssembly (Int32 value) = '$' : show value