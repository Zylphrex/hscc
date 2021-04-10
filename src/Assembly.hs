module Assembly where

import Ast ( Program(Program)
           , Function(Function)
           , Statement(Return)
           , Expression(Int32)
           )

data OsOption = Darwin | Other

newtype Option = Option { osOption :: OsOption }

class Assembly a where
    asAssembly :: Option -> a -> String

instance Assembly Program where
    asAssembly opt (Program function) = asAssembly opt function ++ "\n"

instance Assembly Function where
    asAssembly opt (Function returnType identifier arguments body) =
        "\t.globl\t" ++ alias ++ "\n" ++ alias ++ ":\n" ++ asAssembly opt body
        where alias = makeAlias opt identifier

makeAlias :: Option -> String -> String
makeAlias opt identifier = case osOption opt of
    Darwin -> '_' : identifier
    _      -> identifier

instance Assembly Statement where
    asAssembly opt (Return expression) =
        "\tmovl\t" ++ asAssembly opt expression ++ ", %eax\n\tret"

instance Assembly Expression where
    asAssembly _ (Int32 value) = '$' : show value
