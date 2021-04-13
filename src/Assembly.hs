module Assembly where

import Ast ( Program(Program)
           , Function(Function)
           , Statement(Return)
           , Expression(Int32, UnaryExpression)
           , UnaryOperator(Negation, BitwiseComplement, LogicalNegation)
           )

data OsOption = Darwin | Other

newtype Option = Option { osOption :: OsOption }

class Assembly a where
    asAssembly :: Option -> a -> String

instance Assembly Program where
    asAssembly opt (Program function) = asAssembly opt function

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
        asAssembly opt expression ++ "\tretq\n"

instance Assembly Expression where
    asAssembly _ (Int32 value) =
        "\tmovl\t$" ++ show value ++ ", %eax\n"
    asAssembly opt (UnaryExpression Negation expression) =
        asAssembly opt expression ++ "\tneg\t%eax\n"
    asAssembly opt (UnaryExpression BitwiseComplement expression) =
        asAssembly opt expression ++ "\tnot\t%eax\n"
    asAssembly opt (UnaryExpression LogicalNegation expression) =
        asAssembly opt expression ++ "\tcmpl\t$0, %eax\n\tsete\t%al\n"
