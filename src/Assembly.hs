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
        joinAssembly [ "\t.globl\t" ++ alias
                     , alias ++ ":"
                     , asAssembly opt body
                     ]
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
        joinAssembly [ "\tmovq\t$" ++ show value ++ ", %rax" ]
    asAssembly opt (UnaryExpression Negation exp) =
        joinAssembly [ asAssembly opt exp
                     , "\tnegq\t%rax"
                     ]
    asAssembly opt (UnaryExpression BitwiseComplement exp) =
        joinAssembly [ asAssembly opt exp
                     , "\tnotq\t%rax"
                     ]
    asAssembly opt (UnaryExpression LogicalNegation exp) =
        joinAssembly [ asAssembly opt exp
                     , "\tcmpq\t$0, %rax"
                     , "\tmovq\t$0, %rax"
                     , "\tsete\t%al"
                     ]

joinAssembly :: [String] -> String
joinAssembly = unlines . map (reverse . dropWhile (== '\n') . reverse)
