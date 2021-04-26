module Ast.Program ( Program(..) ) where

import Ast.Function ( Function )
import Compiler ( Compile(compile) )
import Parser ( Parse(parse), parseSpaces )
import Pretty ( PrettyPrint(prettyPrint) )

newtype Program = Program Function
    deriving (Eq, Show)

instance Parse Program where
    parse = Program <$> (parseSpaces *> parse <* parseSpaces)

instance Compile Program where
    compile (Program function) = compile function

instance PrettyPrint Program where
    prettyPrint (Program function) = prettyPrint function
