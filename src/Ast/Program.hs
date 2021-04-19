module Ast.Program ( Program(..) ) where

import Assembly ( Assembly(toAssembly) )
import Ast.Function ( Function )
import Parser ( Parse(parse), parseSpaces )
import Pretty ( PrettyPrint(prettyPrint) )

newtype Program = Program Function
    deriving Show

instance Parse Program where
    parse = Program <$> parse <* parseSpaces

instance Eq Program where
    Program p1 == Program p2 = p1 == p2

instance Assembly Program where
    toAssembly opt (Program function) = toAssembly opt function

instance PrettyPrint Program where
    prettyPrint (Program function) = prettyPrint function
