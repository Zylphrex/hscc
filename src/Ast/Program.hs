module Ast.Program ( Program(..) ) where

import Assembly ( Assembly(toAssembly) )
import Ast.Function ( Function )
import Parser ( Parse(parse), parseSpaces )
import Pretty ( PrettyPrint(prettyPrint) )

newtype Program = Program Function
    deriving (Eq, Show)

instance Parse Program where
    parse = Program <$> parse <* parseSpaces

instance Assembly Program where
    toAssembly opt (Program function) = toAssembly opt function

instance PrettyPrint Program where
    prettyPrint (Program function) = prettyPrint function
