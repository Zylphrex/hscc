module Ast.Program ( Program(..) ) where

import Control.Applicative ( some )
import Control.Monad ( mapM )
import Text.PrettyPrint ( vcat )

import Ast.Function ( Function )
import Compiler ( Compiler(Compiler), Compile(compile) )
import Parser ( Parse(parse), parseSpaces )
import Pretty ( PrettyPrint(prettyPrint) )

newtype Program = Program [Function]
    deriving (Eq, Show)

instance Parse Program where
    parse = Program <$> some (parseSpaces *> parse <* parseSpaces)

instance Compile Program where
    compile (Program functions) = concat <$> mapM compile functions

instance PrettyPrint Program where
    prettyPrint (Program functions) = vcat $ map prettyPrint functions
