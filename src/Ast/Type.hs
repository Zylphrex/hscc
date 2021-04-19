module Ast.Type ( Type(..) ) where

import Text.PrettyPrint ( text )

import Parser ( Parse(parse), parseString )
import Pretty ( PrettyPrint(prettyPrint) )

data Type = Int
    deriving Show

instance Parse Type where
    parse = Int <$ parseString "int"

instance Eq Type where
    Int == Int = True

instance PrettyPrint Type where
    prettyPrint Int = text "INT"
