module Ast.Type ( Type(..), bytes ) where

import Text.PrettyPrint ( text )

import Parser ( Parse(parse), parseString )
import Pretty ( PrettyPrint(prettyPrint) )

data Type = Int
    deriving (Eq, Show)

instance Parse Type where
    parse = Int <$ parseString "int"

instance PrettyPrint Type where
    prettyPrint Int = text "INT"

bytes :: Type -> Int
bytes Int = 8
