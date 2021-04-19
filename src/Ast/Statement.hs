module Ast.Statement ( Statement(..) ) where

import Text.PrettyPrint ( space, text )

import Assembly ( Assembly(toAssembly) )
import Ast.Expression ( Expression )
import Parser ( Parse(parse)
              , parseCharacter
              , parseSpaces
              , parseString
              )
import Pretty ( PrettyPrint(prettyPrint) )

newtype Statement = Return Expression
    deriving (Eq, Show)

instance Parse Statement where
    parse = Return <$> (  parseSpaces
                       *> parseString "return"
                       *> parseSpaces
                       *> parse
                       <* parseSpaces
                       <* parseCharacter ';'
                       )

instance Assembly Statement where
    toAssembly opt (Return expression) =
        toAssembly opt expression ++ "\tretq\n"

instance PrettyPrint Statement where
    prettyPrint (Return expression) = text "RETURN" <> space <> prettyPrint expression
