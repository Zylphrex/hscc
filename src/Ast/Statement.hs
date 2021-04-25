module Ast.Statement ( Statement(..) ) where

import Text.PrettyPrint ( space, text )

import Ast.Expression ( Expression )
import Compiler ( Compiler(Compiler)
                , Compile(compile)
                , runCompiler
                )
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

instance Compile Statement where
    compile (Return exp) = Compiler $ do
        exp' <- runCompiler $ compile exp
        return $ exp' ++ ["\tretq"]

instance PrettyPrint Statement where
    prettyPrint (Return expression) = text "RETURN" <> space <> prettyPrint expression
