module Ast.Statement ( Statement(..) ) where

import Control.Applicative ( Alternative((<|>)) )
import Text.PrettyPrint ( space, text )

import Ast.Expression ( Expression )
import Ast.Identifier ( Identifier, fromIdentifier )
import Ast.Type ( Type, bytes )
import Compiler ( Compiler(Compiler)
                , Compile(compile)
                , runCompiler
                )
import Parser ( Parse(parse)
              , parseCharacter
              , parseSpaces
              , parseString
              , parseNotNull
              )
import Pretty ( PrettyPrint(prettyPrint) )

data Statement = Return Expression
               | Expression Expression
    deriving (Eq, Show)

instance Parse Statement where
    parse = Return <$> (  parseString "return"
                       *> parseNotNull parseSpaces
                       *> parse
                       <* parseSpaces
                       <* parseCharacter ';'
                       )
        <|> Expression <$> parse <* parseSpaces <* parseCharacter ';'

instance Compile Statement where
    compile (Return expression) = Compiler $ do
        expression' <- runCompiler $ compile expression
        return $ expression'
                 -- restore the current stackframe pointer
              ++ [ "\tmovq\t%rbp, %rsp"
                 -- restore the stackframe base pointer
                 , "\tpop\t%rbp"
                 , "\tretq"
                 ]
    compile (Expression expression) = Compiler $ runCompiler $ compile expression

instance PrettyPrint Statement where
    prettyPrint (Return expression) = text "RETURN" <> space <> prettyPrint expression
    prettyPrint (Expression expression) = prettyPrint expression
