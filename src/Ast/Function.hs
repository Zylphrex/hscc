module Ast.Function ( Function(..) ) where

import Data.Char ( isDigit, isLetter )
import Data.Functor (($>))
import Text.PrettyPrint ( colon, empty, nest, parens, space, text, ($$) )

import Ast.Identifier ( Identifier )
import Ast.Statement ( Statement )
import Ast.Type ( Type )
import Compiler ( Compiler(Compiler)
                , Compile(compile)
                , runCompiler
                )
import Parser ( Parse(parse)
              , Parser
              , parseCharacter
              , parseIf
              , parseNotNull
              , parseSpaces
              , parseWhile
              )
import Pretty ( PrettyPrint(prettyPrint) )

data Function = Function { returnType :: Type
                         , identifier :: Identifier
                         , arguments  :: ()
                         , body       :: Statement
                         } deriving (Eq, Show)

instance Parse Function where
    parse = Function <$> (parseSpaces *> parse)
                     <*> (parseNotNull parseSpaces *> parse)
                     <*> (  parseSpaces
                         *> parseCharacter '('
                         *> parseSpaces
                         $> () -- TODO: parse the arguments
                         <* parseSpaces
                         <* parseCharacter ')'
                         <* parseSpaces
                         )
                     <*> (  parseCharacter '{'
                         *> parseSpaces
                         *> parse
                         <* parseSpaces
                         <* parseCharacter '}'
                         )

instance Compile Function where
    compile (Function returnType identifier arguments body) = Compiler $ do
        identifier' <- runCompiler $ compile identifier
        body' <- runCompiler $ compile body
        return $ [ "\t.globl\t" ++ head identifier'
                 , head identifier' ++ ":"
                 ]
              ++ body'

instance PrettyPrint Function where
    prettyPrint (Function returnType identifier arguments body) =
      text "FUN" <> space <> prettyPrint returnType <> space <> prettyPrint identifier <> colon $$
      nest 4 (
          (text "params" <> colon <> space <> parens empty) $$
          (text "body" <> colon $$ nest 4 (prettyPrint body))
      )
