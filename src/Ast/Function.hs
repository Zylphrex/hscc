module Ast.Function ( Function(..) ) where

import Control.Applicative ( Alternative((<|>)), many )
import Control.Monad.State ( get, put )
import Data.Char ( isDigit, isLetter )
import Data.Functor (($>))
import Text.PrettyPrint ( colon, empty, nest, parens, space, text, vcat, ($$) )

import Ast.Identifier ( Identifier )
import Ast.Expression ( Expression(Int32) )
import Ast.Statement ( Statement(Return) )
import Ast.Type ( Type )
import Compiler ( Compiler(Compiler)
                , Compile(compile)
                , runCompiler
                , stackIndex
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
                         , body       :: [Statement]
                         } deriving (Eq, Show)

instance Parse Function where
    parse = Function <$> parse
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
                         *> ( parseNotNull (many (parseSpaces *> parse <* parseSpaces))
                          <|> parseSpaces $> [Return $ Int32 0]
                            )
                         <* parseCharacter '}'
                         )

instance Compile Function where
    compile (Function returnType identifier arguments body) = Compiler $ do
        -- set the stack index to 0 when entering a function
        state <- get
        put $ state { stackIndex = 0}
        identifier' <- runCompiler $ compile identifier
        body' <- runCompiler $ traverse compile body
        -- restore the stack frame and index when exiting a function
        put state
        return $ concat $ [ "\t.globl\t" ++ head identifier'
                          , head identifier' ++ ":"
                          -- save the stackframe base pointer
                          , "\tpush\t%rbp"
                          -- update the stackframe base pointer
                          -- to the current stackframe pointer
                          , "\tmovq\t%rsp, %rbp"
                          ]
                        : body'

instance PrettyPrint Function where
    prettyPrint (Function returnType identifier arguments body) =
      text "FUN" <> space <> prettyPrint returnType <> space <> prettyPrint identifier <> colon $$
      nest 4 (
          (text "params" <> colon <> space <> parens empty) $$
          (text "body" <> colon $$ nest 4 (vcat (prettyPrint <$> body)))
      )
