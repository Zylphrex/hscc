module Ast.Function ( Argument(..), Function(..) ) where

import Control.Applicative ( Alternative((<|>)), many )
import Control.Monad ( forM_ )
import Data.Char ( isDigit, isLetter )
import Data.Functor (($>))
import Data.List (intersperse)
import Text.PrettyPrint ( colon, empty, hcat, nest, parens, space, text, vcat, ($$) )

import Ast.Identifier ( Identifier, fromIdentifier )
import Ast.Expression ( Expression(Int64) )
import Ast.BlockItem ( BlockItem(StatementItem)
                     , Declaration(Declaration)
                     , Statement(Return)
                     )
import Ast.Type ( Type, bytes, fromType )
import Compiler ( Compiler(Compiler)
                , Compile(compile)
                , getState
                , setState
                , runCompiler
                , pushArgument
                , pushFunctionDeclaration
                , pushFunctionPrototype
                , resetFunctionArgs
                , setFunctionArgs
                , resetStackFrame
                , setStackFrame
                , popDeclared
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
                         , arguments  :: [Argument]
                         , body       :: [BlockItem]
                         }
              | FunctionPrototype { returnType :: Type
                                  , identifier :: Identifier
                                  , arguments  :: [Argument]
                                  }
              deriving (Eq, Show)

instance Parse Function where
    parse = Function <$> parse
                     <*> (parseNotNull parseSpaces *> parse)
                     <*> (  parseSpaces
                         *> parseCharacter '('
                         *> parseSpaces
                         *> (flatten <$> parse)
                         <* parseSpaces
                         <* parseCharacter ')'
                         <* parseSpaces
                         )
                     <*> (  parseCharacter '{'
                         *> ( parseNotNull (many (parseSpaces *> parse <* parseSpaces))
                          <|> parseSpaces $> [StatementItem $ Return $ Int64 0]
                            )
                         <* parseCharacter '}'
                         )
        <|> FunctionPrototype <$> parse
                              <*> (parseNotNull parseSpaces *> parse)
                              <*> (  parseSpaces
                                  *> parseCharacter '('
                                  *> parseSpaces
                                  *> (flatten <$> parse)
                                  <* parseSpaces
                                  <* parseCharacter ')'
                                  <* parseSpaces
                                  <* parseCharacter ';'
                                  )

instance Compile Function where
    compile functionDeclaration@(Function returnType identifier arguments body) = Compiler $ do
        -- TODO: check the argument types
        -- resets the stack frame as we enter a function call
        functionArgs <- resetFunctionArgs
        stackFrame <- resetStackFrame
        let (returnType', identifier', arguments') = formatSignature functionDeclaration
        forM_ (zip [0..] arguments) $ \(index, Argument argType argIdentifier) ->
            pushArgument index (fromIdentifier argIdentifier) (bytes argType)
        pushFunctionDeclaration returnType' identifier' arguments'
        identifier' <- runCompiler $ compile identifier
        body' <- runCompiler $ traverse compile body
        -- make sure to free the function arguments from the stack
        clear <- popDeclared
        setFunctionArgs functionArgs
        -- restore the stack frame and index when returning from a function
        setStackFrame stackFrame
        return $ [ "\t.globl\t" ++ head identifier'
                 , head identifier' ++ ":"
                 -- save the stackframe base pointer
                 , "\tpush\t%rbp"
                 -- update the stackframe base pointer
                 -- to the current stackframe pointer
                 , "\tmovq\t%rsp, %rbp"
                 ]
              ++ concat body'
                 -- deallocate location variables in stack
              ++ [ "\tmovq\t%rbp, %rsp"
                 -- restore stackframe base pointer
                 , "\tpop\t%rbp"
                 ]
              ++ clear
              ++ [ "\tretq" ]
    compile functionPrototype = Compiler $ do
        let (returnType', identifier', arguments') = formatSignature functionPrototype
        pushFunctionPrototype returnType' identifier' arguments'
        return []

formatSignature :: Function -> (String, String, [(String, String)])
formatSignature (Function returnType identifier arguments _) =
    (fromType returnType, fromIdentifier identifier, fmap formatArgument arguments)
formatSignature (FunctionPrototype returnType identifier arguments) =
    (fromType returnType, fromIdentifier identifier, fmap formatArgument arguments)

formatArgument :: Argument -> (String, String)
formatArgument (Argument argumentType argumentIdentifier) =
    (fromType argumentType, fromIdentifier argumentIdentifier)

instance PrettyPrint Function where
    prettyPrint (Function returnType identifier arguments body) =
        text "FUN" <> space <> prettyPrint returnType <> space <> prettyPrint identifier <> colon $$
        nest 4 (
            (text "params" <> colon <> space <> parens arguments') $$
            (text "body" <> colon $$ nest 4 (vcat (prettyPrint <$> body)))
        )
      where arguments' = hcat $ intersperse (text ", ") $ map prettyPrint arguments
    prettyPrint (FunctionPrototype returnType identifier arguments) =
        text "PROTO" <> space <> prettyPrint returnType <> space <> prettyPrint identifier <> colon $$
        nest 4 (text "params" <> colon <> space <> parens arguments')
      where arguments' = hcat $ intersperse (text ", ") $ map prettyPrint arguments

data Argument = Argument Type Identifier
    deriving (Eq, Show)

instance Parse Argument where
    parse = Argument <$> (parse <* parseSpaces) <*> (parse <* parseSpaces)

instance PrettyPrint Argument where
    prettyPrint (Argument argumentType argumentIdentifier) =
        prettyPrint argumentType <> space <> prettyPrint argumentIdentifier

data RawArguments = RawArguments Argument [Argument]
                  | NoRawArguments

instance Parse RawArguments where
    parse = RawArguments <$> parse <*> many parseRest <|> pure NoRawArguments
        where parseRest = parseCharacter ',' *> parseSpaces *> parse

flatten :: RawArguments -> [Argument]
flatten (RawArguments arg rest) = arg:rest
flatten NoRawArguments = []
