module Ast.Statement ( Statement(..) ) where

import Control.Monad ( when )
import Control.Monad.State ( get, put )
import Control.Applicative ( Alternative((<|>)) )
import Data.Maybe ( fromJust, isJust )
import Text.PrettyPrint ( empty, equals, space, text )

import Ast.Expression ( Expression )
import Compiler ( Compiler(Compiler)
                , Compile(compile)
                , runCompiler
                , stackFrame
                , stackIndex
                , isDeclared
                , pushFrame
                )
import Ast.Identifier ( Identifier, fromIdentifier )
import Ast.Type ( Type, bytes )
import Parser ( Parse(parse)
              , parseCharacter
              , parseSpaces
              , parseString
              , parseNotNull
              )
import Pretty ( PrettyPrint(prettyPrint) )

data Statement = Return Expression
               | Expression Expression
               | Declaration Type Identifier (Maybe Expression)
    deriving (Eq, Show)

instance Parse Statement where
    parse = Return <$> (  parseString "return"
                       *> parseNotNull parseSpaces
                       *> parse
                       <* parseSpaces
                       <* parseCharacter ';'
                       )
        <|> Expression <$> parse <* parseSpaces <* parseCharacter ';'
        <|> toStatement <$> parse

data DeclarationWithExp = DeclarationWithExp Type Identifier Expression
                        | DeclarationWithoutExp Type Identifier

instance Parse DeclarationWithExp where
    parse = DeclarationWithExp <$> parse
                               <*> (parseNotNull parseSpaces *> parse)
                               <*> (  parseSpaces
                                   *> parseCharacter '='
                                   *> parseSpaces
                                   *> parse
                                   <* parseSpaces
                                   <* parseCharacter ';'
                                   )
        <|> DeclarationWithoutExp <$> parse
                                  <*> (  parseNotNull parseSpaces
                                      *> parse
                                      <* parseSpaces
                                      <* parseCharacter ';'
                                      )

toStatement :: DeclarationWithExp -> Statement
toStatement (DeclarationWithExp t i e) = Declaration t i $ Just e
toStatement (DeclarationWithoutExp t i) = Declaration t i Nothing

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
    compile (Declaration variableType identifier mExpression) = Compiler $ do
        let hasExpression = isJust mExpression
        expression' <- if hasExpression
                       then runCompiler $ compile $ fromJust mExpression
                       else pure []
        state <- get
        let identifier' = fromIdentifier identifier
            stackFrame' = stackFrame state
            stackIndex' = stackIndex state
        when (isDeclared identifier' stackFrame')
             (fail $ "Variable: " ++ identifier' ++ " is not declared")
        let stackIndex'' = stackIndex' - bytes variableType
            stackFrame'' = pushFrame (identifier', stackIndex'') stackFrame'
        put $ state { stackFrame = stackFrame''
                    , stackIndex = stackIndex''
                    }
        if hasExpression
        then return $ expression'
                   ++ [ "\tpush\t%rax" ]
        else return $ [ "\tpush\t%rax" ]

instance PrettyPrint Statement where
    prettyPrint (Return expression) = text "RETURN" <> space <> prettyPrint expression
    prettyPrint (Expression expression) = prettyPrint expression
    prettyPrint (Declaration variableType identifier mExpression) =
         prettyPrint variableType
      <> space
      <> text (fromIdentifier identifier)
      <> if isJust mExpression
         then  space
            <> equals
            <> space
            <> prettyPrint (fromJust mExpression)
         else empty
