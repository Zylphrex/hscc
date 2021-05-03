module Ast.BlockItem ( BlockItem(..) ) where

import Control.Monad ( when )
import Control.Monad.State ( get, put )
import Control.Applicative ( Alternative((<|>)) )
import Data.Maybe ( fromJust, isJust )
import Text.PrettyPrint ( empty, equals, space, text )

import Ast.Expression ( Expression )
import Ast.Identifier ( Identifier, fromIdentifier )
import Ast.Statement ( Statement )
import Ast.Type ( Type, bytes )
import Compiler ( Compiler(Compiler)
                , Compile(compile)
                , runCompiler
                , stackFrame
                , stackIndex
                , isDeclared
                , pushFrame
                )
import Parser ( Parse(parse)
              , parseCharacter
              , parseSpaces
              , parseNotNull
              )
import Pretty ( PrettyPrint(prettyPrint) )

data BlockItem = Statement Statement
               | Declaration Type Identifier (Maybe Expression)
    deriving (Eq, Show)

instance Parse BlockItem where
    parse = Statement <$> parse
        <|> fromDeclaration <$> parse

data Declaration = DeclarationWithExp Type Identifier Expression
                 | DeclarationWithoutExp Type Identifier

instance Parse Declaration where
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

fromDeclaration :: Declaration -> BlockItem
fromDeclaration (DeclarationWithExp t i e) = Declaration t i $ Just e
fromDeclaration (DeclarationWithoutExp t i) = Declaration t i Nothing

instance Compile BlockItem where
    compile (Statement statement) = compile statement
    compile (Declaration variableType identifier mExpression) = Compiler $ do
        expression' <- if isJust mExpression
                       then runCompiler $ compile $ fromJust mExpression
                       else pure []
        state <- get
        let identifier' = fromIdentifier identifier
            stackFrame' = stackFrame state
            stackIndex' = stackIndex state
        when (isDeclared identifier' stackFrame')
             (fail $ "Variable: " ++ identifier' ++ " is already declared")
        let stackIndex'' = stackIndex' - bytes variableType
            stackFrame'' = pushFrame (identifier', stackIndex'') stackFrame'
        put $ state { stackFrame = stackFrame''
                    , stackIndex = stackIndex''
                    }
        return $ expression'
              ++ [ "\tpush\t%rax" ]

instance PrettyPrint BlockItem where
    prettyPrint (Statement statement) = prettyPrint statement
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
