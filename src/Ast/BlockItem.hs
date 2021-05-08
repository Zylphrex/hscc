module Ast.BlockItem ( BlockItem(..) ) where

import Control.Monad ( when )
import Control.Monad.State ( get, put )
import Control.Applicative as A ( Alternative(empty, (<|>)) )
import Data.Maybe ( fromJust, isJust )
import Text.PrettyPrint as P ( empty, equals, space, text )

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
        <|> Declaration <$> parse
                        <*> (parseNotNull parseSpaces *> parse)
                        <*> ( ( ( pure <$> (  parseSpaces
                                           *> parseCharacter '='
                                           *> parseSpaces
                                           *> parse
                                           )
                              ) <|> (pure A.empty) )
                              <* parseSpaces
                              <* parseCharacter ';'
                            )

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
      <> P.space
      <> P.text (fromIdentifier identifier)
      <> if isJust mExpression
         then  P.space
            <> P.equals
            <> P.space
            <> prettyPrint (fromJust mExpression)
         else P.empty
