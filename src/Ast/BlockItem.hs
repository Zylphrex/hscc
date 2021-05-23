module Ast.BlockItem ( BlockItem(..), Statement(..) ) where

import Control.Applicative as A ( Alternative(empty, (<|>)), many )
import Control.Monad ( mapM, when )
import Data.Maybe ( fromJust, isJust )
import Text.PrettyPrint as P ( char
                             , empty
                             , equals
                             , nest
                             , space
                             , text
                             , vcat
                             , ($$)
                             )

import Ast.Expression ( Expression )
import Ast.Identifier ( Identifier, fromIdentifier )
import Ast.Type ( Type, bytes )
import Compiler ( Compiler(Compiler)
                , Compile(compile)
                , getSymbols
                , runCompiler
                , isDeclared
                , pushFrame
                )
import Parser ( Parse(parse)
              , parseCharacter
              , parseSpaces
              , parseString
              , parseNotNull
              )
import Pretty ( PrettyPrint(prettyPrint) )

data BlockItem = StatementItem Statement
               | DeclarationItem Type Identifier (Maybe Expression)
    deriving (Eq, Show)

instance Parse BlockItem where
    parse = StatementItem <$> parse
        <|> DeclarationItem <$> parse
                            <*> (parseNotNull parseSpaces *> parse)
                            <*> ( ( ( pure <$> (  parseSpaces
                                               *> parseCharacter '='
                                               *> parseSpaces
                                               *> parse
                                               )
                                  ) <|> pure A.empty )
                                  <* parseSpaces
                                  <* parseCharacter ';'
                                )

instance Compile BlockItem where
    compile (StatementItem statement) = compile statement
    compile (DeclarationItem variableType identifier mExpression) = Compiler $ do
        expression' <- if isJust mExpression
                       then runCompiler $ compile $ fromJust mExpression
                       else pure []
        let identifier' = fromIdentifier identifier
        alreadyDeclared <- isDeclared identifier'
        when alreadyDeclared $ fail $ "Variable: " ++ identifier' ++ " is already declared"
        pushFrame identifier' $ bytes variableType
        return $ expression'
              ++ [ "\tpush\t%rax" ]

instance PrettyPrint BlockItem where
    prettyPrint (StatementItem statement) = prettyPrint statement
    prettyPrint (DeclarationItem variableType identifier mExpression) =
         prettyPrint variableType
      <> P.space
      <> P.text (fromIdentifier identifier)
      <> if isJust mExpression
         then  P.space
            <> P.equals
            <> P.space
            <> prettyPrint (fromJust mExpression)
         else P.empty

data Statement = Return Expression
               | Expression Expression
               | Conditional Expression Statement (Maybe Statement)
    deriving (Eq, Show)

instance Parse Statement where
    parse = Return <$> (  parseString "return"
                       *> parseNotNull parseSpaces
                       *> parse
                       <* parseSpaces
                       <* parseCharacter ';'
                       )
        <|> Expression <$> parse <* parseSpaces <* parseCharacter ';'
        <|> Conditional <$> (  parseString "if"
                            *> parseSpaces
                            *> parseCharacter '('
                            *> parseSpaces
                            *> parse
                            <* parseSpaces
                            <* parseCharacter ')'
                            )
                        <*> ( parseSpaces *> parse )
                        <*> ( ( pure <$> (  parseNotNull parseSpaces
                                         *> parseString "else"
                                         *> parseNotNull parseSpaces
                                         *> parse
                                         )
                            ) <|> pure A.empty )

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
    compile (Conditional expression statement Nothing) = Compiler $ do
        expression' <- runCompiler $ compile expression
        statement' <- runCompiler $ compile statement
        [end] <- getSymbols ["_if_end"]
        return $ expression'
              ++ [ "\tcmpq\t$0, %rax"
                 , "\tje " ++ end
                 ]
              ++ statement'
              ++ [ end ++ ":" ]
    compile (Conditional expression statement1 (Just statement2)) = Compiler $ do
        expression' <- runCompiler $ compile expression
        statement1' <- runCompiler $ compile statement1
        statement2' <- runCompiler $ compile statement2
        [false, end] <- getSymbols ["_if_false", "_if_end"]
        return $ expression'
              ++ [ "\tcmpq\t$0, %rax"
                 , "\tje " ++ false
                 ]
              ++ statement1'
              ++ [ "\tjmp " ++ end
                 , false ++ ":"
                 ]
              ++ statement2'
              ++ [ end ++ ":" ]


instance PrettyPrint Statement where
    prettyPrint (Return expression) = text "RETURN" <> space <> prettyPrint expression
    prettyPrint (Expression expression) = prettyPrint expression
    prettyPrint (Conditional expression statement Nothing) =
        text "IF" <> space <> prettyPrint expression $$ nest 4 (prettyPrint statement)
    prettyPrint (Conditional expression statement1 (Just statement2)) =
        vcat [ text "IF" <> space <> prettyPrint expression $$ nest 4 (prettyPrint statement1)
             , text "ELSE" $$ nest 4 (prettyPrint statement2)
             ]
