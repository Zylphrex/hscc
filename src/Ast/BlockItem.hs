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
                , getStackFrame
                , setStackFrame
                , clearDeclared
                , popDeclared
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
               | Conditional Expression Statement (Maybe Statement)
               | Compound [BlockItem]
               | While Expression Statement
               | DoWhile Statement Expression
               | Expression (Maybe Expression)
    deriving (Eq, Show)

instance Parse Statement where
    parse = Return <$> (  parseString "return"
                       *> parseNotNull parseSpaces
                       *> parse
                       <* parseSpaces
                       <* parseCharacter ';'
                       )
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
        <|> Compound <$> (  parseCharacter '{'
                         *> many (parseSpaces *> parse <* parseSpaces)
                         <* parseCharacter '}'
                         )
        <|> While <$> (  parseString "while"
                      *> parseSpaces
                      *> parseCharacter '('
                      *> parseSpaces
                      *> parse
                      <* parseSpaces
                      <* parseCharacter ')'
                      )
                  <*> ( parseSpaces *> parse )
        <|> DoWhile <$> (  parseString "do"
                        *> parseSpaces
                        *> parse
                        )
                    <*> (  parseSpaces
                        *> parseString "while"
                        *> parseSpaces
                        *> parseCharacter '('
                        *> parseSpaces
                        *> parse
                        <* parseSpaces
                        <* parseCharacter ')'
                        <* parseSpaces
                        <* parseCharacter ';'
                        )
        <|> Expression <$> ( Just <$> parse <|> pure Nothing ) <* parseSpaces
                           <* parseCharacter ';'

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
    compile (Expression (Just expression)) = Compiler $ runCompiler $ compile expression
    compile (Expression Nothing) = Compiler $ return []
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
    compile (Compound items) = Compiler $ do
        -- clear the set of declared names in the current scope
        -- this allows for shadowing of declared names
        stackFrame <- getStackFrame
        clearDeclared
        items' <- concat <$> (runCompiler . compile) `mapM` items
        clear <- popDeclared
        -- restore the set of declared names to what it was prior
        -- to this compound statement
        setStackFrame stackFrame
        return $ items' ++ clear
    compile (While expression statement) = Compiler $ do
        expression' <- runCompiler $ compile expression
        statement' <- runCompiler $ compile statement
        [cond, end] <- getSymbols ["_while_cond", "_while_end"]
        return $ [ cond ++ ":" ]
              ++ expression'
              ++ [ "\tcmpq\t$0, %rax"
                 , "\tje " ++ end
                 ]
              ++ statement'
              ++ [ "\tjmp " ++ cond
                 , end ++ ":"
                 ]
    compile (DoWhile statement expression) = Compiler $ do
        statement' <- runCompiler $ compile statement
        expression' <- runCompiler $ compile expression
        [start, cond, end] <- getSymbols ["_do_start", "_do_cond", "_do_end"]
        return $ [ start ++ ":" ]
              ++ statement'
              ++ [ cond ++ ":" ]
              ++ expression'
              ++ [ "\tcmpq\t$0, %rax"
                 , "\tjne " ++ start
                 , end ++ ":"
                 ]

instance PrettyPrint Statement where
    prettyPrint (Return expression) = text "RETURN" <> space <> prettyPrint expression
    prettyPrint (Expression (Just expression)) = prettyPrint expression
    prettyPrint (Expression Nothing) = text "NOOP"
    prettyPrint (Conditional expression statement elseStatement) = ifClause $$ elseClause
      where ifClause = case statement of
                Compound items -> vcat [ ifCondition <> space <> char '{'
                                       , nest 4 $ vcat $ map prettyPrint items
                                       , char '}'
                                       ]
                _ -> ifCondition $$ nest 4 (prettyPrint statement)
              where ifCondition = text "IF" <> space <> prettyPrint expression
            elseClause = case elseStatement of
                Nothing -> P.empty
                Just (Compound items) -> vcat [ text "ELSE" <> space <> char '{'
                                              , nest 4 $ vcat $ map prettyPrint items
                                              , char '}'
                                              ]
                Just s -> text "ELSE" $$ nest 4 (prettyPrint s)
    prettyPrint (Compound items) =
        vcat [ char '{'
             , nest 4 $ vcat $ map prettyPrint items
             , char '}'
             ]
    prettyPrint (While expression statement) =
        vcat [ text "WHILE" <> space <> prettyPrint expression
             , nest 4 $ prettyPrint statement
             ]
    prettyPrint (DoWhile statement expression) =
        vcat [ text "DO"
             , nest 4 $ prettyPrint statement
             , text "WHILE" <> space <> prettyPrint expression
             ]
