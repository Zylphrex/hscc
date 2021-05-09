module Ast.Statement ( Statement(..) ) where

import Control.Applicative ( Alternative(empty, (<|>)) )
import Control.Monad.State ( get, put )
import Text.PrettyPrint ( nest, space, text, vcat, ($$) )

import Ast.Expression ( Expression )
import Ast.Identifier ( Identifier, fromIdentifier )
import Ast.Type ( Type, bytes )
import Compiler ( Compiler(Compiler)
                , Compile(compile)
                , runCompiler
                , n
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
                        <*> ( parseSpaces *> parse)
                        <*> ( ( pure <$> (  parseNotNull parseSpaces
                                         *> parseString "else"
                                         *> parseNotNull parseSpaces
                                         *> parse
                                         )
                            ) <|> pure empty )

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
        s <- get
        let i   = n s
            end = "_if_end" ++ show i
        put $ s { n = i + 1 }
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
        s <- get
        let i     = n s
            false = "_if_false" ++ show i
            end   = "_if_end" ++ show i
        put $ s { n = i + 1 }
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
