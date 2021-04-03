module Ast where

import Data.Int ( Int32 )

newtype Program = Program Function
    deriving Show

data Function = Function
    { returnType :: Type
    , identifier :: String
    , arguments  :: ()
    , body       :: Statement
    } deriving Show

data Type = Int
    deriving Show

newtype Statement = Return Expression
    deriving Show

newtype Expression = Int32 Int32
    deriving Show