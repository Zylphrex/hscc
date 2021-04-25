module Compiler ( Compiler(Compiler)
                , Compile(compile)
                , Os(Darwin, Other)
                , os
                , runCompiler
                , tryCompiler
                , executeCompiler
                ) where

import Control.Applicative ( Alternative((<|>), empty) )
import Control.Monad.State ( StateT(StateT)
                           , evalStateT
                           , mapStateT
                           , runStateT
                           )
import Data.Default

data Os = Darwin | Other deriving (Eq, Show)

newtype CompilerState = CompilerState
    { os :: Os
    } deriving (Eq, Show)

instance Default CompilerState where
    def = CompilerState { os = Other }

newtype Compiler a = Compiler
    { runCompiler :: StateT CompilerState Maybe a
    }

class Compile a where
    compile :: a -> Compiler [String]

tryCompiler :: Compiler a -> CompilerState -> Maybe (a, CompilerState)
tryCompiler (Compiler c) = runStateT c

executeCompiler :: Compiler a -> CompilerState -> Maybe a
executeCompiler c s = do
    a <- tryCompiler c s
    return $ fst a
