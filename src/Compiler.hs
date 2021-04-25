module Compiler where

import Control.Applicative ( Alternative((<|>), empty) )
import Control.Monad.State ( StateT(StateT)
                           , evalStateT
                           , mapStateT
                           , runStateT
                           )
import Data.Default

data Os = Darwin | Other deriving (Eq, Show)

data CompilerState = CompilerState
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
tryCompiler (Compiler c) s = runStateT c s

executeCompiler :: Compiler a -> CompilerState -> Maybe a
executeCompiler c s = do
    a <- tryCompiler c s
    return $ fst a
