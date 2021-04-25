module Compiler ( Compiler(Compiler)
                , Compile(compile)
                , Os(Darwin, Other)
                , os
                , n
                , runCompiler
                , tryCompiler
                , executeCompiler
                ) where

import Control.Applicative ( Alternative((<|>), empty) )
import Control.Monad.State ( StateT(StateT)
                           , evalStateT
                           , runStateT
                           )
import Data.Default

data Os = Darwin | Other deriving (Eq, Show)

data CompilerState = CompilerState
    { os :: Os
    , n :: Int
    } deriving (Eq, Show)

instance Default CompilerState where
    def = CompilerState { os = Other, n = 0 }

newtype Compiler a = Compiler
    { runCompiler :: StateT CompilerState Maybe a
    }

class Compile a where
    compile :: a -> Compiler [String]

tryCompiler :: Compiler a -> CompilerState -> Maybe (a, CompilerState)
tryCompiler (Compiler c) = runStateT c

executeCompiler :: Compiler a -> CompilerState -> Maybe a
executeCompiler (Compiler c) = evalStateT c
