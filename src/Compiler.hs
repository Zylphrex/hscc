module Compiler ( Compiler(Compiler)
                , Compile(compile)
                , Os(Darwin, Other)
                , os
                , n
                , runCompiler
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

instance Functor Compiler where
    f `fmap` (Compiler c) = Compiler $ f <$> c

instance Applicative Compiler where
    pure x = Compiler $ pure x
    (Compiler ac) <*> (Compiler c) = Compiler $ ac <*> c

class Compile a where
    compile :: a -> Compiler [String]

executeCompiler :: Compiler a -> CompilerState -> Maybe a
executeCompiler (Compiler c) = evalStateT c
