module Compiler ( Compiler(Compiler)
                , Compile(compile)
                , Os(Darwin, Other)
                , os
                , n
                , stackFrame
                , stackIndex
                , getOffset
                , isDeclared
                , pushFrame
                , runCompiler
                , executeCompiler
                ) where

import Control.Applicative ( Alternative((<|>), empty) )
import Control.Monad.State ( StateT(StateT)
                           , evalStateT
                           , runStateT
                           )
import Data.Default ( Default(def) )
import Data.Maybe ( isJust )

data Os = Darwin | Other deriving (Eq, Show)

newtype StackFrame = StackFrame [(String, Int)]
    deriving (Eq, Show)

getOffset :: String -> StackFrame -> Maybe Int
getOffset key (StackFrame stackFrame) = lookup key stackFrame

isDeclared :: String -> StackFrame -> Bool
isDeclared key = isJust . getOffset key

pushFrame :: (String, Int) -> StackFrame -> StackFrame
pushFrame frame (StackFrame stackFrame) = StackFrame (frame : stackFrame)

data CompilerState = CompilerState
    { os :: Os
    , n :: Int
    , stackFrame :: StackFrame
    , stackIndex :: Int
    } deriving (Eq, Show)

instance Default CompilerState where
    def = CompilerState { os = Other
                        , n = 0
                        , stackFrame = StackFrame []
                        , stackIndex = 0
                        }

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
