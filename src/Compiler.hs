module Compiler ( Compiler(Compiler)
                , Compile(compile)
                , Os(Darwin, Other)
                , os
                , stackIndex
                , getOffset
                , getOs
                , getSymbols
                , isDeclared
                , pushFrame
                , runCompiler
                , executeCompiler
                ) where

import Control.Applicative ( Alternative((<|>), empty) )
import Control.Monad.State ( StateT(StateT)
                           , evalStateT
                           , runStateT
                           , get
                           , put
                           )
import Data.Default ( Default(def) )
import Data.Maybe ( isJust )

data Os = Darwin | Other deriving (Eq, Show)

type CompilerStateT a = StateT CompilerState Maybe a

newtype StackFrame = StackFrame [(String, Int)]
    deriving (Eq, Show)

getOffset :: String -> CompilerStateT (Maybe Int)
getOffset key = do
    state <- get
    let StackFrame sf = stackFrame state
    return $ lookup key sf

isDeclared :: String -> CompilerStateT Bool
isDeclared key = do
    mOffset <- getOffset key
    return $ isJust mOffset

pushFrame :: String -> Int -> CompilerStateT ()
pushFrame key size = do
    state <- get
    let StackFrame sf = stackFrame state
        index = stackIndex state - size
    put $ state { stackFrame = StackFrame ((key, index) : sf)
                , stackIndex = index
                }

getOs :: CompilerStateT Os
getOs = do
    state <- get
    return $ os state

getSymbols :: [String] -> CompilerStateT [String]
getSymbols prefixes = do
    state <- get
    let i = n state
    put $ state { n = i + 1}
    return $ (++ show i) <$> prefixes

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
    { runCompiler :: CompilerStateT a
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
