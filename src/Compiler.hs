module Compiler ( Compiler(Compiler)
                , Compile(compile)
                , Os(Darwin, Other)
                , os
                , stackIndex
                , getState
                , setState
                , getStackFrame
                , setStackFrame
                , clearDeclared
                , popDeclared
                , resetIndex
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

newtype StackFrame = StackFrame [(String, Int)]
    deriving (Eq, Show)

data CompilerState = CompilerState
    { os :: Os
    , n :: Int
    , stackFrame :: StackFrame
    , stackIndex :: Int
    , declared :: [String]
    } deriving (Eq, Show)

instance Default CompilerState where
    def = CompilerState { os = Other
                        , n = 0
                        , stackFrame = StackFrame []
                        , stackIndex = 0
                        , declared = []
                        }

type CompilerStateT a = StateT CompilerState Maybe a

getState :: CompilerStateT CompilerState
getState = get

setState :: CompilerState -> CompilerStateT ()
setState = put

resetIndex :: CompilerStateT ()
resetIndex = do
    state <- getState
    setState $ state { stackIndex = 0 }

getOs :: CompilerStateT Os
getOs = os <$> getState

getSymbols :: [String] -> CompilerStateT [String]
getSymbols prefixes = do
    state <- getState
    let i = n state
    setState $ state { n = i + 1}
    return $ (++ show i) <$> prefixes

getOffset :: String -> CompilerStateT (Maybe Int)
getOffset key = do
    state <- getState
    let StackFrame sf = stackFrame state
    return $ lookup key sf

getStackFrame :: CompilerStateT (StackFrame, Int, [String])
getStackFrame = do
    state <- getState
    let sf = stackFrame state
        si = stackIndex state
        d  = declared state
    return (sf, si, d)

setStackFrame :: (StackFrame, Int, [String]) -> CompilerStateT ()
setStackFrame (sf, si, d) = do
    state <- getState
    setState $ state { stackFrame = sf
                     , stackIndex = si
                     , declared = d
                     }

clearDeclared :: CompilerStateT ()
clearDeclared = do
    state <- getState
    setState $ state { declared = [] }

popDeclared :: CompilerStateT [String]
popDeclared = do
    state <- getState
    let count = length $ declared state
    return $ take count $ repeat "\tpop\t%rax"

isDeclared :: String -> CompilerStateT Bool
isDeclared key = do
    state <- getState
    return $ key `elem` declared state

pushFrame :: String -> Int -> CompilerStateT ()
pushFrame key size = do
    state <- getState
    let StackFrame sf = stackFrame state
        index = stackIndex state - size
        declaredNames = declared state
    setState $ state { stackFrame = StackFrame ((key, index) : sf)
                     , stackIndex = index
                     , declared = key : declaredNames
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
