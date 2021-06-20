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
                , setLoop
                , getContinueTarget
                , getBreakTarget
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
    , bytes :: Int
    , continueTarget :: Maybe String
    , breakTarget :: Maybe String
    } deriving (Eq, Show)

instance Default CompilerState where
    def = CompilerState { os = Other
                        , n = 0
                        , stackFrame = StackFrame []
                        , stackIndex = 0
                        , declared = []
                        , bytes = 0
                        , continueTarget = Nothing
                        , breakTarget = Nothing
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

getStackFrame :: CompilerStateT (StackFrame, Int, [String], Int)
getStackFrame = do
    state <- getState
    let sf = stackFrame state
        si = stackIndex state
        d  = declared state
        b  = bytes state
    return (sf, si, d, b)

setStackFrame :: (StackFrame, Int, [String], Int) -> CompilerStateT ()
setStackFrame (sf, si, d, b) = do
    state <- getState
    setState $ state { stackFrame = sf
                     , stackIndex = si
                     , declared   = d
                     , bytes      = b
                     }

clearDeclared :: CompilerStateT ()
clearDeclared = do
    state <- getState
    setState $ state { declared = [], bytes = 0 }

popDeclared :: CompilerStateT [String]
popDeclared = do
    state <- getState
    let bytesUsed = show $ bytes state
    return [ "\taddq\t$" ++ bytesUsed ++ ", %rsp" ]

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
        bytesUsed = bytes state
    setState $ state { stackFrame = StackFrame ((key, index) : sf)
                     , stackIndex = index
                     , declared = key : declaredNames
                     , bytes = bytesUsed + size
                     }

setLoop :: (Maybe String, Maybe String) -> CompilerStateT (Maybe String, Maybe String)
setLoop (continueTarget', breakTarget') = do
    state <- getState
    setState $ state { continueTarget = continueTarget', breakTarget = breakTarget' }
    return (continueTarget state, breakTarget state)

getContinueTarget :: CompilerStateT (Maybe String)
getContinueTarget = do
    state <- getState
    return $ continueTarget state

getBreakTarget :: CompilerStateT (Maybe String)
getBreakTarget = do
    state <- getState
    return $ breakTarget state

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
