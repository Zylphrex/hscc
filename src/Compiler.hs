module Compiler ( Compiler(Compiler)
                , Compile(compile)
                , FunctionSignature(FunctionSignature)
                , Os(Darwin, Other)
                , os
                , stackIndex
                , getState
                , setState
                , setFunctionArgs
                , resetFunctionArgs
                , setStackFrame
                , getStackFrame
                , resetStackFrame
                , clearDeclared
                , popDeclared
                , getVariable
                , getOs
                , getSymbols
                , isDeclared
                , pushDeclared
                , pushArgument
                , pushFunctionDeclaration
                , pushFunctionPrototype
                , getFunctionSignature
                , runCompiler
                , executeCompiler
                , setLoop
                , getContinueTarget
                , getBreakTarget
                , argumentRegister
                , saveFunctionArgs
                , restoreFunctionArgs
                ) where

import Control.Applicative ( Alternative((<|>), empty) )
import Control.Monad.State ( StateT(StateT)
                           , evalStateT
                           , runStateT
                           , get
                           , put
                           )
import Data.Default ( Default(def) )
import Data.List ( find, sortOn )
import Data.Maybe ( isJust )

data Os = Darwin | Other deriving (Eq, Show)

newtype StackFrame = StackFrame [(String, Int)]
    deriving (Eq, Show)

data FunctionSignature = FunctionSignature String String [(String, String)]
    deriving (Eq, Show)

data CompilerState = CompilerState
    { os :: Os
    , n :: Int
    , functionArgs :: [(String, Int)]
    , stackFrame :: StackFrame
    , stackIndex :: Int
    , argIndex :: Int
    , declared :: [String]
    , bytes :: Int
    , continueTarget :: Maybe String
    , breakTarget :: Maybe String
    , functionDeclarations :: [FunctionSignature]
    , functionPrototypes :: [FunctionSignature]
    } deriving (Eq, Show)

instance Default CompilerState where
    def = CompilerState { os = Other
                        , n = 0
                        , functionArgs = []
                        , stackFrame = StackFrame []
                        , stackIndex = 0
                        , argIndex = 8
                        , declared = []
                        , bytes = 0
                        , continueTarget = Nothing
                        , breakTarget = Nothing
                        , functionDeclarations = []
                        , functionPrototypes = []
                        }

type CompilerStateT a = StateT CompilerState Maybe a

getState :: CompilerStateT CompilerState
getState = get

setState :: CompilerState -> CompilerStateT ()
setState = put

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

getFunctionArg :: String -> CompilerStateT (Maybe Int)
getFunctionArg key = do
    state <- getState
    let args = functionArgs state
    return $ lookup key args

setFunctionArgs :: [(String, Int)] -> CompilerStateT ()
setFunctionArgs args = do
    state <- getState
    setState $ state { functionArgs = args }

resetFunctionArgs :: CompilerStateT [(String, Int)]
resetFunctionArgs = do
    state <- getState
    let args = functionArgs state
    setState $ state { functionArgs = [] }
    return args

saveFunctionArgs :: CompilerStateT [String]
saveFunctionArgs = do
    state <- getState
    let args = take count $ sortOn snd $ functionArgs state
        n = length args - 1
    return $ map save [0..n]
  where count = length argumentRegisters
        save index = "\tpush\t" ++ argumentRegister index

restoreFunctionArgs :: CompilerStateT [String]
restoreFunctionArgs = do
    state <- getState
    let args = take count $ sortOn snd $ functionArgs state
        n = length args - 1
    return $ map restore $ reverse [0..n]
  where count = length argumentRegisters
        restore index = "\tpop\t" ++ argumentRegister index

getVariable :: String -> CompilerStateT (Maybe String)
getVariable key = do
    stackOffset <- getOffset key
    case stackOffset of
        Just offset -> return $ Just $ show offset ++ "(%rbp)"
        Nothing -> do
            arg <- getFunctionArg key
            case arg of
                Just index -> return $ Just $ argumentRegister index
                Nothing -> return Nothing

setStackFrame :: (StackFrame, Int, [String], Int) -> CompilerStateT ()
setStackFrame (sf, si, d, b) = do
    state <- getState
    setState $ state { stackFrame = sf
                     , stackIndex = si
                     , declared   = d
                     , bytes      = b
                     }

getStackFrame :: CompilerStateT (StackFrame, Int, [String], Int)
getStackFrame = do
    state <- getState
    let sf = stackFrame state
        si = stackIndex state
        d  = declared state
        b  = bytes state
    return (sf, si, d, b)

resetStackFrame :: CompilerStateT (StackFrame, Int, [String], Int)
resetStackFrame = do
    stackFrame <- getStackFrame
    state <- getState
    setState $ state { stackFrame = StackFrame []
                     , stackIndex = 0
                     , argIndex   = 8
                     , declared   = []
                     , bytes      = 0
                     }
    return stackFrame

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

pushDeclared :: String -> Int -> CompilerStateT ()
pushDeclared key size = do
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

argumentRegisters :: [String]
argumentRegisters = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"]

argumentRegister :: Int -> String
argumentRegister index = argumentRegisters !! index

pushArgument :: Int -> String -> Int -> CompilerStateT ()
pushArgument index key size = do
    state <- getState
    if index >= 6
    then let StackFrame sf = stackFrame state
             index = argIndex state + size
             declaredNames = declared state
         in setState $ state { stackFrame = StackFrame ((key, index) : sf)
                             , argIndex = index
                             , declared = key : declaredNames
                             }
    else let args = functionArgs state
         in setState $ state { functionArgs = (key, index):args }

pushFunctionPrototype :: String -> String -> [(String, String)] -> CompilerStateT ()
pushFunctionPrototype returnType identifier arguments = do
    state <- getState
    let signature = FunctionSignature returnType identifier arguments
        prototypes = functionPrototypes state
    setState $ state { functionPrototypes = signature:prototypes }

pushFunctionDeclaration :: String -> String -> [(String, String)] -> CompilerStateT ()
pushFunctionDeclaration returnType identifier arguments = do
    state <- getState
    let signature = FunctionSignature returnType identifier arguments
        declarations = functionDeclarations state
    setState $ state { functionDeclarations = signature:declarations }

getFunctionSignature :: String -> CompilerStateT (Maybe FunctionSignature)
getFunctionSignature identifier = do
    state <- getState
    let prototypes   = functionPrototypes state
        declarations = functionDeclarations state
        pred (FunctionSignature _ identifier' _) = identifier == identifier'
    return $ find pred prototypes <|> find pred declarations

setLoop :: (Maybe String, Maybe String) -> CompilerStateT (Maybe String, Maybe String)
setLoop (continueTarget', breakTarget') = do
    state <- getState
    setState $ state { continueTarget = continueTarget', breakTarget = breakTarget' }
    return (continueTarget state, breakTarget state)

getContinueTarget :: CompilerStateT (Maybe String)
getContinueTarget = continueTarget <$> getState

getBreakTarget :: CompilerStateT (Maybe String)
getBreakTarget = breakTarget <$> getState

newtype Compiler a = Compiler
    { runCompiler :: CompilerStateT a
    }

instance Functor Compiler where
    f `fmap` (Compiler c) = Compiler $ f <$> c

instance Applicative Compiler where
    pure x = Compiler $ pure x
    (Compiler ac) <*> (Compiler c) = Compiler $ ac <*> c

instance Monad Compiler where
    (Compiler c) >>= mc = Compiler $ StateT d
        where d state = do
                (a, state) <- runStateT c state
                runStateT (runCompiler (mc a)) state

class Compile a where
    compile :: a -> Compiler [String]

executeCompiler :: Compiler a -> CompilerState -> Maybe a
executeCompiler (Compiler c) = evalStateT c
