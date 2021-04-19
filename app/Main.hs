module Main where

import Control.Monad ( when )
import Data.Maybe ( isNothing )
import System.Environment ( getArgs )
import System.Exit ( ExitCode(ExitFailure), exitWith )
import System.FilePath ( extSeparator, takeBaseName )
import System.IO ( readFile )
import System.Info ( os )
import Text.PrettyPrint ( render )

import Ast.Program ( Program )
import Assembly ( Option(Option)
                , OsOption(Darwin, Other)
                , osOption
                , toAssembly
                )
import Parser ( executeParser, Parser(runParser), parse )
import Pretty ( prettyPrint )

main :: IO ()
main = do
    args     <- getArgs
    when (length args /= 1) $ exitWith $ ExitFailure 1
    let inputFile = head args
    content <- readFile inputFile
    let result = executeParser parse content
    when (isNothing result) $ exitWith $ ExitFailure 2
    let (Just program) = result
    let outputFile = takeBaseName inputFile ++ [extSeparator, 's']
        assembly   = compile program
    putStrLn $ format program
    writeFile outputFile assembly

compile :: Program -> String
compile = toAssembly option
    where osOption = if os == "darwin" then Darwin else Other
          option   = Option { osOption = osOption }

format :: Program -> String
format = render . prettyPrint
