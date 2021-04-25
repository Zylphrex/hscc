module Main where

import Control.Monad ( when )
import Data.Default ( def )
import Data.Maybe ( isNothing )
import System.Environment ( getArgs )
import System.Exit ( ExitCode(ExitFailure), exitWith )
import System.FilePath ( extSeparator, takeBaseName )
import System.IO ( readFile )
import qualified System.Info

import Ast.Program ( Program )
import Compiler ( compile
                , executeCompiler
                , Os(Darwin, Other)
                , os
                )
import Parser ( executeParser, parse )
import Pretty ( prettyPrint, render )

osOption :: Os
osOption = case System.Info.os of
                "darwin" -> Darwin
                _        -> Other

main :: IO ()
main = do
    args     <- getArgs
    when (length args /= 1) $ exitWith $ ExitFailure 1
    let inputFile  = head args
        outputFile = takeBaseName inputFile ++ [extSeparator, 's']
    content <- readFile inputFile
    let result = toAssembly content osOption
    when (isNothing result) $ exitWith $ ExitFailure 2
    let (Just (program, assembly)) = result
    putStrLn $ render program
    writeFile outputFile assembly

toAssembly :: String -> Os -> Maybe (Program, String)
toAssembly input osOption = do
    program <- executeParser parse input
    assembly <- executeCompiler (compile program) option
    return (program, unlines assembly)
  where option = def { os = osOption}
